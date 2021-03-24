;;;; User interface

(in-package :cprl)

(defgeneric start (ui)
  (:documentation "Initialise and open a UI."))

(defgeneric stop (ui)
  (:documentation "Clean up and close a UI."))

(defgeneric input (ui)
  (:documentation "Read user input from the UI, blocking until something is read."))

(defgeneric output (ui x y c)
  (:documentation "Draw the representation of code-point C at grid coordinates (X,Y) on the UI"))

(defgeneric flush (ui)
  (:documentation "Flush output buffer to the UI."))

(defgeneric clear (ui)
  (:documentation "Clear the UI to a blank state."))

(defgeneric set-colour (ui &optional fg bg)
  (:documentation "Set the colour for future draws to have foreground FG and background BG."))


;;; ---------------------------------------------------------------------------


(defmacro with-colour ((ui fg bg) &body body)
  "Perform actions in BODY with colour set by FG and BG."
  `(unwind-protect
        (progn
          (set-colour ,ui ,fg ,bg)
          ,@body)
     (set-colour ,ui ,*normal-fg* ,*normal-bg*)))

(defun display (ui string x y &key width (fg *normal-fg*) (bg *normal-bg*))
  "Display STRING on the screen at coordinate (X, Y).
If WIDTH is provided, wrap text onto next line when it reaches WIDTH.
If FG or BG are provided, set the text colour."
  (with-colour (ui fg bg)
    (loop
       with w = x
       with h = y
       for c across string do
         (when (or (eq (- w x) width) (eq c #\Newline))
           (setf w x)
           (incf h))
         (unless (eq c #\Newline)
           (output ui w h (char-code c))
           (incf w)))))

(defun line-length (string)
  (loop for c across string
     if (char= c #\Newline)
     return total
     else
     counting c into total
     finally (return total)))

(defun display-centred (ui string y &key width (fg *normal-fg*) (bg *normal-bg*))
  "Display STRING aligned horizontally in the centre of the screen."
  (let ((x (round (/ (- 80 (line-length string)) 2))))
    (display ui string x y :width width :fg fg :bg bg)))

(defmacro ensure-char (s)
  `(when (stringp ,s)
     (setf ,s (char ,s 0))))

(defun box (ui ls rs ts bs tl bl tr br x y width height &key (fg *normal-fg*) (bg *normal-bg*))
  "Display a box on the screen with top left corner coordinate (X, Y),
out to WIDTH and HEIGHT. If FG or BG are provided, set the border colour."
  (ensure-char ls)
  (ensure-char rs)
  (ensure-char ts)
  (ensure-char bs)
  (ensure-char tl)
  (ensure-char bl)
  (ensure-char tr)
  (ensure-char br)
  (with-colour (ui fg bg)
    (loop
       with xmax = (+ x (1- width))
       with ymax = (+ y (1- height))
       for i from x to xmax do
         (loop
           for j from y to ymax do
             (cond
               ((and (= j y) (= i x)) (output ui i j (char-code tl)))
               ((and (= j ymax) (= i xmax)) (output ui i j (char-code br)))
               ((and (= j y) (= i xmax)) (output ui i j (char-code tr)))
               ((and (= j ymax) (= i x)) (output ui i j (char-code bl)))
               ((= j y) (output ui i j (char-code ts)))
               ((= j ymax) (output ui i j (char-code bs)))
               ((= i x) (output ui i j (char-code ls)))
               ((= i xmax) (output ui i j (char-code rs))))))))

(defmacro with-screen (ui &body body)
  `(float-features:with-float-traps-masked T
     (start ,ui)
     (flush ,ui)
     (unwind-protect
          (progn ,@body)
       (stop ,ui))))


;;; ---------------------------------------------------------------------------


(defun next-key (sel plist)
  (let ((i (position sel plist)))
    (if (= i (- (length plist) 2))
        (first plist)
        (nth (+ i 2) plist))))

(defun prev-key (sel plist)
  (let ((i (position sel plist)))
    (if (= i 0)
        (nth (- (length plist) 2) plist)
        (nth (- i 2) plist))))

(defmacro selection-loop (ui options draw)
  (let ((sel (gensym)))
    `(let ((,sel (first ,options)))
       (funcall ,draw ,ui ,sel)
       (loop do
         (case (input ,ui)
           (:q (return))
           (:close (return))
           (:up (setf ,sel (prev-key ,sel ,options)))
           (:down (setf ,sel (next-key ,sel ,options)))
           (:enter (if (eq ,sel :quit)
                       (return)
                       (funcall (getf ,options ,sel) ,ui))))
         (funcall ,draw ,ui ,sel)))))

(defun draw-selection (ui sel options)
  (loop for p in options by #'cddr
     for props = (getf options p)
     for text = (getf props :text)
     for x = (getf props :x)
     for y = (getf props :y)
     if (eq p sel) do
       (display ui text x y :fg *sel-fg* :bg *sel-bg*)
     else do
       (display ui text x y)))

(defun draw-start-screen (ui sel)
  (clear ui)
  (display-centred ui " ██████\\  ███████\\  ███████\\  ██\\
██  __██\\ ██  __██\\ ██  __██\\ ██ |      
██ /  \\__|██ |  ██ |██ |  ██ |██ |      
██ |      ███████  |███████  |██ |      
██ |      ██  ____/ ██  __██< ██ |      
██ |  ██\\ ██ |      ██ |  ██ |██ |      
\\██████  |██ |      ██ |  ██ |████████\\ 
 \\______/ \\__|      \\__|  \\__|\\________|"
                       1 :fg *cyberspace-fg*)
  (display-centred ui "A Cyberpunk Roguelike Game" 10)
  (display ui (format nil
                      "Version ~A.~A.~A"
                      *major-version*
                      *minor-version*
                      *patch-version*) 1 22)
  (draw-selection ui sel (list
                          :start (list :text "Start game" :x 35 :y 13)
                          :tutorial (list :text "Tutorial" :x 35 :y 14)
                          :quit (list :text "Quit" :x 35 :y 15)))
  (flush ui))

(defun start-screen (ui)
  "Display the opening screen of the game, with the option to start or quit."
  (with-screen ui
    (selection-loop ui
                    (list :start 'home-screen :tutorial 'tutorial-screen :quit nil)
                    'draw-start-screen)))

(defun draw-home-screen (ui sel)
  (clear ui)
  (display-centred ui "H.O.M.E. - Helpful Organisational Monitor Enhancement" 1)
  (box ui "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 1 2 36 11)
  (display ui "input" 2 2 :fg *label-fg*)
  (box ui "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 1 13 36 9)
  (display ui "info" 2 13 :fg *label-fg*)
  (box ui "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 37 2 42 20)
  (display ui "description" 38 2 :fg *label-fg*)
  (draw-selection ui
                  sel
                  (list :job
                        (list :text "Start next job" :x 3 :y 4)
                        :hotel
                        (list :text "Visit the Blue Cafe Hotel" :x 3 :y 6)
                        :bazaar
                        (list :text "Visit the Bazaar" :x 3 :y 8)
                        :adchq
                        (list :text "Infiltrate Akuma-Druden Corp. HQ" :x 3 :y 10)))
  (display-centred ui "[Q] Quit   [UP/DOWN/ENTER] Navigate   [M] Menu" 22)
  (display ui (describe-job *player*) 39 4 :width 38)
  (flush ui))

(defun home-screen (ui)
  "Display the home screen of the game."
  (selection-loop ui
                  (list :job
                        'job-screen
                        :hotel
                        'hotel-screen
                        :bazaar
                        'bazaar-screen
                        :adchq
                        'adchq-screen)
                  'draw-home-screen))

(defmacro player-loop (ui location draw)
  (let ((quitp (gensym)))
    `(let ((,quitp nil))
       (funcall ,draw ,ui ,location)
       (loop :while (not ,quitp) :do
         (case (input ,ui)
           (:q (setf ,quitp t))
           (:close (setf ,quitp t))
           (:up (move-player ,location 0 -1))
           (:num8 (move-player ,location 0 -1))
           (:down (move-player ,location 0 1))
           (:num2 (move-player ,location 0 1))
           (:right (move-player ,location 1 0))
           (:num6 (move-player ,location 1 0))
           (:left (move-player ,location -1 0))
           (:num4 (move-player ,location -1 0))
           (:num7 (move-player ,location -1 -1))
           (:num9 (move-player ,location 1 -1))
           (:num1 (move-player ,location -1 1))
           (:num3 (move-player ,location 1 1)))
         (move-entities ,location)
         (funcall ,draw ,ui ,location)))))

(defun draw-bazaar-screen (ui bazaar)
  (clear ui)
  (box ui "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 1 1 49 21)
  (display ui "bazaar" 2 1 :fg *label-fg*)
  (box ui "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 50 1 29 11)
  (display ui "description" 51 1 :fg *label-fg*)
  (box ui "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 50 12 29 10)
  (display ui "info" 51 12 :fg *label-fg*)
  (let* ((start-x 3)
         (start-y 3)
         (mid-x 25)
         (mid-y 11)
         (width (- mid-x start-x))
         (height (- mid-y start-y)))
    (multiple-value-bind (layout x y)
        (sub-location bazaar
                      (entity-x *player*)
                      (entity-y *player*)
                      width
                      height
                      mid-x
                      mid-y)
      (display ui layout x y :fg *bazaar-fg*))
    (display ui "@" mid-x mid-y))
  (display-centred ui "[Q] Quit   [UP/DOWN/LEFT/RIGHT] Move   [L] Look   [M] Menu" 22)
  (flush ui))

(defun bazaar-screen (ui)
  "Display bazaar screen."
  (setf (entity-x *player*) *player-bazaar-x*)
  (setf (entity-y *player*) *player-bazaar-y*)
  (player-loop ui *bazaar* 'draw-bazaar-screen)
  (setf *player-bazaar-x* (entity-x *player*))
  (setf *player-bazaar-y* (entity-y *player*)))

(defun draw-job-screen ())

(defun job-screen (ui))

(defun draw-hotel-screen ())

(defun hotel-screen (ui))

(defun draw-adchq-screen ())

(defun adchq-screen (ui))

(defun draw-tutorial-screen ())

(defun tutorial-screen (ui))
