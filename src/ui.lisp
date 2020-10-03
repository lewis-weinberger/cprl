;;;; User interface

(in-package :ui)

(defmacro with-screen (&body body)
  `(sb-int:with-float-traps-masked
       (:inexact :underflow :overflow :invalid :divide-by-zero)
     (blt:start)
     (blt:refresh)
     (unwind-protect
	  (progn ,@body)
       (blt:stop))))

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

(defmacro selection-loop (options draw)
  (let ((sel (gensym)))
    `(let ((,sel (first ,options)))
       (funcall ,draw ,sel)
       (loop do
	    (case (blt:input)
	      (:q (return))
	      (:close (return))
	      (:up (setf ,sel (prev-key ,sel ,options)))
	      (:down (setf ,sel (next-key ,sel ,options)))
	      (:enter (if (eq ,sel :quit)
			  (return)
			  (funcall (getf ,options ,sel)))))
	    (funcall ,draw ,sel)))))

(defun draw-selection (sel options)
  (loop for p in options by #'cddr
     for props = (getf options p)
     for text = (getf props :text)
     for x = (getf props :x)
     for y = (getf props :y)
     if (eq p sel) do
       (blt:display text x y :fg *sel-fg* :bg *sel-bg*)
     else do
       (blt:display text x y)))

(defun draw-start-screen (sel)
  (blt:clear)
  (blt:display-centred " ██████\\  ███████\\  ███████\\  ██\\       
██  __██\\ ██  __██\\ ██  __██\\ ██ |      
██ /  \\__|██ |  ██ |██ |  ██ |██ |      
██ |      ███████  |███████  |██ |      
██ |      ██  ____/ ██  __██< ██ |      
██ |  ██\\ ██ |      ██ |  ██ |██ |      
\\██████  |██ |      ██ |  ██ |████████\\ 
 \\______/ \\__|      \\__|  \\__|\\________|"
		       1 :fg *cyberspace-fg*)
  (blt:display-centred "A Cyberpunk Roguelike Game" 10)
  (blt:display (format nil
		       "Version ~A.~A.~A"
		       *major-version*
		       *minor-version*
		       *patch-version*) 1 22)
  (draw-selection sel (list
		       :start (list :text "Start game" :x 35 :y 13)
		       :tutorial (list :text "Tutorial" :x 35 :y 14)
		       :quit (list :text "Quit" :x 35 :y 15)))
  (blt:refresh))

(defun start-screen ()
  "Display the opening screen of the game, with the option to start or quit."
  (with-screen
    (selection-loop (list :start 'home-screen :tutorial 'tutorial-screen :quit)
		    'draw-start-screen)))

(defun draw-home-screen (sel)
  (blt:clear)
  (blt:display-centred "H.O.M.E. - Helpful Organisational Monitor Enhancement" 1)
  (blt:box "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 1 2 36 11)
  (blt:display "input" 2 2 :fg *label-fg*)
  (blt:box "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 1 13 36 9)
  (blt:display "info" 2 13 :fg *label-fg*)
  (blt:box "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 37 2 42 20)
  (blt:display "description" 38 2 :fg *label-fg*)
  (draw-selection sel (list
		       :job
		       (list :text "Start next job" :x 3 :y 4)
		       :hotel
		       (list :text "Visit the Blue Cafe Hotel" :x 3 :y 6)
		       :bazaar
		       (list :text "Visit the Bazaar" :x 3 :y 8)
		       :adchq
		       (list :text "Infiltrate Akuma-Druden Corp. HQ" :x 3 :y 10)))
  (blt:display-centred "[Q] Quit   [UP/DOWN/ENTER] Navigate   [M] Menu" 22)
  (blt:display (game:describe-job *player*) 39 4 :width 38)
  (blt:refresh))

(defun home-screen ()
  "Display the home screen of the game."
  (selection-loop (list :job 'job-screen
			:hotel 'hotel-screen
			:bazaar 'bazaar-screen
			:adchq 'adchq-screen)
		  'draw-home-screen))

(defun move-player (location dx dy)
  "Move player only when allowed."
  (let ((x (+ (game:x *player*) dx))
	(y (+ (game:y *player*) dy)))
    (when (game:valid-move location x y)
      (setf (game:x *player*) x))
    (when (game:valid-move location x y)
      (setf (game:y *player*) y))))

(defmacro player-loop (location draw)
  (let ((quitp (gensym)))
    `(let ((,quitp nil))
       (funcall ,draw ,location)
       (loop :while (not ,quitp) :do
	    (case (blt:input)
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
	    (funcall ,draw ,location)))))

(defun draw-bazaar-screen (bazaar)
  (blt:clear)
  (blt:box "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 1 1 49 21)
  (blt:display "bazaar" 2 1 :fg *label-fg*)
  (blt:box "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 50 1 29 11)
  (blt:display "description" 51 1 :fg *label-fg*)
  (blt:box "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 50 12 29 10)
  (blt:display "info" 51 12 :fg *label-fg*)
  (let* ((start-x 3)
	 (start-y 3)
	 (mid-x 25)
	 (mid-y 11)
	 (width (- mid-x start-x))
	 (height (- mid-y start-y)))
    (multiple-value-bind (layout x y)
	(game:sub-location bazaar
			   (game:x *player*)
			   (game:y *player*)
			   width
			   height
			   mid-x
			   mid-y)
      (blt:display layout x y :fg *bazaar-fg*))
    (blt:display "@" mid-x mid-y))
  (blt:display-centred "[Q] Quit   [UP/DOWN/LEFT/RIGHT] Move   [L] Look   [M] Menu" 22)
  (blt:refresh))

(defun bazaar-screen ()
  "Display bazaar screen."
  (setf (game:x *player*) *player-bazaar-x*)
  (setf (game:y *player*) *player-bazaar-y*)
  (player-loop *bazaar* 'draw-bazaar-screen))

(defun draw-job-screen ())

(defun job-screen ())

(defun draw-hotel-screen ())

(defun hotel-screen ())

(defun draw-adchq-screen ())

(defun adchq-screen ())

(defun draw-tutorial-screen ())

(defun tutorial-screen ())

