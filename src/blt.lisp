;;;; BearLibTerminal FFI

(in-package :blt)


;;; Raw FFI -------------------------------------------------------------------


(define-foreign-library bearlibterminal
  (:darwin "libBearLibTerminal.dylib")
  (:unix "libBearLibTerminal.so")
  (t (:default "BearLibTerminal")))

(use-foreign-library bearlibterminal)

(defcfun "terminal_open" :int)

(defcfun "terminal_close" :void)

(defcfun "terminal_refresh" :void)

(defcfun "terminal_clear" :void)

(defcfun "terminal_clear_area" :void
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun "terminal_has_input" :int)

(defcfun "terminal_read" :int)

(defcfun "terminal_peek" :int)

(defcfun "terminal_set" :int
  (s :string))

(defcfun "terminal_put" :void
  (x :int)
  (y :int)
  (code :int))

(defcfun "terminal_color" :void
  (colour :uint))

(defcfun "terminal_bkcolor" :void
  (colour :uint))

(defcfun "terminal_state" :int
  (slot :int))


;;; Wrappers ------------------------------------------------------------------


(defun start ()
  "Open a terminal screen."
  (terminal-open))

(defun stop ()
  "Close the terminal screen."
  (terminal-close))

(defun refresh ()
  "Flush output to the terminal screen."
  (terminal-refresh))

(defun clear ()
  "Clear the terminal screen."
  (terminal-clear))

(defun input ()
  "Read user input (blocking)."
  (case (terminal-read)
    (#x04 :a)
    (#x05 :b)
    (#x06 :c)
    (#x07 :d)
    (#x08 :e)
    (#x09 :f)
    (#x0a :g)
    (#x0b :h)
    (#x0c :i)
    (#x0d :j)
    (#x0e :k)
    (#x0f :l)
    (#x10 :m)
    (#x11 :n)
    (#x12 :o)
    (#x13 :p)
    (#x14 :q)
    (#x15 :r)
    (#x16 :s)
    (#x17 :t)
    (#x18 :u)
    (#x19 :v)
    (#x1a :w)
    (#x1b :x)
    (#x1c :y)
    (#x1d :z)
    (#x1e :1)
    (#x1f :2)
    (#x20 :3)
    (#x21 :4)
    (#x22 :5)
    (#x23 :6)
    (#x24 :7)
    (#x25 :8)
    (#x26 :9)
    (#x27 :0)
    (#x28 :enter)
    (#x36 :comma)
    (#x37 :period)
    (#x38 :slash)
    (#x4f :right)
    (#x50 :left)
    (#x51 :down)
    (#x52 :up)
    (#xe0 :close)
    (otherwise nil)))

(defun colour (a r g b)
  "#xAARRGGBB colour format recognised by BearLibTerminal."
  (let ((c 0))
    (setf c (dpb a (byte 8 24) c))
    (setf c (dpb r (byte 8 16) c))
    (setf c (dpb g (byte 8 8) c))
    (setf c (dpb b (byte 8 0) c))
    c))

(defun set-colour (&optional fg bg)
  "Set colours for future printing to foreground FG and background BG."
  (when fg (terminal-color fg))
  (when bg (terminal-bkcolor bg)))

(defun get-colour ()
  "Get current foreground and background colours."
  (values (terminal-state #xC4)
	  (terminal-state #xC5)))

(defmacro with-colour ((fg bg) &body body)
  "Perform actions in BODY with colour set by FG and BG."    
  `(unwind-protect
	(progn
	  (set-colour ,fg ,bg)
	  ,@body)
     (set-colour ,*normal-fg* ,*normal-bg*)))

(defun display (string x y &key width (fg *normal-fg*) (bg *normal-bg*))
  "Display STRING on the screen at coordinate (X, Y).
If WIDTH is provided, wrap text onto next line when it reaches WIDTH.
If FG or BG are provided, set the text colour."
  (with-colour (fg bg)
    (loop
       with w = x
       with h = y
       for c across string do
	 (when (or (eq (- w x) width) (eq c #\Newline))
	   (setf w x)
	   (incf h))
	 (unless (eq c #\Newline)
	   (terminal-put w h (char-code c))
	   (incf w)))))

(defun line-length (string)
  (loop for c across string
     if (char= c #\Newline)
     return total
     else
     counting c into total
     finally (return total)))

(defun display-centred (string y &key width (fg *normal-fg*) (bg *normal-bg*))
  "Display STRING aligned horizontally in the centre of the screen."
  (let ((x (round (/ (- 80 (line-length string)) 2))))
    (display string x y :width width :fg fg :bg bg)))

(defmacro ensure-char (s)
  `(when (stringp ,s)
     (setf ,s (char ,s 0))))

(defun box (ls rs ts bs tl bl tr br x y width height &key (fg *normal-fg*) (bg *normal-bg*))
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
  (with-colour (fg bg)
    (loop
       with xmax = (+ x (1- width))
       with ymax = (+ y (1- height))
       for i from x to xmax do
	 (loop
	    for j from y to ymax do
	      (cond
		((and (= j y) (= i x)) (terminal-put i j (char-code tl)))
		((and (= j ymax) (= i xmax)) (terminal-put i j (char-code br)))
		((and (= j y) (= i xmax)) (terminal-put i j (char-code tr)))
		((and (= j ymax) (= i x)) (terminal-put i j (char-code bl)))
		((= j y) (terminal-put i j (char-code ts)))
		((= j ymax) (terminal-put i j (char-code bs)))
		((= i x) (terminal-put i j (char-code ls)))
		((= i xmax) (terminal-put i j (char-code rs))))))))
