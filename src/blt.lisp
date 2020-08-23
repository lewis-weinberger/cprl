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
    (#xe0 :close)
    (#x52 :up)
    (#x51 :down)
    (#x50 :left)
    (#x4f :right)
    (#x28 :enter)
    (#x14 :q)
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
	 (when (eq w width)
	   (setf w x)
	   (incf h))
	 (terminal-put w h (char-code c))
	 (incf w))))
