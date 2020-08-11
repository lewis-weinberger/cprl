;;;; User interface

(in-package :ui)


;;; Generic user interface ----------------------------------------------------


(defgeneric start (interface)
  (:documentation "Open the user-interface."))

(defgeneric stop (interface)
  (:documentation "Close the user-interface."))

(defgeneric display (interface string x y &key width height)
  (:documentation "Print STRING starting at coordinate (X, Y).
Optional WIDTH and HEIGHT specify text-wrapping boundaries."))

(defgeneric flush (interface)
  (:documentation "Flush all printed characters to the user-interface."))

(defgeneric clear (interface)
  (:documentation "Clear the user-interface."))

(defgeneric box (interface c x y width height)
  (:documentation "Draw a box with character C starting from
coordinate (X, Y) out to WIDTH and HEIGHT."))

(defgeneric input (interface)
  (:documentation "Return latest user input, 
or NIL if no input since last call."))

(defgeneric pane-display (interface string pane x y pad)
  (:documentation "Print STRING to particular PANE,
at relative coordinate (X, Y), wrapped within the pane's width and height."))

(defgeneric pane-clear (interface pane)
  (:documentation "Clear user-interface within PANE."))

(defgeneric pane-box (interface c pane)
  (:documentation "Draw a box with character C around border of PANE."))


;;; BearLibTerminal implementation --------------------------------------------


(defvar *default-width* 80)
(defvar *default-height* 24)

(defclass user-interface ()
  ((dimensions
    :initarg :dimensions
    :initform (list *default-width* *default-height*)
    :accessor dimensions
    :documentation "Terminal dimensions in characters.")
   (panes
    :initarg :panes
    :initform (list (list 0 0 *default-width* *default-height*))
    :accessor panes
    :documentation "Position(s) and dimensions of UI pane(s).")))

(defmethod start ((interface user-interface))
  (blt:terminal-open))
;  (with-slots ((d dimensions) panes) interface
;    (blt:terminal-set
;     (format nil "window: size=~Ax~A;" (elt d 0) (elt d 1)))))

(defmethod stop ((interface user-interface))
  (blt:terminal-close))

(defmethod display ((interface user-interface) string x y &key width height)
  (loop
     with w = x
     with h = y
     for c across string do
       (when (eq w width)
	 (setf w x)
	 (incf h))
       (when (eq h height)
	 (setf h y))
       (blt:terminal-put w h (char-code c))
       (incf w)))

(defmethod flush ((interface user-interface))
  (blt:terminal-refresh))

(defmethod clear ((interface user-interface))
  (blt:terminal-clear))

(defmethod box ((interface user-interface) c x y width height)
  (when (stringp c)
    (setf c (char c 0)))
  (loop
     with xmax = (+ x (1- width))
     with ymax = (+ y (1- height))
     for i from x to xmax do
       (loop 
	  for j from y to ymax do
	    (when (or (= j y) (= j ymax) (= i x) (= i xmax))
	      (blt:terminal-put i j (char-code c))))))

(defmethod input ((interface user-interface))
  (if (not (= (blt:terminal-peek) 0))
      (blt:terminal-read)
      0))

(defmethod pane-clear ((interface user-interface) pane)
  (with-slots (dimensions (p panes)) interface
    (let ((pp (elt p pane)))
      (destructuring-bind (px py pw ph) pp
	(blt:terminal-clear-area px py pw ph)))))

(defmethod pane-display ((interface user-interface) string pane x y pad)
  (with-slots (dimensions (p panes)) interface
    (let ((pp (elt p pane)))
      (destructuring-bind (px py pw ph) pp
	(loop
	   with w = (+ x px pad)
	   with h = (+ y py pad)
	   for c across string do
	     (when (eq (+ w pad) (+ px pw))
	       (setf w (+ px pad))
	       (incf h))
	     (when (eq (+ h pad) (+ py ph))
	       (setf h (+ py pad)))
	     (blt:terminal-put w h (char-code c))
	     (incf w))))))

(defmethod pane-box ((interface user-interface) c pane)
  (with-slots (dimensions (p panes)) interface
    (let ((pp (elt p pane)))
      (destructuring-bind (px py pw ph) pp
	(box interface c px py pw ph)))))

(defmacro with-user-interface (name dimensions panes &rest body)
  "Open a user-interface, bound to NAME, with DIMENSIONS and PANES, 
then perform actions in BODY"
  `(sb-int:with-float-traps-masked
       (:inexact :underflow :overflow :invalid :divide-by-zero)
     (let ((,name (make-instance 'user-interface
				 :dimensions ,dimensions
				 :panes ,panes)))
       (start ,name)
       (flush ,name)
       (unwind-protect
	    (progn ,@body)
	 (stop ,name)))))
