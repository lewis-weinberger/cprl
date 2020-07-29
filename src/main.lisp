;;;; Entry-point and main game loop

(in-package :cprl)

(defvar *player-x* 1)
(defvar *player-y* 1)

(defun draw-player ()
  "Draw player position on terminal"
  (blt:clear)
  (blt:put *player-x* *player-y* #\@)
  (blt:refresh))

(defun main ()
  "Main game loop"
  (blt:with-open-terminal
    (draw-player)
    (let ((runp t))
      (loop while runp do
	   (let ((key (blt:read-key)))
	     (cond ((= key #x14) (setf runp nil))     ; Q
		   ((= key #xe0) (setf runp nil))     ; CLOSE
		   ((= key #x52) (decf *player-y*))   ; UP
		   ((= key #x51) (incf *player-y*))   ; DOWN
		   ((= key #x50) (decf *player-x*))   ; LEFT
		   ((= key #x4f) (incf *player-x*)))) ; RIGHT
	   (draw-player))))
  (sb-ext:exit))
