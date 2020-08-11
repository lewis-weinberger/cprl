;;;; Entry-point and main game loop

(in-package :cprl)

(defvar *player-x* 3)
(defvar *player-y* 5)
(defvar *screen-width* 80)
(defvar *screen-height* 24)
(defvar *main-pane* (list 0 0 55 20))
(defvar *info-pane* (list 55 0 25 20))
(defvar *log-pane* (list 0 20 80 4))

(defun draw-game (screen)
  ;; Main pane
  (ui:pane-clear screen 0)
  (ui:pane-box screen #\. 0)
  (ui:pane-display screen "@" 0 *player-x* *player-y* 0)

  ;; Info pane
  (ui:pane-clear screen 1)
  (ui:pane-box screen #\# 0)
  (ui:pane-display screen "Useful player stats can go here in the future!" 1 0 0 1)

  (ui:flush screen))

(defun main ()
  "Main game loop"
  (ui:with-user-interface screen
    (list *screen-width* *screen-height*)
    (list *main-pane* *info-pane* *log-pane*)
    (draw-game screen)
    (let ((runp t))
      (loop while runp do
	   (let ((key (ui:input screen)))
	     (cond ((= key #x14) (setf runp nil))     ; Q
		   ((= key #xe0) (setf runp nil))     ; CLOSE
		   ((= key #x52) (decf *player-y*))   ; UP
		   ((= key #x51) (incf *player-y*))   ; DOWN
		   ((= key #x50) (decf *player-x*))   ; LEFT
		   ((= key #x4f) (incf *player-x*)))) ; RIGHT
	   (draw-game screen))))
  (sb-ext:exit))
