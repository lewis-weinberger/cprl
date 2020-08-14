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

(defun draw-start-screen (sel)
  (blt:clear)
  (blt:display "CPRL" 35 10)
  (if sel
      (progn
	(blt:display "Start game" 35 13 :fg *sel-fg* :bg *sel-bg*)
	(blt:display "Quit" 35 14))
      (progn
	(blt:display "Start game" 35 13)
	(blt:display "Quit" 35 14 :fg *sel-fg* :bg *sel-bg*)))
  (blt:refresh))

(defun start-screen ()
  "Display the opening screen of the game, with the option to start or quit."
  (with-screen
      (let ((quitp nil)
	    (sel t)) ; "Start game" is selected first
	(draw-start-screen sel)
	(loop while (not quitp) do
	     (let ((key (blt:input)))
	       (case key
		 (:q (setf quitp t))
		 (:close (setf quitp t))
		 (:up (setf sel (not sel)))
		 (:down (setf sel (not sel)))
		 (:enter (if sel
			     (game-screen)
			     (setf quitp t))))
	       (draw-start-screen sel))))))

(defvar player-x 1)
(defvar player-y 1)

(defun draw-game-screen ()
  (blt:clear)
  (blt:display "@" player-x player-y)
  (blt:display "Press q to quit!" 1 22)
  (blt:refresh))

(defun game-screen ()
  "Display main game screen."
  (let ((quitp nil))
    (draw-game-screen)
    (loop while (not quitp) do
	 (let ((key (blt:input)))
	   (case key
	     (:q (setf quitp t))
	     (:close (setf quitp t))
	     (:up (decf player-y))
	     (:down (incf player-y))
	     (:right (incf player-x))
	     (:left (decf player-x))))
	 (draw-game-screen))))

(defun menu-screen ())
