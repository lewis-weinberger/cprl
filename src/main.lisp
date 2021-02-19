;;;; Entry-point and main game loop

(in-package :cprl)

(defun init ()
  (setf *player* (make-player :name "3Jane"))
  (init-bazaar))

(defun main ()
  "Entry point for game. Performs initialisation and displays the start menu."
  (init)
  (start-screen)
  (uiop:quit))
