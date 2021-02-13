;;;; Entry-point and main game loop

(in-package :cprl)

(defun init ()
  (setf *player* (game:init-player "3Jane"))
  (setf *bazaar* (game:init-bazaar)))

(defun main ()
  "Entry point for game. Performs initialisation and displays the start menu."
  (init)
  (ui:start-screen)
  (uiop:quit))
