;;;; Entry-point and main game loop

(in-package :cprl)

(defun main ()
  "Entry point for game. Performs initialisation and displays the start menu."
  (ui:start-screen)
  (sb-ext:exit))
