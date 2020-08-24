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
		       1)
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
    (selection-loop (list :start 'home-screen :tutorial 'dungeon-screen :quit)
		    'draw-start-screen)))

(defun draw-home-screen (sel)
  (blt:clear)
  (blt:display-centred "H.O.M.E. - Helpful Organisational Monitor Enhancement" 1)
  (blt:box "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 1 2 36 11)
  (blt:display "input" 2 2)
  (blt:box "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 1 13 36 9)
  (blt:display "info" 2 13)
  (blt:box "┃" "┃" "━" "━" "┏" "┗" "┓" "┛" 37 2 42 20)
  (blt:display "description" 38 2)
  (draw-selection sel (list
		       :quest
		       (list :text "Start next quest" :x 3 :y 4)
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
  (selection-loop (list :quest 'dungeon-screen
			:hotel 'dungeon-screen
			:bazaar 'dungeon-screen
			:adchq 'dungeon-screen)
		  'draw-home-screen))

(defvar player-x 1)
(defvar player-y 1)

(defun draw-dungeon-screen ()
  (blt:clear)
  (blt:display "@" player-x player-y)
  (blt:display "Press q to quit!" 1 22)
  (blt:refresh))

(defun dungeon-screen ()
  "Display dungeon screen, used for exploring locations."
  (let ((quitp nil))
    (draw-dungeon-screen)
    (loop while (not quitp) do
	 (let ((key (blt:input)))
	   (case key
	     (:q (setf quitp t))
	     (:close (setf quitp t))
	     (:up (decf player-y))
	     (:down (incf player-y))
	     (:right (incf player-x))
	     (:left (decf player-x))))
	 (draw-dungeon-screen))))

(defun menu-screen ()
  "Display menu screen.")


