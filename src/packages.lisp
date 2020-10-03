(in-package :cl-user)

(defpackage :macros
  (:use :common-lisp)
  (:export :if-let))

(defpackage :config
  (:use :common-lisp)
  (:export :*major-version*
	   :*minor-version*
	   :*patch-version*
	   :*player*
	   :*bazaar*
	   :*player-bazaar-x*
	   :*player-bazaar-y*
	   :*normal-fg*
	   :*normal-bg*
	   :*sel-fg*
	   :*sel-bg*
	   :*label-fg*
	   :*bazaar-fg*
	   :*cyberspace-fg*))

(defpackage :blt
  (:use :common-lisp :cffi :macros :config)
  (:export :start
	   :stop
	   :refresh
	   :clear
	   :input
	   :colour
	   :set-colour
	   :get-colour
	   :with-colour
	   :display
	   :display-centred
	   :box))

(defpackage :game
  (:use :common-lisp :macros :config)
  (:export :init-player
	   :describe-job
	   :x
	   :y
	   :text-to-layout
	   :init-bazaar
	   :print-layout
	   :sub-location
	   :valid-move))

(defpackage :ui
  (:use :common-lisp :macros :config)
  (:export :start-screen))

(defpackage :cprl
  (:use :common-lisp :config)
  (:export :main))
