(in-package :cl-user)

(defpackage :config
  (:use :common-lisp)
  (:export :*normal-fg*
	   :*normal-bg*
	   :*sel-fg*
	   :*sel-bg*))

(defpackage :blt
  (:use :common-lisp :cffi :config)
  (:export :start
	   :stop
	   :refresh
	   :clear
	   :input
	   :colour
	   :set-colour
	   :get-colour
	   :with-colour
	   :display))

(defpackage :ui
  (:use :common-lisp :config)
  (:export :start-screen))

(defpackage :cprl
  (:use :common-lisp)
  (:export :main))
