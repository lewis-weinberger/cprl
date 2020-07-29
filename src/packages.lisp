(in-package :cl-user)

(defpackage :blt
  (:use :common-lisp :cffi)
  (:export :with-open-terminal
           :terminal-open
	   :terminal-close
	   :refresh
	   :clear
	   :put
	   :read-key))

(defpackage :cprl
  (:use :common-lisp)
  (:export :main))
