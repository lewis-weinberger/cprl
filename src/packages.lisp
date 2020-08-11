(in-package :cl-user)

(defpackage :blt
  (:use :common-lisp :cffi)
  (:export :terminal-open
	   :terminal-close
	   :terminal-refresh
	   :terminal-clear
	   :terminal-clear-area
	   :terminal-put
	   :terminal-read
	   :terminal-has-input
	   :terminal-peek
	   :terminal-set))

(defpackage :ui
  (:use :common-lisp)
  (:export :with-user-interface
	   :start
	   :stop
	   :display
	   :flush
	   :clear
	   :box
	   :input
	   :pane-display
	   :pane-clear
	   :pane-box))

(defpackage :cprl
  (:use :common-lisp)
  (:export :main))
