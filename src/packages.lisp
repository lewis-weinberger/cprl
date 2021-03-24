(in-package :cl-user)

(defpackage :macros
  (:use :common-lisp)
  (:export :if-let))

(defpackage :config
  (:use :common-lisp)
  (:export :*major-version*
           :*minor-version*
           :*patch-version*
           :*normal-fg*
           :*normal-bg*
           :*sel-fg*
           :*sel-bg*
           :*label-fg*
           :*bazaar-fg*
           :*cyberspace-fg*))

(defpackage :blt
  (:use :common-lisp)
  (:export :blt-ui
           :start
           :stop
           :input
           :output
           :flush
           :clear
           :set-colour))

(defpackage :cprl
  (:use :common-lisp :macros :config :blt)
  (:export :main))
