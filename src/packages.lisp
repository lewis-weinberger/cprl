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
  (:use :common-lisp :macros :config)
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

(defpackage :cprl
  (:use :common-lisp :macros :config)
  (:export :main))
