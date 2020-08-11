;;;; BearLibTerminal FFI

(in-package :blt)

(define-foreign-library bearlibterminal
  (:darwin "libBearLibTerminal.dylib")
  (:unix "libBearLibTerminal.so")
  (t (:default "BearLibTerminal")))

(use-foreign-library bearlibterminal)

(defcfun "terminal_open" :int)

(defcfun "terminal_close" :void)

(defcfun "terminal_refresh" :void)

(defcfun "terminal_clear" :void)

(defcfun "terminal_clear_area" :void
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun "terminal_has_input" :int)

(defcfun "terminal_read" :int)

(defcfun "terminal_peek" :int)

(defcfun "terminal_set" :int
  (s :string))

(defcfun "terminal_put" :void
  (x :int)
  (y :int)
  (code :int))
