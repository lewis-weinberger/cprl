;;;; A minimal wrapper around some of BearLibTerminal.

(in-package :blt)

(define-foreign-library bearlibterminal
  (:darwin "libBearLibTerminal.dylib")
  (:unix "libBearLibTerminal.so")
  (t (:default "BearLibTerminal")))

(use-foreign-library bearlibterminal)

(defcfun "terminal_open" :int)      ; int  terminal_open()
(defcfun "terminal_close" :void)    ; void terminal_close()
(defcfun "terminal_refresh" :void)  ; void terminal_refresh()
(defcfun "terminal_clear" :void)    ; void terminal_clear()
(defcfun "terminal_has_input" :int) ; int  terminal_has_input();
(defcfun "terminal_read" :int)      ; int  terminal_read()
(defcfun "terminal_put" :void       ; void terminal_put()
  (x :int)
  (y :int)
  (code :int))

(defmacro with-open-terminal (&body body)
  "Perform actions on a terminal instance"
  `(sb-int:with-float-traps-masked
     (:inexact :underflow :overflow :invalid :divide-by-zero)
     (terminal-open)
     (unwind-protect
	  (progn ,@body)
       (terminal-close))))

(defun clear ()
  "Clear the terminal"
  (terminal-clear))

(defun refresh ()
  "Flush the output buffer to the terminal"
  (terminal-refresh))

(defun put (x y char)
  "Print CHAR to the output buffer at position X, Y"
  (terminal-put x y (char-code char)))

(defun read-key ()
  "Read (blocking) an event from the input queue"
  (terminal-read))
