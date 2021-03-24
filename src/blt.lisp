;;;; BearLibTerminal FFI

(in-package :blt)


;;; Raw FFI -------------------------------------------------------------------


(cffi:define-foreign-library bearlibterminal
  (:darwin "libBearLibTerminal.dylib")
  (:unix "libBearLibTerminal.so")
  (t (:default "BearLibTerminal")))

(cffi:use-foreign-library bearlibterminal)

(cffi:defcfun "terminal_open" :int)

(cffi:defcfun "terminal_close" :void)

(cffi:defcfun "terminal_refresh" :void)

(cffi:defcfun "terminal_clear" :void)

(cffi:defcfun "terminal_read" :int)

(cffi:defcfun "terminal_put" :void
  (x :int)
  (y :int)
  (code :int))

(cffi:defcfun "terminal_color" :void
  (colour :uint))

(cffi:defcfun "terminal_bkcolor" :void
  (colour :uint))


;;; UI class and methods ------------------------------------------------------


(defclass blt-ui () ())

(defmethod start ((ui blt-ui))
  "Open a terminal screen."
  (declare (ignore ui))
  (terminal-open))

(defmethod stop ((ui blt-ui))
  "Close the terminal screen."
  (declare (ignore ui))
  (terminal-close))

(defmethod input ((ui blt-ui))
  "Read user input, blocking until something can be read."
  (declare (ignore ui))
  (case (terminal-read)
    (#x04 :a)
    (#x05 :b)
    (#x06 :c)
    (#x07 :d)
    (#x08 :e)
    (#x09 :f)
    (#x0a :g)
    (#x0b :h)
    (#x0c :i)
    (#x0d :j)
    (#x0e :k)
    (#x0f :l)
    (#x10 :m)
    (#x11 :n)
    (#x12 :o)
    (#x13 :p)
    (#x14 :q)
    (#x15 :r)
    (#x16 :s)
    (#x17 :t)
    (#x18 :u)
    (#x19 :v)
    (#x1a :w)
    (#x1b :x)
    (#x1c :y)
    (#x1d :z)
    (#x1e :1)
    (#x1f :2)
    (#x20 :3)
    (#x21 :4)
    (#x22 :5)
    (#x23 :6)
    (#x24 :7)
    (#x25 :8)
    (#x26 :9)
    (#x27 :0)
    (#x28 :enter)
    (#x36 :comma)
    (#x37 :period)
    (#x38 :slash)
    (#x4f :right)
    (#x50 :left)
    (#x51 :down)
    (#x52 :up)
    (#xe0 :close)
    (#x59 :num1)
    (#x5A :num2)
    (#x5B :num3)
    (#x5C :num4)
    (#x5D :num5)
    (#x5E :num6)
    (#x5F :num7)
    (#x60 :num8)
    (#x61 :num9)
    (#x62 :num0)
    (#x63 :numperiod)
    (otherwise nil)))

(defmethod output ((ui blt-ui) x y c)
  "Put a tile with given code C at grid coordinates (X,Y)"
  (declare (ignore ui))
  (terminal-put x y c))

(defmethod flush ((ui blt-ui))
  "Flush output to the terminal screen."
  (declare (ignore ui))
  (terminal-refresh))

(defmethod clear ((ui blt-ui))
  "Clear the terminal screen."
  (declare (ignore ui))
  (terminal-clear))

(defmethod set-colour ((ui blt-ui) &optional fg bg)
  "Set colours for future printing to foreground FG and background BG."
  (declare (ignore ui))
  (when fg (terminal-color fg))
  (when bg (terminal-bkcolor bg)))
