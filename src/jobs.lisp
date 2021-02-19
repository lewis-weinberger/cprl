;;;; Jobs functionality

(in-package :cprl)

(defstruct job
  (description "a job" :type string)
  (location (make-location) :type location)
  (start (lambda (j)) :type function))
