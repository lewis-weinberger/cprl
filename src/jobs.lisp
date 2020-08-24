;;;; Jobs functionality

(in-package :game)

(defclass job ()
  ((description
    :initarg :description
    :initform ""
    :accessor description)
   (location
    :initarg :location
    :accessor location)))
