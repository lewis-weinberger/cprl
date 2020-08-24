;;;; Entity functionality

(in-package :game)


;;; Base attributes object ----------------------------------------------------


(eval-when (:compile-toplevel)
  (defun make-attr (attr)
    (let ((name (car attr))
	  (doc (cdr attr)))
      `(,name
	:initarg ,(intern (string name) :keyword)
	:initform 0
	:accessor ,name
	:documentation ,doc))))

(defmacro defattr (attrs)
  `(defclass attributes ()
     ,(mapcar #'make-attr attrs)))

(defattr
    ((constitution . "Constitution determines total hit-points.")
     (brawn . "Brawn determines armour carrying capacity.")
     (strength . "Strength determines melee weapon ability.")
     (dexterity . "Dexterity determines ranged weapon ability.")
     (tenacity . "Tenacity determines contact-based psychic ability")
     (potentiality . "Potentiality determines ranged telekinetic ability")
     (adaptability . "Adaptability determines cybernetic modification capacity")
     (attunement . "Attunement determines modification effectiveness")
     (penetration . "Penetration determines destructive hacking ability")
     (security . "Security determines defensive anti-hacking ability")))


;;; Base entity object --------------------------------------------------------


(defclass entity (attributes)
  ((hit-points
    :initarg :hit-points
    :initform 1
    :accessor hit-points
    :documentation "Total health")
   (x
    :initarg :x
    :accessor x
    :documentation "x-coordinate")
   (y
    :initarg :y
    :accessor y
    :documentation "y-coordinate")))
