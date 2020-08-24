;;;; Player functionality

(in-package :game)

(defclass player (entity)
  ((name
    :initarg :name
    :initform "No name"
    :accessor name
    :documentation "Name of player character")
   (level
    :initarg :level
    :initform 0
    :accessor level
    :documentation "Level determines total attribute points.")
   (experience
    :initarg :experience
    :initform 0
    :accessor experience
    :documentation "Experience determines level.")
   (jobs
    :initarg :jobs
    :initform ()
    :accessor jobs
    :documentation "List of (remaining) jobs taken on at the Bazaar.")
   (items
    :initarg :items
    :initform ()
    :accessor items
    :documentation "List of items acquired doing jobs.")
   (liquidity
    :initarg :liquidity
    :initform 0
    :accessor liquidity
    :documentation "Money acquired doing jobs.")))

(defun init-player (name)
  (make-instance 'player :name name))

(defun describe-job (player)
  (if-let ((job (first (jobs player))))
    (description job)
    "Currently you do not have any jobs. Visit the Bazaar!"))
