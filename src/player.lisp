;;;; Player functionality

(in-package :cprl)

(defstruct (player (:include entity))
  (name "player" :type string) ;; Name of player character
  (level 0 :type integer)      ;; Level determines total attribute points
  (exp 0.0 :type float)        ;; Experience determines level
  (jobs '() :type list)        ;; List of unfinished jobs taken on at the Bazaar
  (items '() :type list)       ;; List of items acquired doing jobs
  (cash 0.0 :type float))      ;; Money acquired doing jobs

(defun describe-job (p)
  (if-let ((job (first (player-jobs p))))
    (description job)
    "Currently you do not have any jobs.
Visit the Bazaar!"))

(defun start-job (p)
  (if-let ((job (first (player-jobs p))))
    (start job)
    nil))
