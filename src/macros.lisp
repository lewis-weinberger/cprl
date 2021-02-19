;;;; Useful macros

(in-package :macros)

(defmacro if-let (vars then else)
  "If VARS are not nil, evalute THEN clause, otherwise evalute ELSE clause."
  (let ((variables (mapcar #'first vars)))
    `(let ,vars
       (if (and ,@variables)
           ,then
           ,else))))
