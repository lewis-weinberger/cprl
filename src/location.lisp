;;;; Location functionality

(in-package :cprl)

(defstruct location
  (layout (make-array '(25 25)) :type array) ;; Array containing map layout
  (entities '() :type list))                 ;; List containing NPCs tied to location

(defun sub-array (arr i1 i2 j1 j2)
  (let* ((height (1+ (- i2 i1)))
         (width (1+ (- j2 j1)))
         (new-arr (make-array `(,height ,width))))
    (loop
      :with sub-i = 0
      :for i :from i1 :to i2 :do
        (loop
          :with sub-j = 0
          :for j :from j1 :to j2 :do
            (setf (aref new-arr sub-i sub-j) (aref arr i j))
            (incf sub-j))
        (incf sub-i))
    new-arr))

(defun valid-move (loc x y)
  (and (> x 0)
       (> y 0)
       (eq (aref (location-layout loc) y x) :empty)))

(defun text-to-layout (string width height)
  (let ((layout (make-array `(,height ,width) :initial-element :empty)))
    (loop
      :with i = 0
      :with j = 0
      :for c :across string :do
        (when (= j width)
          (setf j 0))
        (when (= i height)
          (setf i 0))
        ; incorrect string dimensions will create weird output
        (case c
          (#\  (setf (aref layout i j) :empty))
          (#\# (setf (aref layout i j) :wall))
          (#\Newline (incf i) (decf j))
          (otherwise (setf (aref layout i j) :empty)))
        (incf j))
    layout))

(defun layout-to-text (layout)
  (let* ((width (array-dimension layout 1))
         (height (array-dimension layout 0))
         (string (make-string (* height (1+ width)))))
    (if (or (= width 0) (= height 0))
        ""
        (progn
          (loop
            :with c = 0
            :for i :from 0 :to (1- height) :do
              (loop
                :for j :from 0 :to (1- width) :do
                  (setf (aref string c) (case (aref layout i j)
                                          (:empty #\ )
                                          (:wall #\Medium_shade)
                                          (otherwise #\ )))
                  (incf c))
              (setf (aref string c) #\Newline)
              (incf c))
          string))))

(defun print-layout (loc)
  (format t (layout-to-text (location-layout loc))))

(defun find-corner (loc px py dx dy)
  (let* ((layout (location-layout loc))
         (width (array-dimension layout 1))
         (height (array-dimension layout 0))
         (pdx- (- px dx))
         (pdy- (- py dy))
         (pdx+ (+ px dx))
         (pdy+ (+ py dy))
         (x1 (if (>= pdx- 0) pdx- 0))
         (y1 (if (>= pdy- 0) pdy- 0))
         (x2 (if (< pdx+ width) pdx+ (1- width)))
         (y2 (if (< pdy+ height) pdy+ (1- height))))
    (values y1 y2 x1 x2)))

(defun sub-location (loc px py dx dy sx sy)
  (multiple-value-bind (i1 i2 j1 j2) (find-corner loc px py dx dy)
    (values
     (layout-to-text (sub-array (location-layout loc) i1 i2 j1 j2))
     (- sx (- px j1))
     (- sy (- py i1)))))
