;;;; Location functionality

(in-package :cprl)

(defstruct location
  (layout (make-array '(5 5) :initial-element :empty) :type array) ;; Array containing map layout
  (path (make-array '(5 5)) :type array)   ;; Array containing Dijkstra map for pathing
  (free (make-array '(5 5)) :type array)   ;; Array containing whether a cell is empty
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

(defun find-free (loc)
  "Find all empty cells in the location"
  (with-slots (layout free entities) loc
    (dotimes (n (array-total-size free))
      (let ((cell (row-major-aref layout n)))
        (setf (row-major-aref free n)
              (case cell
                (:empty T)
                (otherwise nil)))))
    (dolist (entity entities)
      (setf (aref free (entity-y entity) (entity-x entity)) nil))))

(defun find-path (loc x y)
  "Construct the Dijkstra map for the location with player at (x, y)"
  (with-slots (layout path) loc
    (let ((width (array-dimension layout 1))
          (height (array-dimension layout 0)))
      (dotimes (i height)
        (dotimes (j width)
          (let ((dx (abs (- x j)))
                (dy (abs (- y i))))
            (setf (aref path i j) (min dx dy))))))))

(defun valid-move (loc x y)
  (and (> x 0)
       (> y 0)
       (aref (location-free loc) y x)))

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
         (height (array-dimension layout 0)))
    (with-output-to-string (s nil)
      (dotimes (i height)
        (dotimes (j width)
          (write-char (case (aref layout i j)
                        (:empty #\ )
                        (:wall #\Medium_shade)
                        (:trader #\T)
                        (otherwise #\ )) s))
        (write-char #\Newline s)))))

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

(defun paint-entities (loc)
  (with-slots (layout entities) loc
    (let ((painted (make-array (array-dimensions layout))))
      ;; First we make a copy of the layout
      (dotimes (n (array-total-size layout))
        (let ((cell (row-major-aref layout n)))
          (setf (row-major-aref painted n) cell)))
      ;; Then we paint entities on the copy and return it
      (dolist (entity entities)
        (with-slots (x y kind) entity
          (setf (aref painted y x) kind)))
      painted)))

(defun sub-location (loc px py dx dy sx sy)
  (multiple-value-bind (i1 i2 j1 j2) (find-corner loc px py dx dy)
    (values
     (layout-to-text (sub-array (paint-entities loc) i1 i2 j1 j2))
     (- sx (- px j1))
     (- sy (- py i1)))))

(defun move-entities (loc)) ;; TODO
