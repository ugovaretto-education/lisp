(proclaim '(optimize (speed 3) (safety 0) (space 0) (debug 0)))
(deftype array-index ()
  `(integer 0 (,array-dimension-limit)))

(defvar *matrix-multipliers* (make-hash-table :test #'equal))

(defmacro define-matrix-multiplier (&optional for-type)
  `(progn
     (setf (gethash ',for-type *matrix-multipliers*)
           (lambda (a b c ar ac bc zero-c)
             ,@(if for-type
                   `((declare (type (simple-array ,for-type 2) a b c)
                              (optimize speed)))
                   '())
             (declare (type array-index ar ac bc))
             (when zero-c
               (let ((s (array-total-size c)))
                 (declare (type array-index s))
                 (dotimes (i s)
                   (setf (row-major-aref c i) ,(if for-type (coerce 0 for-type) 0)))))
             (dotimes (i ar c)
               (declare (type array-index i))
               (dotimes (j ac)
                 (declare (type array-index j))
                 (dotimes (k bc)
                   (declare (type array-index k))
                   (incf (aref c i j) (* (aref a i k) (aref b k j))))))))
     ',for-type))

(define-matrix-multiplier)
(define-matrix-multiplier single-float)
(define-matrix-multiplier double-float)

(defun matrix-multiplier-for (type)
  (or (gethash type *matrix-multipliers*)
      (gethash nil *matrix-multipliers*)))

(defun matrix-multiply (a b &optional (c nil))
  (declare (type (array * 2) a b)
           (type (or (array * 2) null) c))
  (let ((ar (array-dimension a 0))
        (ac (array-dimension a 1))
        (br (array-dimension b 0))
        (bc (array-dimension b 1))
        (at (array-element-type a))
        (bt (array-element-type b)))
    (declare (type array-index ar ac bc))
    (unless (= ac br)
      (error "dimension"))
    (if (and (typep a 'simple-array)
             (typep b 'simple-array)
             (typep c '(or null simple-array))
             (eq at bt)
             (or (not c) (eq (array-element-type c) at)))
        (if c
            (funcall (matrix-multiplier-for at)
                     a b c
                     ar ac bc
                     t)
            (funcall (matrix-multiplier-for at)
                     a b (make-array (list bc ar)
                                     :element-type at
                                     :initial-element (coerce 0 at))
                     ar ac bc
                     nil))
        (if c
            (funcall (matrix-multiplier-for nil)
                     a b c
                     ar ac bc
                     t)
            (funcall (matrix-multiplier-for nil)
                     a b (make-array (list bc ar) :initial-element 0)
                     ar ac bc
                     nil)))))
