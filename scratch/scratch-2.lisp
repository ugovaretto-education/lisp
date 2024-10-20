(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(defun mat-mul (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array double-float 2) a b)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (assert (and (= (array-rank a) (array-rank b) 2)
               (= (array-dimension a 1) (array-dimension b 0)))
          (a b)
          "Cannot multiply ~S by ~S." a b)
  (let* ((m (first (array-dimensions a)))
         (n (second (array-dimensions b)))
         (p (second (array-dimensions a)))
         (result (make-array `(,m ,n) :element-type (array-element-type a)
                                      :adjustable nil :fill-pointer nil)))
    (declare (fixnum m n p))
    (dotimes (i m)
      (declare (fixnum i))
      (dotimes (j n)
        (declare (fixnum j))
        (dotimes (k p)
          (declare (fixnum k))
          (incf (aref result i j) (* (aref a i k) (aref b k j))))))
    result))

(defun mat-mul-kij (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array double-float 2) a b)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (assert (and (= (array-rank a) (array-rank b) 2)
               (= (array-dimension a 1) (array-dimension b 0)))
          (a b)
          "Cannot multiply ~S by ~S." a b)
  (let* ((m (first (array-dimensions a)))
         (n (second (array-dimensions b)))
         (p (second (array-dimensions a)))
         (result (make-array `(,m ,n) :element-type (array-element-type a)
                                      :adjustable nil :fill-pointer nil)))
    (declare (fixnum m n p))
    (dotimes (k p)
      (declare (fixnum k))
      (dotimes (i m)
        (declare (fixnum i))
        (dotimes (j n)
          (declare (fixnum j))
          (incf (aref result i j) (* (aref a i k) (aref b k j))))))
    result))


(defun make-matrix (rows columns type init-value)
  (make-array (list rows columns) :element-type type
                         :initial-element init-value :adjustable nil :fill-pointer nil))

(defvar *A* (make-matrix 3000 3000 'double-float 0.0043d0))
(defvar *B* (make-matrix 3000 3000 'double-float 0.1d0))

(defun benchmark ()
  (let  (( _ (time (mat-mul *A* *B*)))) (declare (ignore _))))

(defun benchmark-kij ()
  (let  (( _ (time (mat-mul-kij *A* *B*)))) (declare (ignore _))))