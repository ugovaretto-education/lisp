(defun matrix-multiply (matrix1 matrix2)
  "Multiply two matrices and return the result."
  (let* ((rows1 (length matrix1))
         (cols1 (length (first matrix1)))
         (rows2 (length matrix2))
         (cols2 (length (first matrix2))))
    (if (not (eq cols1 rows2))
        (error "Matrix dimensions are not compatible for multiplication")
        (let ((result (make-array (list rows1 cols2) :initial-element 0)))
          (dotimes (i rows1 result)
            (dotimes (j cols2)
              (dotimes (k cols1)
                (incf (aref result i j)
                      (* (nth k (nth i  matrix1))
                         (nth j (nth k  matrix2))))))
            )))))

(defun matrix-multiply-array (matrix1 matrix2)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  "Multiply two matrices and return the result."
  (let  ((rows1 (array-dimension matrix1 0))
         (cols1 (array-dimension matrix1 1))
         (rows2 (array-dimension matrix2 0))
         (cols2 (array-dimension matrix2 1)))
         (if (not (= cols1 rows2))
             (error "Matrix dimensions are not compatible for multiplication")
             (let ((result (make-array (list rows1 cols2) :initial-element 0)))
               (dotimes (i rows1 result)
                 (dotimes (j cols2)
                   (dotimes (k cols1)
                     (incf (aref result i j)
                           (* (aref matrix1 i k)
                              (aref matrix2 k j))))))))))

(defun double-matrix-multiply-array (matrix1 matrix2)
  (declare (type (array double-float 2)  matrix1 )
           (type (array double-float 2)  matrix2 )
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  "Multiply two matrices and return the result."
  (let  ((rows1 (array-dimension matrix1 0))
         (cols1 (array-dimension matrix1 1))
         (rows2 (array-dimension matrix2 0))
         (cols2 (array-dimension matrix2 1)))
         (if (not (= cols1 rows2))
             (error "Matrix dimensions are not compatible for multiplication")
             (let ((result (make-array (list rows1 cols2) :element-type '(double-float) :initial-element 0.0d0)))
               (dotimes (i rows1 result)
                 (dotimes (j cols2)
                   (dotimes (k cols1)
                     (incf (aref result i j)
                           (* (aref matrix1 i k)
                              (aref matrix2 k j))))))))))

(defun double-matrix-multiply-array-1d (matrix1 cols1 matrix2 cols2)
  (declare (type (array double-float 1)  matrix1 )
           (type (array double-float 1)  matrix2 )
           (type (unsigned-byte 64) cols1)
           (type (unsigned-byte 64) cols2)
           (optimize (speed 1) (safety 3) (space 0) (debug 3)))
  "Multiply two matrices and return the result."
  (let  ((rows1 (/ (array-dimension matrix1 0) cols1))
         (rows2 (/ (array-dimension matrix2 0) cols2)))
         (if (not (= cols1 rows2))
             (error "Matrix dimensions are not compatible for multiplication")
             (let ((result (make-array (* rows1 cols2) :element-type '(double-float) :initial-element 0.0d0)))
               (dotimes (i rows1 result)
                 (dotimes (j cols2)
                   (dotimes (k cols1)
                     (incf (aref result (+ (* i cols2) j))
                           (* (aref matrix1 (+ (* i cols1) k) )
                              (aref matrix2 (+ (* k cols2) j) ))))))))))


;; declare typed array
;; (defvar typed-array-1 (make-array '(2 2) :element-type
;;                                  '(single-float) :initial-element 3.3))


(defconstant SIZE 100)

;; 1
(defun test-typed-function ()
  (let* ((m (make-array (list SIZE SIZE) :element-type '(double-float) :initial-element 3.45d0))
         (_ (time (double-matrix-multiply-array m m)))) (declare (ignore _))))

;; 2
(defun test-untyped-values ()
  (let* ((m (make-array (list SIZE SIZE) :initial-element 3.45d0))
         (_ (time (matrix-multiply-array m m)))) (declare (ignore _))))

;; 3
(defun test-typed-values ()
  (let* ((m (make-array (list SIZE SIZE) :element-type '(double-float) :initial-element 3.45d0))
         (_ (time (matrix-multiply-array m m)))) (declare (ignore _))))

;; 4
(defun test-typed-1d ()
  (let* ((m (make-array (* SIZE SIZE) :element-type '(double-float) :initial-element 3.45d0))
         (_ (time (double-matrix-multiply-array-1d m SIZE m SIZE)))) (declare (ignore _))))
