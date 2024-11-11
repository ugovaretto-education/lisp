(defun fact (n acc)
  (if (zerop n)
      acc
      (fact (1- n) (* acc n))))

;; without fixnum it gives a warning "unable to to FIXNUM inline arithmetic"
(defun fact2 (n)
  (declare (fixnum n))
  (let ((acc 1))
    (dotimes (i n acc)
      (declare (fixnum i acc))
      (setf acc (* (1+ i) acc)))))
                  
(defun fact3 (n)
  ;;(declare (fixnum n))
  (let ((acc 1))
    (dotimes (i n acc)
      ;;(declare (fixnum i acc))
      (setf acc (* (1+ i) acc)))))


    
                
