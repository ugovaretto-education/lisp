(defvar *hist-array* (make-array 11))
(defvar *total-points* 0)

(defun new-histogram (num-bins)
  (setf *hist-array* (make-array num-bins))
  (setf *total-points* 0))

(defun record-value (n)
  (if (>= n (length *hist-array*))
      (error "Number out of range")
      (progn
        (incf (aref *hist-array* n))
        (incf *total-points*))))

;; (format nil "~v@{~A~:*~}" 3 #\*) Repeat '*' three times

(defun print-histogram ()
  (map nil #'(lambda (e) (format t "~v@{~A~:*~}~%" e #\*)) *hist-array*))

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defun fill-with-random-values (count)
  (dotimes (i count)
    (record-value (random (length *hist-array*)))))
