(defvar *crypto-text* "")
(defvar *encipher-table* (make-hash-table))
(defvar *decipher-table* (make-hash-table))

(defun init-encipher-table () (setf *encipher-table* (make-hash-table)))
(defun init-decipher-table () (setf *decipher-table* (make-hash-table)))

(defun make-subsitution (enciphered deciphered)
  (setf (gethash deciphered *encipher-table*) enciphered) ;; decoded -> encoded
  (setf (gethash enciphered *decipher-table*) deciphered)) ;; encoded -> decoded

(defun undo-substitution (enciphered)
  (let ((deciphered (gethash enciphered *decipher-table*)))
    (setf (gethash enciphered *decipher-table*) nil)
    (setf (gethash deciphered *encipher-table*) nil)))

(defun clear-tables ()
  (mapcar #'clrhash (list *encipher-table* *decipher-table*)))

(defun decipher-string ()
  (let ((outstr (make-string (length *crypto-text*) :initial-element #\Space)))
    (dotimes (i (length *crypto-text*))
      (let ((c (gethash (aref *crypto-text* i) *decipher-table* )))
        (if c (setf (aref outstr i) c))))))

(defun show-text ()
  (format t "~&~A~%~A" *crypto-text* (decipher-string)))

(defun get-first-char (x)
    (char-downcase
     (char (format nil "~A" x) 0)))

(defun sub-letter ()
  )
