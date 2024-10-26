;;;; Utility functions
(defun inh (e h)
  (not (null (gethash e h))))

(defun geth (k h)
  (gethash k h))

(defun puth (k h v)
  (setf (gethash k h) v))

(defun killh (k h)
  (remhash k h))

(defun max-elem (seq)
  (labels
    ((max-e (xs m)
       (cond
         ((null xs) m)
         (t (cond
              ((> (car xs) m) (max-e (cdr xs) (car xs)))
              (t (max-e (cdr xs) m)))))))
    (max-e seq (car seq))))
;;;;


(defvar *crypto-text* ())
(defvar *encipher-table* (make-hash-table))
(defvar *decipher-table* (make-hash-table))


(defun init-encipher-table () (setf *encipher-table* (make-hash-table)))
(defun init-decipher-table () (setf *decipher-table* (make-hash-table)))

(defun fill-crypto-text ()
  (setf *crypto-text*
        '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
          "enlpo pib slafml pvv bfwkj")))

(defun make-subsitution (enciphered deciphered)
  (setf (gethash enciphered *decipher-table*) deciphered) ;; decoded -> encoded
  (setf (gethash deciphered *encipher-table*) enciphered)) ;; encoded -> decoded

(defun undo-substitution (enciphered)
  (let ((deciphered (gethash enciphered *decipher-table*)))
    (setf (gethash enciphered *decipher-table*) nil)
    (setf (gethash deciphered *encipher-table*) nil)))

(defun clear-tables ()
  (mapcar #'clrhash (list *encipher-table* *decipher-table*)))

(defun decipher-string (line)
  (let ((outstr (make-string (length line) :initial-element #\Space)))
    (dotimes (i (length line))
      (let ((c (gethash (aref line i) *decipher-table*)))
        (if (not (null c)) (setf (aref outstr i) c))))
    outstr))

(defun show-line (line)
  (format t "~&~A~%~A~%" line (decipher-string line)))

(defun get-first-char (x)
    (char-downcase
     (char (string x) 0)));;(format nil "~A" x) 0)))

(defun show-text (text)
  (map nil #'show-line text)
  (let ((m (max-elem (mapcar #'length text))))
   (format t "~&~A" (make-string m :initial-element #\-))))

(defun read-letter ()
  (let ((r (read)))
    (case r
      ((end undo) r)
      (otherwise (char-downcase (char (string r) 0))))))

(defun sub-letter (l)
  (if (inh l *decipher-table*)
      (format t "~&~A already mapped to  ~A" l (gethash l *decipher-table*))
      (progn
        (format t "~&What does ~A decipher to? " l)
        (let ((c (read-char)))
          (if (alphanumericp c)
              (make-subsitution l c)
              (print "Not a letter")) nil))))

(defun undo-letter ()
  (format t "~&Undo which letter? ")
  (let ((c (read-char)))
    (if (alphanumericp c)
        (if (inh c *decipher-table*)
            (progn
              (killh (geth c *decipher-table*) *encipher-table*)
              (killh c *decipher-table*))
            (format t "~&~A not found" c))
        (print "Error - not a character"))))

(defun solve ()
  (fill-crypto-text)
  (init-encipher-table)
  (init-decipher-table)
  (loop
    (show-text *crypto-text*)
    (format t "~&Substitute which letter? ")
    (let ((l (read-letter)))
      (case l
        (undo (undo-letter))
        (end (return))
        (otherwise (sub-letter l)))
      (print l))))
