(defvar *crypto-text* "")
(defvar *encipher-table* (make-hash-table))
(defvar *decipher-table* (make-hash-table))

(defun in (e h)
  (not (null (gethash e h))))

(defun init-encipher-table () (setf *encipher-table* (make-hash-table)))
(defun init-decipher-table () (setf *decipher-table* (make-hash-table)))

(defun fill-crypto-text ()
  (setf *crypto-text*
        '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
          "enlpo pib slafml pvv bfwkj")))

(defun make-subsitution (enciphered deciphered)
  (setf (gethash deciphered *encipher-table*) enciphered) ;; decoded -> encoded
  (setf (gethash enciphered *decipher-table*) deciphered)) ;; encoded -> decoded

(defun undo-substitution (enciphered)
  (let ((deciphered (gethash enciphered *decipher-table*)))
    (setf (gethash enciphered *decipher-table*) nil)
    (setf (gethash deciphered *encipher-table*) nil)))

(defun clear-tables ()
  (mapcar #'clrhash (list *encipher-table* *decipher-table*)))

(defun decipher-string (line)
  (let ((outstr (make-string (length line) :initial-element #\Space)))
    (dotimes (i (length line))
      (let ((c (gethash (aref line i) *decipher-table* )))
        (if c (setf (aref outstr i) c))))))

(defun show-line (line)
  (format t "~&~A~%~A~%" line (decipher-string line)))

(defun get-first-char (x)
    (char-downcase
     (char (string x) 0)));;(format nil "~A" x) 0)))

(defun show-text (text)
  (map nil #'show-line text))


(defun read-letter ()
  (let ((r (read)))
    (case r
      ((done undo) r)
      (otherwise (char (string r) 0)))))

(defun sub-letter (l)
  (if (in l *decipher-table*)
      (format t "~&~A already mapped to  ~A" l (gethash l *decipher-table*))
      (progn
        (format t "~&What does ~A decipher to? " l)
        (let ((c (read-char)))
          (if (alphanumericp c)
              (make-subsitution l c)
              (print "Not a letter")) nil))))

;; (defun undo-letter ()
;;                (format t "~&Undo which letter? ")
;;                (let (c (read-char))
;;                  (if (alphanumeric c)
;;                      ))
