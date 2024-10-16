;; DO DOTIMES
(dotimes (i 4)
  (format t "~&I is ~S." i))

(dolist (x '(red green blue (turquoise violet)) 'flowers)
  (format t "~&Roses are ~S." x))

(defun find-firs-odd (xs)
  (dolist (e xs)
    (format t "~& Tesing ~S..." e)
    (when (oddp e)
      (format t "found one")
      (return e))))

(defun check-all-odd (xs)
  (dolist (e xs t)
    (format t "~&Checking ~S..." e)
    (if (not (oddp e)) (return nil))))

(defun rec-ffo (xs)
  "Recursively find first odd number."
  (cond ((null xs) nil)
        ((oddp (first xs) (first xs)))
        (t (rec-ffo (rest xs)))))

(defun it-ffo (xs)
  "Iteratively find first odd number."
  (dolist (x xs nil)
    (if (oddp x)
        (return x))))

(defun it-fact (n)
  (let ((prod 1))
    (dotimes (i n prod)
      (setf prod (* prod (+ i 1))))))

(defun it-intersection (xs ys)
  (let ((result-set nil))
    (dolist (e xs result-set)
      (when (member e ys )
        (push e result-set)))))

(defun it-length (xs)
  (let ((l 0))
    (dolist (x xs l)
      (incf l))))

(defun it-nth (n xs)
  (let ((i 0))
    (dolist (x xs nil)
      (if (eq i n) (return x))
      (incf i))))


;; DO

(defun launch (n)
  (do ((cnt n (- cnt 1)))
      ((zerop cnt) (format t "Blast off!"))
    (format t "~S..." cnt)))

(defun check-all-odd (xs)
  (do ((ys xs (cdr ys)))
      ((null ys) t)
  (if (evenp (first ys)) (return nil))))

(defun count-slices (loaf)
  (do ((cnt 0 (+ cnt 1))
       (xs loaf (cdr xs)))
      ((null xs) cnt)))

(defun find-largest (list-of-numbers)
  (let ((largest (first list-of-numbers)))
    (dolist (element (rest list-of-numbers)
                     largest)
      (when (> element largest)
        (setf largest element)))))

;; DO*

(defun ffo-with-do* (list-of-numbers)
  (do* ((x list-of-numbers (rest x))
        (e (first x) (first x)))
       ((null x) nil)
    (if (oddp e) (return e))))

(defun find-largest-do* (xs)
  (do* ((ys xs (cdr ys))
        (e (car xs) (if (not (null ys)) (car ys) e))
        (largest (car xs) (if (> e largest) e largest)))
       ((null ys) largest)))

;; RETURN-FROM
(defun find-first-odd (x)
  (format t "~&Searching for an odd number...")
  (dolist (element x)
    (when (oddp element)
      (format t "~&Found ~S." element)
      (return-from find-first-odd element)))
  (format t "~&None found.")
  'none)

(defun square-list (x)
  (mapcar
   #'(lambda (e)
       (if (numberp e)
           (* e e)
           (return-from square-list 'nope)))
   x))

;; PROG

(prog1 (setf x 'foo)
  (setf x 'bar)
  (setf x 'baz)
  (format t "~&X is ~S" x))

(prog2 (setf x 'foo)
    (setf x 'bar)
  (setf x 'baz)
  (format t "~&X is ~S" x))

(progn (setf x 'foo)
       (setf x 'bar)
       (setf x 'baz)
       (format t "~&X is ~S" x))

;; OPTIONAL

(defun foo (x &optional y)
  (format t "~&X is ~S" x)
  (format t "~&Y is ~S" y)
  (list x y))

;; REST (VARARGS)

(defun average (&rest args)
  (/ (reduce #'+ args)
     (length args)
     1.0))

;; NAMED ARGUMENTS

(defun make-sundae (name &key (size 'regular)
                           (ice-cream 'vanilla)
                           (syrup 'hot-fudge)
                           nuts
                           cherries
                           whipped-cream)
  (list 'sundae
        (list 'for name)
        (list ice-cream 'with syrup 'syrup)
        (list 'toppings '=
              (remove nil
                      (list (and nuts 'nuts)
                            (and cherries 'cherries)
                            (and whipped-cream
                                 'whipped-cream))))))

;; AUXILIARY VARIABLES

(defun average (&rest args
                &aux (len (length args)))
  (/ (reduce #'+ args) len 1.0))
