(defvar name 'fred)

(defvar sentence `(this is ,name from pittsburgh))

(defvar sentence-2 `(I gave ,name about ,(* 25 8) dollars))

(defmacro simple-incf (var &optional (amount 1))
           (list 'setq var (list '+ var amount)))

(defmacro simpler-incf (var &optional (amount 1))
           `(setq ,var (+ ,var ,amount)))

(defmacro simple-rotatef (x y)
  `(let* ((tmp ,y))
     (setf ,y ,x)
     (setf ,x tmp)))

(defmacro showvar (x)
  (list 'format 't "~&The value of ~A is ~A" (symbol-name x) x ))

(defmacro set-mutual (x y)
  `(setf x y))

(defmacro set-zero (&rest variables)
  `(progn ,@(mapcar #'(lambda (var)
                        (list 'setf var 0))
                    variables)'(zeroed ,@variables)))
