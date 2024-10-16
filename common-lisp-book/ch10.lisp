;; ASSOC
(defvar amap '((:key1 1)
               (:key2 2)
               (:key3 3)))
(print (cdr (assoc :key2 amap)))
(print amap)
(setf amap (remove :key2 amap));; :key #'car))
(nconc amap '((:keyX 200)))
(print amap)

(defvar *things* '((object1 large green shiny cube)
                   (object2 small red dull metal cube)
                   (object3 red small dull plastic cube)))

(defun rename-things-key (obj newname)
  (setf (car (assoc obj *things*)) newname))

(defun rename-things-value (obj newname)
  (setf (cdr (assoc obj *things*)) newname))

;; SUBST NSUBST
(defvar tree '())

(setf tree '(i say (e i (e i) o)))

(defvar st (subst 'a 'e tree))
(print st)
(print tree)
(nsubst 'a 'e tree)
(print tree)

;; APPEND NCONC
(defvar alist (cons 1 2))
(print (append alist '(3 4)))
(print alist)
(nconc alist '(3 4))
(print alist)

;; NREVERSE
(print (nreverse alist))
