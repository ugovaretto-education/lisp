;; properties
(setf (get 'fred 'gender) 'male)
(setf (get 'fred 'age) 23)
(setf (get 'fred 'siblings) '(george wanda))

(get 'fred 'age)
(get 'fred 'not-present 'default)

;; add an element to the 'prop' property
(defun addprop (sym elem prop)
  (pushnew elem (get sym prop)))

(defun record-meeting (attendant person-meeting-with)
  (addprop attendant person-meeting-with 'has-met)
  (addprop person-meeting-with attendant 'has-met))

(record-meeting 'little-red 'wolfie)
(record-meeting 'wolfie 'grandma)

;; GC statistics
(room)

;; coercion
(coerce "Cockatoo" 'list)
(coerce '(#\b #\i #\r #\d) 'string)
(coerce '(foo bar baz) 'vector)

;; multi-type, multi-dimensional mapping
(map 'list #'+ '(1 2 3 4) '#(10 34 50 60) '(1 1 1 1))
(map 'vector #'+ '(1 2 3 4) '#(10 34 50 60) '(1 1 1 1))
(map 'simple-vector #'+ '(1 2 3 4) '#(10 34 50 60) '(1 1 1 1))
(map 'string
      #'(lambda (x) (if (oddp x) #\1 #\0))
     '(1 2 3 4))

;; apply, no transform, applies 'print' to each element
(map nil #'print "a b")

;; array as string
(make-array 3 :element-type 'string-char
              :initial-contents '(#\M #\o #\m))
