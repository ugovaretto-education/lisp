(defstruct (starship (:print-function print-starship))
  (name nil)
  (speed 0)
  (condition 'green)
  (shields 'down))

(defvar a-starship (make-starship :name "Reliant"
                                  :shields 'damaged))

(defvar another-starship (make-starship :name "Reliant"
                                        :shields 'damaged))

(defun print-starship (s stream depth)
  (declare (ignore depth))
  (format stream "#<STARSHIP ~A>" (starship-name s)))


(equalp a-starship another-starship) ;; TRUE

(equal a-starship another-starship) ;; FALSE

(equal "ENTERPRISE" "enterprise") ;; FALSE

(equalp "ENTERPRISE" "enterprise") ;; TRUE

;; inheritance

(defstruct ship
  (name nil)
  (captain nil)
  (crew-size nil))

(defstruct (spaceship (:include ship))
  (weapons nil)
  (shields nil))

(defstruct (supply-ship (:include ship))
  (cargo nil))


(defvar space-ship (make-space-ship))
(ship-name space-ship)
(spaceship-name space-ship)
