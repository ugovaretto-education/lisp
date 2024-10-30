;;
;; START [NICKEL]->(Clunk!)->HAVE-5
;; HAVE-5 [DIME]->(Clink!)->HAVE-15
;; HAVE-15 [GUM-BUTTON]->(Deliver gum packet)->END
;;
;; STATES
;; START HAVE-5 HAVE-10 HAVE-15 HAVE-20 END

(defstruct (node (:print-function print-node))
  (name nil)
  (inputs ())
  (outputs ()))

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "#<Node ~A>"
          (node-name node)))

(defstruct (arc (:print-function print-arc))
  (from nil)
  (to nil)
  (label nil)
  (action nil))

(defun print-arc (arc stream depth)
  (declare (ignore depth))
  (format stream "#<ARC ~A / ~A / ~A>"
          (node-name (arc-from arc))
          (arc-label arc)
          (node-name (arc-to arc))))

(defvar *nodes* ())
(defvar *arcs* ())
(defvar *current-node* nil)

(defun initialize ()
  (setf *nodes* ())
  (setf *arcs* ())
  (setf *current-node* nil))

(defmacro defnode (name)
  `(add-node ',name))

(defun add-node (name)
  (let ((new-node (make-node :name name)))
    (push new-node *nodes*)))

(defun find-node (name)
  (or (find name *nodes* :key #'node-name)
      (error "No node names ~A exists." name)))

(defun find-arc (from to)
   (find-if (lambda (arc)
              (let
                  ((nf (node-name (arc-from arc)))
                   (nt (node-name (arc-to arc))))
                (and (eq nf from) (eq nt to))))
            *arcs*))


(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun add-arc (from-name label to-name action)
  (let ((a (find-arc from-name to-name)))
    (if a a
        (let* ((from (find-node from-name))
               (to (find-node to-name))
               (new-arc (make-arc :from from
                                  :label label
                                  :to to
                                  :action action)))
          (push new-arc *arcs*)
          (push new-arc (node-outputs from))
          (push new-arc (node-inputs to))
          new-arc))))

(defun fsm (&optional (starting-state 'start))
  (initialize)
  (init-states)
  (setf *current-node* (find-node starting-state))
  (loop
    (one-transition)
    (when (null (node-outputs *current-node*)) (return))))

(defun one-transition ()
  (format t "~&Current state: ~A, event: " (node-name *current-node*))
  (let* ((label (read))
         (arc (find label (node-outputs *current-node*) :key #'arc-label)))
    (if arc (setf *current-node* (arc-to arc)))))

(defun init-states()
  (defnode start)
  (defnode have-5)
  (defnode have-10)
  (defnode have-15)
  (defnode have-20)
  (defnode end)
  (defarc start nickel have-5 "Clunk!")
  (defarc start dime have-10 "Clink!")
  (defarc start coin-return start "Nothing to return.")
  (defarc have-5 nickel have-10 "Clunk!")
  (defarc have-5 dime have-15 "Clink!")
  (defarc have-5 coin-return start "Returned five cents.")
  (defarc have-10 nickel have-15 "Clunk!")
  (defarc have-10 dime have-20 "Clink!")
  (defarc have-10 coint-return start "Returned ten cents.")
  (defarc have-15 nickel have-20 "Clunk!")
  (defarc have-15 dime have-20 "Nickel change.")
  (defarc have-15 gum-button end "Deliver gum.")
  (defarc have-15 coin-return start "Returned fifteen cents.")
  (defarc have-20 nickel have-20 "Nickel returned.")
  (defarc have-20 dime have-20 "Dime returned.")
  (defarc have-20 gum-button end
    "Deliver gum, nickel change.")
  (defarc have-20 mint-button end "Deliver mints.")
  (defarc have-20 coin-return start "Returned twenty cents.")
  (setf *current-node* (find-node 'start)))
