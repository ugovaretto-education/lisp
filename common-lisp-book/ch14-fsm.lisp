;;
;; START [NICKEL]->(Clunk!)->HAVE-5
;; HAVE-5 [DIME]->(Clink!)->HAVE-15
;; HAVE-15 [GUM-BUTTON]->(Deliver gum packet)->END
;;
;; STATES
;; START HAVE-5 HAVE-10 HAVE-15 HAVE-20 END

(defstruct (node (:print-function print-node))
  (name nil)
  (inputs nil)
  (outputs nil))

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

(defvar *nodes*)
(defvar *arcs*)
(defvar *current-node*)

(defun initialize ()
  (setf *nodes* nil)
  (setf *arcs* nil)
  (setf *current-node* nil))

(defun add-node (name)
  (let ((new-node (make-node :name name)))
    (push new-node *nodes*)))

(defun find-node (name)
  (or (find name *nodes* :key #'node-name)
      (error "No node names ~A exists." name)))

(defmacro add-arc )
