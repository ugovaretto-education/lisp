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

(defvar *nodes* ())
(defvar *arcs* ())
(defvar *current-node* ())

(defun initialize ()
  (setf *nodes* nil)
  (setf *arcs* nil)
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
   (or (find-if (lambda (arc)
              (let
                  ((nf (node-name (arc-from arc)))
                   (nt (node-name (arc-to arc))))
                (and (eq nf from) (eq nt to))))
            *arcs*)
       (error "Cannot find Arc going from ~A to ~A" from to)))


(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
         (to (find-node to-name))
         (new-arc (make-arc :from from
                            :label label
                            :to to
                            :action action)))
    (setf *arcs* (nconc *arcs* (list new-arc)))
    (setf (node-outputs from)
          (nconc (node-outputs from)
                 (list new-arc)))
    (setf (node-inputs to)
          (nconc (node-inputs to)
                 (list new-arc)))
    new-arc))
