(defstruct node
  (name nil)
  (question nil)
  (yes-case nil)
  (no-case nil))

(defvar *node-list* ())

(defun init () (setf *node-list* nil))

(defun add-node (n)
  (push n *node-list*)
  (node-name n))

(defun find-node (name)
  (find-if #'(lambda (n) (eq (node-name n) name)) *node-list*))

(defun process-node (name)
  (let ((node (find-node name)))
    (if (not (null node))
        (if (yes-or-no-p (node-question node))
            (node-yes-case node)
            (node-no-case node))
        (progn (print "Node not found"
                      ) nil))))
