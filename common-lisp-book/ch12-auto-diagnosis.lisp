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

(defun read-nodes-from-file (fname)
  (setf *node-list* (uiop:read-file-form fname)))

(defun save-nodes-to-file (fname &key if-exists)
  (with-open-file
      (out fname :direction :output ;; append LISP objects to file
                 :if-exists if-exists
                 :if-does-not-exist :create)))

(defun get-input (prompt)
  (format t "~A" prompt)
  (let ((l (read-line *query-io*)))
    (finish-output *query-io*)
    l))

(defun get-expr (prompt)
  (read-from-string (get-input prompt)))

(defun get-new-object ()
  (get-input "PRESS ENTER TO START")
  (let ((name (get-expr "Node name: "))
        (question (get-input "Question: "))
        (yes-case (get-expr "Yes action: "))
        (no-case (get-expr "No action: ")))
    (make-node :name name
               :question question
               :yes-case yes-case
               :no-case no-case)))

(defun process-node (name)
  (let ((node (find-node name)))
    (if (not (null node))
        (if (yes-or-no-p (node-question node))
            (node-yes-case node)
            (node-no-case node))
        (progn (print "Node not found")
               nil))))

(defun run()
  (let ((current-node 'start))
    (get-input "AUTO DIAGNOSIS - PRESS ENTER TO START")
    (loop
      (let ((n (process-node current-node)))
         (if (stringp n)
         (progn (print n) (return-from run))
         (if (null n)
             (return-from run)
             (setf current-node n)))))))
