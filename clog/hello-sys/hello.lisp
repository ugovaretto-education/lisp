(defpackage :hello-package
  (:nicknames :hello-pkg)
  (:use :cl :clog)
  (:export :hello-world))

(in-package :hello-package)

(defun hello-private (body)
  "Create a div on new pages containing - hello world"
  (create-div body :content "hello world"))

(defun hello-world ()
  "Initialize CLOG and open a browser"
  (initialize 'hello-private)
  (open-browser))
