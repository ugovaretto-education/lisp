;; invoke with: sbcl --load ./compile.lisp
;;(push '#".")
(push (uiop:getenv "PWD") ql:*local-project-directories*)
(ql:quickload :clog)
(load (compile-file "hello.lisp"))
(save-lisp-and-die "hello.exe" :toplevel #'hello-pkg:hello-world :executable t)
