;; invoke with: sbcl --load ./compile.lisp
;;(push '#".")
(push (uiop:getenv "PWD") ql:*local-project-directories*)
;; (load "~/.local/quicklisp/asdf.lisp")
;; (load "~/.local/quicklisp/setup.lisp")
(ql:quickload :hello-sys)
(setq uiop:*image-entry-point* #'hello-pkg:hello-world)
(uiop:dump-image "hello.exe" :executable t)
