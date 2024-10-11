(asdf:defsystem #:hello-sys
  :description "Common Lisp - The Tutorial Part 3"

  :author "david@botton.com"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog)
  :components ((:file "hello")))
