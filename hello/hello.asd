;;;; -*- Mode: Lisp; -*-

(asdf:defsystem "bazel/hello"
  :description "Example Bazel Hello World application"
  :depends-on (:alexandria)
  :components ((:file "hello")))
