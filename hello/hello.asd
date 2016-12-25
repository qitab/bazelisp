;;;; -*- Mode: Lisp; -*-

(defsystem "hello"
  :description "Example Bazel Hello World application"
  :depends-on (:alexandria)
  :components ((:file "hello")))

(defsystem "hello/chello"
  :description "Example Bazel Hello World application, using C"
  :defsystem-depends-on (:cffi-toolchain)
  :depends-on (:cffi)
  :serial t
  :components
  ((:c-file "chello_lib")
   (:file "chello")))
