;;;; -*- Mode: Lisp; -*-

(defsystem "hello"
  :description "Example Bazel Hello World application"
  :class program-system
  :entry-point "hello:main"
  :depends-on (:alexandria)
  :components ((:file "hello")))

(defsystem "hello/chello"
  :description "Example Bazel Hello World application, using C"
  :class program-system
  :entry-point "chello:main"
  :defsystem-depends-on (:cffi-toolchain)
  :depends-on (:cffi-toolchain)
  :serial t
  :components
  ((:c-file "chello_lib")
   (:file "chello")))
