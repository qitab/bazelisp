;; Hello World in Lisp
(defpackage :chello
  (:use :common-lisp :cffi)
  (:export #:main))

(in-package :chello)

(defcfun ("hello_string" hello-string) :string)

(defun main ()
  "Trivial Hello World function"
  (format t "~A~%" (hello-string)))
