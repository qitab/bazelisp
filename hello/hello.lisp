;; Hello World in Lisp
(defpackage :hello
  (:use :common-lisp :uiop :alexandria)
  (:export #:main))

(defun main ()
  "Trivial Hello World function"
  ;; exercise alexandria
  (assert (= 720 (factorial 6)))
  ;; exercise uiop
  (format! t "Hello, World!~%")
  (quit 0))
