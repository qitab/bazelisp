;;
;; Random test for compilation.
;;
(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *counter* 0))

(eval-when (:compile-toplevel :load-toplevel)
  (incf *counter*))

(eval-when (:compile-toplevel)
  (format t "Compile test: ~A~%" *counter*))

(incf *counter*)
(format t "Loaded test: ~A~%" *counter*)

(defmethod test-no-generic ((a string))
  (format t "Test no generic: ~A~%" a))

(defun bad-style (&optional a &key b)
  (format t "a: ~A, b: ~A~%" a b))

(defun main ()
  (format t "Hello World!!~%")

  (apropos "function-defined-in-c") (terpri)

  (assert (= *counter* 2)) ; compile/load + load ; NOLINT

  (test-some-yet-undefined-function-from-c)

  (bad-style 1 :b 2)

  (test-no-generic "passed")

  (assert (equal *compile-symbols* '(E D C B A))) ; NOLINT

  (assert (equal *load-symbols* '(E D C B A)))) ; NOLINT

(defun asdf-test-main ()
  (format t "Hello ASDF World!!~%")

  (apropos "function-defined-in-c") (terpri)

  (assert (= *counter* 2)) ; compile/load + load ; NOLINT

  (test-some-yet-undefined-function-from-c)

  (bad-style 1 :b 2)

  (test-no-generic "passed")

  (assert (equal *compile-symbols* '(E D M C B A))) ; NOLINT

  (assert (equal *load-symbols* '(E D M C B A)))) ; NOLINT
