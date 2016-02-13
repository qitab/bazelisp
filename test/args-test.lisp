(in-package :cl-user)

(defun main ()
  (format t "~A~%" (bazel.sbcl:command-line-arguments))
  (assert (equal '("--foo") (bazel.sbcl:command-line-arguments)))) ; NOLINT
