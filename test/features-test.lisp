(in-package :cl-user)

(defun main ()
  (assert (find :features *features*)) ; NOLINT
  (assert (find :features-test *features*))) ; NOLINT
