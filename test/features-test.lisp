(in-package :cl-user)

(defun main ()
  (assert (eq :xyz (bazel.main::to-feature :xyz)))
  (assert (eq :xyz (bazel.main::to-feature "xyz")))
  (assert (eq 'cl-user::xyz (bazel.main::to-feature "cl-user::xyz")))
  (assert (eq 'cl-user::xyz1234567890
              (bazel.main::to-feature "cl-user:xyz1234567890")))

  (assert (null (ignore-errors (bazel.main::to-feature "123"))))
  (assert (null (ignore-errors (bazel.main::to-feature "()"))))
  (assert (null (ignore-errors (bazel.main::to-feature 123))))

  (assert (find :features *features*)) ; NOLINT
  (assert (find :features-test *features*))) ; NOLINT
