;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package :cl-user)

(defun main ()
  (assert (eq :xyz (bazel.main::to-feature :xyz)))
  (assert (eq :xyz (bazel.main::to-feature "xyz")))
  (assert (eq 'cl-user::xyz (bazel.main::to-feature "cl-user::xyz")))
  (assert (eq 'cl-user::xyz1234567890
              (bazel.main::to-feature "cl-user:xyz1234567890")))

  (assert (eq :|123| (bazel.main::to-feature "123")))
  (assert (null (ignore-errors (bazel.main::to-feature 123))))

  (assert (find :bazel *features*)) ; NOLINT
  (assert (find :features *features*)) ; NOLINT
  (assert (find :features-test *features*))) ; NOLINT
