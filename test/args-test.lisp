;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package :cl-user)

(defun main ()
  (format t "~A~%" (bazel.sbcl:command-line-arguments))
  (assert (equal '("--foo") (bazel.sbcl:command-line-arguments)))) ; NOLINT
