;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Generates a stack-allocation note to test the
;;;    nowarn = ["stack-allocate-note"]
;;; parameter.
;;;

(in-package :cl-user)

(defun stack-alloc-list (size)
  (let ((l (make-list size)))
    (declare (dynamic-extent l))
    (length l)))

(defun main ()
  (format t "len: ~A~%" (stack-alloc-list 63)))
