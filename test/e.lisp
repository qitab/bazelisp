;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (format t "Compile E~%"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push 'E *compile-symbols*))

(format t "Load E~%")
(push 'E *load-symbols*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "E> c:~A~20Tl:~A~%" *compile-symbols* *load-symbols*))

(defun test-undefined-function-2 ()
  (some-yet-undefined-function-from-c "called by E"))

(defun zot ()
  (flet ((sub-zot (x)
           (format t "Hey ~d~%" x)))
    (sub-zot 1)
    (sub-zot "again")))
