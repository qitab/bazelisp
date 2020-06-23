;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (format t "Compile B~%"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push 'B *compile-symbols*))

(format t "Load B~%")
(push 'B *load-symbols*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "B> c:~A~20Tl:~A~%" *compile-symbols* *load-symbols*))

(eval-when (:compile-toplevel)
  (assert (not (fboundp 'some-yet-undefined-function-from-c))))

(defun test-some-yet-undefined-function-from-c ()
  (some-yet-undefined-function-from-c "called by B"))
