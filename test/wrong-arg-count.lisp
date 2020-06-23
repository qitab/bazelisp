;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; Tests the wrong argument count.
;;

(defun wrong-arg-count-fun (a b) (+ a b))

(declaim (ftype (function (number number) (values number &optional)) wrong-arg-count-fun2))
(defun wrong-arg-count-fun2 (a b) (+ a b))
