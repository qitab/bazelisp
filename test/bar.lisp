;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(eval-when (:compile-toplevel) (format t "Compile: BAR~%"))


(defun bar ()
  (foo :bar))
