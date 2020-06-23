;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defun undefined-macro-reference (arg)
  "A function that takes ARG and invokes undefined macro C."
  (undefined-macro arg))
