;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defun main ()
  (format t "Compilation-mode: ~A~%" '(#+dbg :dbg #+opt :opt #-(or opt dbg) :fastbuild))
  #+sbcl (sb-ext:describe-compiler-policy))
