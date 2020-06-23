;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defun wrong-arg-count-fun3 (a b) (+ a b))

(eval-when (:load-toplevel)
  (defun test ()
    (wrong-arg-count-fun 10)
    (wrong-arg-count-fun2 10)
    (wrong-arg-count-fun3 10)))

(defvar *muffled-warnings-count* #.(bazel.main::action-muffled-warnings-count bazel.main:*action*))

(eval-when (:load-toplevel)
  (unless (= 3 *muffled-warnings-count*)
    (format t "Muffled-warnings-count: ~D != 3~%" *muffled-warnings-count*)
    (format t "KNOWN BUG: Wrong argument count is not signaled for interpreted functions.!!!~%")))

(defun main ()
  (unless (= 3 *muffled-warnings-count*)
    (warn "KNOWN BUG: Wrong argument count is not signaled for interpreted functions!!!")))
