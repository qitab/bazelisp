;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defvar *undefined-function-signaled-p* nil)

;; The bazel.main represents its action as the ACTION datastructure, which contains
;; all the necessary inputs, processing parameters, and intermediate info.
;; One of the lists stored on the action object are the deferred warnings which are
;; checked by a BINARY action at the end when the FINISH-ACTION method is called.
;; An easy way to proof that there was a deferred warning is to check this field
;; on the action before the FINISH-ACTION runs.
;;
;; The following code will check that there was a deferred warning from the BINARY command
;; and than empty this field so that the build and test can complete without failures.
;; The generic below runs BEFORE the binary image is written at the build time.
;;
;; This is possible because we load all the compiled FASLs into the binary image before
;; we dump the image as the build output artifact. When loading the FASLs the generic below
;; will be found at the method dispatch time.

(defmethod bazel.main:finish-action :before ((action bazel.main:action) (command (eql :core)))
  "Checks ACTION if there was an undefined-function warning for the core COMMAND."
  ;; TODO(czak): Need to provide a better path here.
  (assert (equalp (bazel.main:action-deferred-warnings action)
                  '(("third_party/lisp/bazel/test/undefined-macro-reference.lisp"
                     :UNDEFINED-FUNCTION UNDEFINED-MACRO))))
  (setf *undefined-function-signaled-p* t)
  (setf (bazel.main:action-deferred-warnings action) nil))

;; This MAIN function runs at the blaze test run time.
;; Since the UNDEFINED-MACRO-REFERENCE has been compiled as undefined function,
;; the call to it will result in the UNDEFINED-FUNCTION warning.
;; The above generic that runs before the FINISH-ACTION takes care that this code compiles.

(defun main ()
  "The test expects that there was an undefined function."
  (assert *undefined-function-signaled-p*)
  (handler-case
      (progn
        (format t "~A~%" (undefined-macro-reference 1))
        (error "there should be an undefined-function warning"))
    (undefined-function (e) e)))
