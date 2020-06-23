;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; TODO(czak): This requires proper path.
(defconstant +data+ "third_party/lisp/bazel/test/foo.data")

(eval-when (:compile-toplevel)
  #+sbcl (format t "Compile CWD: ~A~%" (sb-posix:getcwd))
  #+sbcl (format t "Compile arg0: ~A~%" (bazel.sbcl:program-name))
  (format t "Compile default pathname: ~A~%" *default-pathname-defaults*)
  (format t "Compile: FOO: ~A~%" (with-open-file (in +data+) (read in))))

(eval-when (:load-toplevel)
  #+sbcl (format t "Load CWD: ~A~%" (sb-posix:getcwd))
  #+sbcl (format t "Load arg0: ~A~%" (bazel.sbcl:program-name))
  (format t "Load default pathname: ~A~%" *default-pathname-defaults*)
  (format t "Load: FOO: ~A~%" (with-open-file (in +data+) (read in))))

(defun foo (from)
  (format t "Called foo from: ~A~%" from))

(defun foo-data-test ()
  (with-open-file (in +data+)
    (assert (equal 1234 (read in))))) ; NOLINT
