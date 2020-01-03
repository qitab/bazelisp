(in-package :cl-user)

(defun main ()
  (let* ((*error-output* (make-string-output-stream)))
    (bazel.main::print-warning-conditions
     ""
     (list (list "pre-foo" "foo.lisp" "bar.lisp")
           (list "non-struct" "struct" "type")
           (list "bad-src" "good-src" "stop"))
     "")
    (assert (search "WARNING:"
                    (get-output-stream-string
                     *error-output*)))))
