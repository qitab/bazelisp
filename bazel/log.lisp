;;;
;;; Simple logging utilities for the Bazel Lisp Compile utility.
;;;

(defpackage bazel.log
  (:use :cl)
  (:export #:info #:message
           #:verbose #:vv #:vvv
           #:fatal #:fatal-error #:non-fatal-error
           #:*verbose*))

(in-package #:bazel.log)

(declaim (fixnum *verbose*))
(defvar *verbose* 0)

(define-condition fatal-error (error)
  ((message :reader fatal-error-message :initarg :message :type (or null string)))
  (:documentation "An error caused by the log:fatal.")
  (:report (lambda (e s)
             (format s "~@[~A~]" (fatal-error-message e)))))

(deftype non-fatal-error () '(and error (not fatal-error)))

(defun message (severity level control &rest args)
  "Format and print a log message.
 The first argument is SEVERITY: :INFO, :WARNING, :ERROR.
 The second argument specifies log verbosity LEVEL.
 The ARGS are applied to the CONTROL string to produce the log output."
  (declare (fixnum level))
  (when (>= *verbose* level)
    (with-standard-io-syntax
      (let ((*print-readably* nil)
            (out (if (eq severity :info)
                     *standard-output*
                     *error-output*)))
        (format out "~&~A: ~?~%" severity control args)))))

(defun verbose (control &rest args)
  "Same as message with level 1. CONTROL is the format control string that operates on ARGS."
  (apply #'message :info 1 control args))

(defun vv (control &rest args)
  "Same as message with level 2. CONTROL is the format control string that operates on ARGS."
  (apply #'message :info 2 control args))

(defun vvv (control &rest args)
  "Same as message with level 2. CONTROL is the format control string that operates on ARGS."
  (apply #'message :info 3 control args))

(defun info (control &rest args)
  "Same as message with level 0. CONTROL is the format control string that operates on ARGS."
  (apply #'message :info 0 control args))

(defun fatal (control &rest args)
  "Format an print a fail message then exit.
 CONTROL is the format control string that operates on ARGS."
  (apply #'message :fatal 0 control args)
  (with-simple-restart (continue "Continue from the fatal Blaze error.")
    (error 'fatal-error :message (apply #'format nil control args))))
