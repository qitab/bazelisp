;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;
;;; Simple logging utilities for the Bazel Lisp Compile utility.
;;;

(defpackage bazel.log
  (:use :cl)
  (:shadow #:error #:warning)
  (:export #:message
           #:info
           #:error
           #:warning
           #:verbose #:vv #:vvv
           #:fatal #:fatal-error #:non-fatal-error
           #:*verbose*
           #:with-safe-io-syntax))

(in-package #:bazel.log)

(declaim (fixnum *verbose*))
(defvar *verbose* 0)

(define-condition fatal-error (cl:error)
  ((message :reader fatal-error-message :initarg :message :type (or null string)))
  (:documentation "An error caused by the log:fatal.")
  (:report (lambda (e s)
             (format s "~@[~A~]" (fatal-error-message e)))))

(deftype non-fatal-error () '(and cl:error (not fatal-error)))

(defun ttyp (&optional (stream *standard-output*))
  "Returns true if the STREAM is a TTY."
  (declare (ignorable stream))
  #+sbcl
  (let* ((stream (sb-impl::stream-output-stream stream))
         (fd (and (sb-sys:fd-stream-p stream)
                  (sb-sys:fd-stream-fd stream))))
    (and (integerp fd)
         (plusp (sb-unix:unix-isatty fd)))))

(defun ansi-code (severity)
  "Return an ANSI escape code used to print the severity on screen."
  (case severity
    ((:fatal :error) 31)
    ((:warning)      33)
    (t               32)))

(defmacro with-safe-io-syntax (&body body)
  "Execute BODY with IO environment set to safe values."
  `(with-standard-io-syntax
     (let ((*print-readably* nil)
           (*print-circle* t)
           #+sbcl
           (sb-ext:*suppress-print-errors* t))
        (locally ,@body))))

(defun message (severity level control &rest args)
  "Format and print a log message.
 The first argument is SEVERITY: :INFO, :WARNING, :ERROR.
 The second argument specifies log verbosity LEVEL.
 The ARGS are applied to the CONTROL string to produce the log output."
  (declare (fixnum level))
  (when (>= *verbose* level)
    (with-safe-io-syntax
      (let ((out (if (eq severity :info)
                     *standard-output*
                     *error-output*)))
        (if (ttyp out)
            (format out "~&~c[~Dm~A~c[0m: ~?~%"
                    #\ESC (ansi-code severity) severity #\ESC control args)
            (format out "~&~A: ~?~%" severity control args))))))

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

(defun warning (control &rest args)
  "Message with warning severity. CONTROL is the format control string that operates on ARGS."
  (apply #'message :warning 0 control args))

(defun error (control &rest args)
  "Message with error severity. CONTROL is the format control string that operates on ARGS."
  (apply #'message :error 0 control args))

(defun fatal (control &rest args)
  "Format an print a fail message then exit.
 CONTROL is the format control string that operates on ARGS."
  (apply #'message :fatal 0 control args)
  (with-simple-restart (continue "Continue from the fatal bazel error.")
    (with-safe-io-syntax
      (cl:error 'fatal-error :message (apply #'format nil control args)))))
