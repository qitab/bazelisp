;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Package contains utilities for Bazel Lisp compilation tool.
;;;

(defpackage #:bazel.utils
  (:use #:cl)
  (:export #:message
           #:report-info
           #:report-error
           #:report-warning
           #:verbose #:vv
           #:fatal #:fatal-error
           #:*verbose*
           #:with-safe-io-syntax)
  (:export #:prefixp
           #:strip-prefix
           #:split
           #:to-keyword))

(in-package #:bazel.utils)

(defun prefixp (prefix string)
  "Test if STRING starts with the PREFIX."
  (declare (string string prefix))
  (let ((len (length prefix)))
    (and (<= len (length string)) (string= string prefix :end1 len))))

(defun strip-prefix (prefix string)
  "If the STRING is prefixed with the PREFIX, remove it, and return (values stripped t).
 Otherwise return the complete string and NIL."
  (declare (string prefix string))
  (if (prefixp prefix string)
      (values (subseq string (length prefix)) t)
      (values string nil)))

(defun split (string &key (by #\Space))
  "Split the STRING by the separator BY into a list. Empty strings are not included."
  (declare (type (or string null) string) (character by))
  (when string
    (loop for start fixnum = 0 then (1+ pos)
          for pos = (position by string :start start)
          for part = (subseq string start pos)
          when (plusp (length (the string part)))
            collect part
          while pos)))

(defun to-keyword (string)
  "Transforms the STRING designator into a keyword.
 The string is interned in the upper case into the keyword package."
  (intern (string-upcase string) :keyword))

(declaim (fixnum *verbose*))
(defvar *verbose* 0 "Default verbosity")

(define-condition fatal-error (error)
  ((message :reader fatal-error-message :initarg :message :type (or null string)))
  (:documentation "An error caused by the log:fatal.")
  (:report (lambda (e s)
             (format s "~@[~A~]" (fatal-error-message e)))))

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
        (format out "~&~A: ~?~%" severity control args)))))

(defun verbose (control &rest args)
  "Same as message with level 1. CONTROL is the format control string that operates on ARGS."
  (apply #'message :info 1 control args))

(defun vv (control &rest args)
  "Same as message with level 2. CONTROL is the format control string that operates on ARGS."
  (apply #'message :info 2 control args))

(defun report-info (control &rest args)
  "Same as message with level 0. CONTROL is the format control string that operates on ARGS."
  (apply #'message :info 0 control args))

(defun report-warning (control &rest args)
  "Message with warning severity. CONTROL is the format control string that operates on ARGS."
  (apply #'message :warning 0 control args))

(defun report-error (control &rest args)
  "Message with error severity. CONTROL is the format control string that operates on ARGS."
  (apply #'message :error 0 control args))

(defun fatal (control &rest args)
  "Format an print a fail message then exit.
 CONTROL is the format control string that operates on ARGS."
  (apply #'message :fatal 0 control args)
  (with-simple-restart (continue "Continue from the fatal Bazel error.")
    (with-safe-io-syntax
      (error 'fatal-error :message (apply #'format nil control args)))))
