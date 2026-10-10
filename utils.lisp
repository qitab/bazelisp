;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Package contains utilities for Bazel Lisp compilation tool.
;;;

(defpackage #:bazel.utils
  (:use #:cl)
  (:export #:nconcf
           #:prefixp
           #:strip-prefix
           #:split
           #:to-keyword))

(in-package #:bazel.utils)

(define-modify-macro nconcf (&rest lists) nconc
  "Helper macro doing an nconc and setf to the first argument.")

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
