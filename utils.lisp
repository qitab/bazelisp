;;; Package contains utilities for Bazel Lisp compilation tool.
;;;

(defpackage #:bazel.utils
  (:use #:cl)
  (:export #:octet
           #:nconcf
           #:prefixp
           #:strip-prefix
           #:dohash
           #:split
           #:to-keyword
           #:write-stringz
           #:read-stringz
           #:funcall-named
           #:funcall-named*))

(in-package #:bazel.utils)


(deftype octet () '(unsigned-byte 8))

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

(defmacro dohash ((k v table) &body body)
  "Iterate through the hash TABLE binding the keys to K and values to V."
  `(loop for ,k being the hash-keys in ,table using (hash-value ,v) do ,@body))

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

(defun write-stringz (string out)
  "Write a 0 terminated STRING to the OUT stream."
  (declare (string string) (stream out))
  (loop for c across string do
    ;; This assumes that the CHAR-CODE is an octet.
    (write-byte (char-code c) out))
  (write-byte 0 out))

(defun read-stringz (stream)
  "Read a 0 terminated string from the STREAM."
  (declare (stream stream))
  (coerce
   (loop for code = (read-byte stream nil)
         until (zerop code)
         collect (code-char code))
   'string))

(defun funcall-named (name &rest args)
  "Call a function with NAME composed of package and function name. Passes ARGS to the function.
 If the package is not found, nothing is called and NIL is returned."
  (let ((split (split name :by #\:)))
    (assert (= 2 (length split))) ; NOLINT
    (let ((package (find-package (first split))))
      (when package
        (let ((function (find-symbol (second split) package)))
          (assert function) ; NOLINT
          (assert (fboundp function)) ; NOLINT
          (apply function args))))))

(defun funcall-named* (name &rest args)
  "Call a function with NAME composed of package and function name. Passes ARGS to the function.
 If the function is not found, nothing is called and NIL is returned."
  (let ((split (split name :by #\:)))
    (assert (= 2 (length split))) ; NOLINT
    (let* ((package (find-package (first split)))
           (function (and package (find-symbol (second split) package))))
      (when (and function (fboundp function))
        (apply function args)))))
