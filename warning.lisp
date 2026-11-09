;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; A basic package defining compiler warnings independent from
;;; the CL implementation. TODO(czak): Extend for other than SBCL.
;;;

(cL:defpackage #:bazel.warning
  (:use #:common-lisp)
  (:export #:inlining-notinline
           #:redefined-macro
           #:redefined-function
           #:optional-and-key
           #:uninteresting-condition))

(cl:in-package #:bazel.warning)

(defun inlining-notinline-p (warning)
  "True if WARNING is about an attempt to inline a notinline function."
  #-sbcl nil
  #+sbcl
  (typep warning 'sb-c:inlining-dependency-failure))

(deftype inlining-notinline ()
  "Type of warning when trying to inline a notinline function."
  '(and warning (satisfies inlining-notinline-p)))

(defun redefined-macro-p (warning)
  "Is WARNING a redefined macro compiler warning?"
  #+sbcl (typep warning 'sb-kernel:redefinition-with-defmacro))

(deftype redefined-macro ()
  "Type of a redefined macro warning."
  '(and warning (satisfies redefined-macro-p)))

(defun redefined-function-p (warning)
  "Is WARNING a redefined function compiler warning?"
  #+sbcl (typep warning 'sb-kernel:redefinition-with-defun))

(deftype redefined-function ()
  "Type of a redefined function warning."
  '(and warning (satisfies redefined-function-p)))

(deftype optional-and-key ()
  "Type of a style warning with optional and key parameters."
  'sb-kernel:&optional-and-&key-in-lambda-list)

(defun uninteresting-condition-p (condition)
  "A test for an uninteresting CONDITION to be muffled including compiler notes.
The conditions muffled here are the minimal/uncontroversial set."
  #+sbcl
  (typep condition '(or sb-kernel:redefinition-with-defmacro
                     sb-kernel:parse-unknown-type
                     sb-kernel:uninteresting-redefinition
                     sb-int:slot-initform-type-style-warning
                     sb-ext:compiler-note
                     sb-kernel:undefined-alien-style-warning)))

(deftype uninteresting-condition ()
  "Type of the least interesting compiler warnings and notes."
  '(and condition (satisfies uninteresting-condition-p)))
