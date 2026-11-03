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
  (:export #:style
           #:undefined-function-p
           #:undefined-function-warning
           #:inlining-notinline
           #:redefined-macro
           #:redefined-function
           #:redefined-method
           #:redefined-generic
           #:redefine-warning
           #:optional-and-key
           #:type-style
           #:type-conflict
           #:uninteresting-condition))

(cl:in-package #:bazel.warning)

(defun format-control-string-or-nil (simple-condition)
  "Return the control slot of SIMPLE-CONDITION only if it is a string."
  (let ((control (simple-condition-format-control simple-condition)))
    (typecase control
      (string control)
      #+#.(cl:if (cl:or #+sbcl (sb-c::version>= (sb-c::split-version-string
                                                 (cl:lisp-implementation-version))
                                                '(1 4 13)))
                 '(:and) '(:or))
      (sb-format::fmt-control (sb-format::fmt-control-string control))
      (t nil))))

(deftype style ()
  "A generic style warning."
  'cl:style-warning)

(defun %undefined-p (warning kind)
  "Is WARNING an undefined thing warning?
KIND maybe :FUNCTION or :VARIABLE.
Returns two values: a boolean and a name symbol of the thing."
  #+sbcl
  (when (typep warning '(and warning simple-condition))     ; not really a simple-warning
    (let ((control (format-control-string-or-nil warning))
          (args (simple-condition-format-arguments warning)))
      (cond ((search "undefined ~(~A~):" control)
             (and (eq (first args) kind)
                  (values t (second args))))
            ((equal control "~W more use~:P of undefined ~(~A~) ~S")
             (and (eq (second args) kind)
                  (values t (third args))))))))

(defun undefined-function-p (warning)
  "Is WARNING an undefined function warning?
This returns two values: a boolean and a name symbol of the function."
  (%undefined-p warning :function))

(deftype undefined-function-warning ()
  "Generic type of undefined function warning."
  '(and warning (satisfies undefined-function-p)))

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

(defun redefined-method-p (warning)
  "Is WARNING a redefined method compiler warning?"
  #+sbcl (typep warning 'sb-kernel:redefinition-with-defmethod))

(deftype redefined-method ()
  "Type of a redefined method warning."
  '(and warning (satisfies redefined-method-p)))

(defun redefined-generic-p (warning)
  "Is WARNING a redefined generic compiler warning?"
  #+sbcl (typep warning 'sb-kernel:redefinition-with-defgeneric))

(deftype redefined-generic ()
  "Type of a redefined generic warning."
  '(and warning (satisifies redefined-generic-p)))

(defun redefine-warning-p (warning)
  "Is WARNING a generic redefinition warning?"
  #+sbcl (typep warning 'sb-kernel:redefinition-warning))

(deftype redefine-warning ()
  "Type of a general redefinition warning."
  '(and warning (satisfies redefine-warning-p)))

(defun optional-and-key-p (warning)
  "Is WARNING a bad style warning about &optional and &key present in the same lambda list?"
  (when (typep warning 'simple-condition)
    (equal (format-control-string-or-nil warning)
           "&OPTIONAL and &KEY found in the same lambda list: ~S")))

(deftype optional-and-key ()
  "Type of a style warning with optional and key parameters."
  #+sbcl 'sb-kernel:&optional-and-&key-in-lambda-list
  #-sbcl '(and warning (satisfies optional-and-key-p)))

(defun type-style-warning-p (warning)
  "Is WARNING a warning about wrong argument type?"
  #+sbcl (typep warning 'sb-c::type-style-warning))

(deftype type-style ()
  "Warning about type incompatibility compile time."
  '(and warning (satisfies type-style-warning-p)))

(defun type-conflict-p (warning)
  "Is WARNING a warning about wrong argument type?"
  #+sbcl (typep warning 'sb-int::type-warning))

(deftype type-conflict ()
  "Warning about type incompatibility compile time."
  '(and warning (satisfies type-conflict-p)))

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
