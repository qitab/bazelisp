;;; A basic package defining compiler warnings independent from
;;; the CL implementation. TODO(czak): Extend for other than SBCL.
;;;

(cL:defpackage #:bazel.warning
  (:use #:common-lisp)
  (:export #:style
           #:undefined-variable-p
           #:undefined-variable-warning
           #:undefined-function-p
           #:undefined-function-warning
           #:inline-used-before-definition
           #:compiler-macro-after-function-use
           #:redefined-macro
           #:redefined-function
           #:redefined-method
           #:redefined-generic
           #:redefined-package
           #:redefine-warning
           #:conflicting-ftype-declaration
           #:changed-ftype-proclamation
           #:wrong-argument-count
           #:optional-and-key
           #:deleted-code
           #:type-style
           #:type-conflict
           #:complex-lexical-environment
           #:implicit-generic
           #:uninteresting-condition
           #:show-notes
           #:show-stack-allocate-notes))

(cl:in-package #:bazel.warning)

(deftype style ()
  "A generic style warning."
  'cl:style-warning)

(defun %undefined-p (warning kind)
  "Is WARNING an undefined thing warning?
KIND maybe :FUNCTION or :VARIABLE.
Returns two values: a boolean and a name symbol of the thing."
  #+sbcl
  (when (typep warning '(and warning simple-condition))     ; not really a simple-warning
    (let ((control (simple-condition-format-control warning))
          (args (simple-condition-format-arguments warning)))
      (cond ((equal control "undefined ~(~A~): ~S")
             (and (eq (first args) kind)
                  (values t (second args))))
            ((equal control "~W more use~:P of undefined ~(~A~) ~S")
             (and (eq (second args) kind)
                  (values t (third args))))))))

(defun undefined-variable-p (warning)
  "Is WARNING an undefined variable warning?
This returns two values: a boolean and a name symbol of the variable."
  (%undefined-p warning :variable))

(deftype undefined-variable-warning ()
  "Generic type of undefined function variable."
  '(and warning (satisfies undefined-variable-p)))

(defun undefined-function-p (warning)
  "Is WARNING an undefined function warning?
This returns two values: a boolean and a name symbol of the function."
  (%undefined-p warning :function))

(deftype undefined-function-warning ()
  "Generic type of undefined function warning."
  '(and warning (satisfies undefined-function-p)))

(defun inline-used-before-definition-p (warning)
  "True if WARNING is a warning about an early use of function declared inline later."
  #+sbcl
  (when (typep warning '(and warning simple-condition))
    (let ((control (simple-condition-format-control warning)))
      (search "previously compiled. A declaration of NOTINLINE" control))))

(deftype inline-used-before-definition ()
  "Type of warning for early use of functions with inline or compiler-macro optimizations."
  '(and warning (satisfies inline-used-before-definition-p)))

(defun compiler-macro-after-function-use-p (warning)
  "True for a WARNING about a function used before the compiler-macro was defined."
  #+sbcl
  (when (typep warning '(and warning simple-condition))
    (let ((control (simple-condition-format-control warning)))
      (search "compiled before a compiler-macro was defined for it" control))))

(deftype compiler-macro-after-function-use ()
  "Type of warning for early use of functions with compiler-macros."
  '(and warning (satisfies compiler-macro-after-function-use-p)))

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

(defun changed-ftype-proclamation-p (warning)
  "True when WARNING is a warning about changed function FTYPE."
  #+sbcl
  (when (typep warning '(and warning simple-condition))
    (let ((control (simple-condition-format-control warning))
          (args (simple-condition-format-arguments warning)))
      (and (search "function" control)
           (search "clobbers" control)
           (search "proclamation" control)
           (member 'ftype args)))))

(deftype changed-ftype-proclamation ()
  "Type of a warning for a changed FTYPE proclamation."
  '(and warning (satisfies changed-ftype-proclamation-p)))

(defun conflicting-ftype-declaration-p (warning)
  "True when WARNING is a warning about a new, conflicting FTYPE declaration."
  #+sbcl
  (when (typep warning '(and warning simple-condition))
    (let ((control (simple-condition-format-control warning)))
      (and (search "The previously declared FTYPE" control)
           (search "conflicts with the definition type" control)))))

(deftype conflicting-ftype-declaration ()
  "Type of a warning for a conflicting FTYPE declaration."
  '(and warning (satisfies conflicting-ftype-declaration-p)))

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

(defun redefined-package-p (warning)
  "Is WARNING a redefined package/package variance compiler warning?"
  #+sbcl (typep warning 'sb-int:package-at-variance))

(deftype redefined-package ()
  "Type of a redefined package warning."
  '(and warning (satisfies redefined-package-p)))

(defun redefine-warning-p (warning)
  "Is WARNING a generic redefinition warning?"
  #+sbcl (typep warning 'sb-kernel:redefinition-warning))

(deftype redefine-warning ()
  "Type of a general redefinition warning."
  '(and warning (satisfies redefine-warning-p)))

(defun wrong-argument-count-p (warning)
  "True if WARNING is about a function call with a wrong number of arguments."
  #+sbcl (when (typep warning '(and warning simple-condition))
           (let ((control (simple-condition-format-control warning)))
             (and (search "The function was called with" control)
                  (search "but wants exactly" control)))))

(deftype wrong-argument-count ()
  "Type of warning about calling a function with wrong argument count."
  '(and warning (satisfies wrong-argument-count-p)))

(defun optional-and-key-p (warning)
  "Is WARNING a bad style warning about &optional and &key present in the same lambda list?"
  (when (typep warning 'simple-condition)
    (equal (simple-condition-format-control warning)
           "&OPTIONAL and &KEY found in the same lambda list: ~S")))

(deftype optional-and-key ()
  "Type of a style warning with optional and key parameters."
  '(and warning (satisfies optional-and-key-p)))

(defun deleted-code-p (warning)
  "Is WARNING a deleted/unreachable code warning?"
  #+sbcl (typep warning 'sb-c::code-deletion-note))

(deftype deleted-code ()
  "Type of a warning about deleted or unreachable code."
  '(and warning (satisfies deleted-code-p)))

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

(defun complex-lexical-environment-p (warning)
  "Is WARNING a warning about a too complex lexical environment?"
  #+sbcl (typep warning 'sb-kernel:lexical-environment-too-complex))

(deftype complex-lexical-environment ()
  "Warning about a too complex lexical environment."
  '(and warning (satisfies complex-lexical-environment-p)))

(defun implicit-generic-p (warning)
  "Is this a style WARNING about missing generic definition?"
  #+sbcl (typep warning 'sb-ext:implicit-generic-function-warning))

(deftype implicit-generic ()
  "Type of style warning about missing generic function declaration."
  '(and warning (satisfies implicit-generic-p)))

(defun show-notes (note)
  "Shows compiler NOTE that is turned off by default."
  #+sbcl
  (when (typep note 'sb-ext:compiler-note)
    :show))

(defun show-stack-allocate-notes (note)
  "Shows compiler NOTE about stack allocation failures in opt mode."
  (declare (ignorable note))
  #+(and sbcl copt)
  (when (typep note 'sb-ext:compiler-note)
    (let ((control (simple-condition-format-control note)))
      (when (and (or (search "could" control) (search "can" control))
                 (search "not stack allocate" control))
        :show))))

(defun uninteresting-condition-p (condition)
  "A test for an uninteresting CONDITION to be muffled including compiler notes.
The conditions muffled here are the minimal/uncontroversial set."
  #+sbcl
  (typep condition '(or sb-kernel:redefinition-with-defmacro
                     sb-kernel:uninteresting-redefinition
                     sb-ext:compiler-note
                     sb-kernel:undefined-alien-style-warning)))

(deftype uninteresting-condition ()
  "Type of the least interesting compiler warnings and notes."
  '(and condition (satisfies uninteresting-condition-p)))
