(defmacro undefined-macro (arg)
  "A macro that takes ARG. The macro is undefined in t.lisp."
  `(oddp ,arg))
