;;;
;;; Some forms with backquotes. Used to check file location parser.
;;;

'(1 2 3)

(defun ttt (a b c d)
  "A test function."
  (+ a b c d))

(defmacro macro0 (a b c d)
  `(1 ,a 2 ,b 3 ,c 4 ,d))

#|
(defmacro macro1 (a b c d)
  `(,a ,@b ,.c #(,d)))
|#

(defmacro macro2 (a b c d)
  `((1 2 3 ,a)
    (1 2 ,b 3)
    (1 ,c 2 ,d 3)))

#|
#+nil
(defmacro macro3 (a b c d)
  `((1 2 3 ,@a)
    (1 2 ,.b 3)
    (1 ,@c 2 ,.d 3)))
|#
