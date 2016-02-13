(in-package :cl-user)

(deftype octet () '(unsigned-byte 8))

'quote

'(1 2 3)

(find #\\ " /:;&^\\|?<>(){}[]$#`'\"")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *compile-symbols* nil)
  (defvar *load-symbols* nil))

(eval-when (:compile-toplevel)
  (format t "Compile A~%"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push 'A *compile-symbols*))

(format t "Load A~%")
(push 'A *load-symbols*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bar (x) x)
  (format t "A> c:~A~20Tl:~A ~%" *compile-symbols* *load-symbols*))

(defmacro foo (a b c d)
  `(list ,@a ,(bar b) ,@c `#(,,d)))

