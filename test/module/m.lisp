(in-package :cl-user)

(eval-when (:compile-toplevel)
  (format t "Compile M~%"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push 'M *compile-symbols*))

(format t "Load M~%")
(push 'M *load-symbols*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "M> c:~A~20Tl:~A~%" *compile-symbols* *load-symbols*))
