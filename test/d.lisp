(in-package :cl-user)

(eval-when (:compile-toplevel)
  (format t "Compile D~%"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push 'D *compile-symbols*))

(format t "Load D~%")
(push 'D *load-symbols*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "D> c:~A~20Tl:~A~%" *compile-symbols* *load-symbols*))
