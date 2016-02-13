(in-package :cl-user)

(eval-when (:compile-toplevel)
  (format t "Compile C~%"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push 'C *compile-symbols*))

(format t "Load C~%")
(push 'C *load-symbols*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "C> c:~A~20Tl:~A~%" *compile-symbols* *load-symbols*))

(defun some-yet-undefined-function-from-c (message)
  ;; yet defined
  (format t "C> ~A~%" message))
