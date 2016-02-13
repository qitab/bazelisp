(in-package :cl-user)

(eval-when (:compile-toplevel)
  (format t "Compile E~%"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push 'E *compile-symbols*))

(format t "Load E~%")
(push 'E *load-symbols*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "E> c:~A~20Tl:~A~%" *compile-symbols* *load-symbols*))

(defun test-undefined-function-2 ()
  (some-yet-undefined-function-from-c "called by E"))
