;; Tests the wrong argument count.
;;

(defun wrong-arg-count-fun (a b) (+ a b))

(declaim (ftype (function (number number) (values number &optional)) wrong-arg-count-fun2))
(defun wrong-arg-count-fun2 (a b) (+ a b))
