(eval-when (:compile-toplevel) (format t "Compile: BAR2~%"))


(defun bar2 ()
  (foo :bar))
