(eval-when (:compile-toplevel) (format t "Compile: BAR~%"))


(defun bar ()
  (foo :bar))
