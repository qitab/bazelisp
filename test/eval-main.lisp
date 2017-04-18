

(defun foo (&key x y)
  (declare (optimize safety))
  (* x y))
