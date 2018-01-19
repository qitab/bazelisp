;;; Generates a stack-allocation note to test the
;;;    nowarn = ["stack-allocate-note"]
;;; parameter.
;;;

(in-package :cl-user)

(defun stack-alloc-list (size)
  (let ((l (make-list size)))
    (declare (dynamic-extent l))
    (length l)))

(defun main ()
  (format t "len: ~A~%" (stack-alloc-list 63)))
