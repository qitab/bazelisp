
(defun main ()
  (format t "Compilation-mode: ~A~%" '(#+dbg :dbg #+opt :opt #-(or opt dbg) :fastbuild))
  #+sbcl (sb-ext:describe-compiler-policy))
