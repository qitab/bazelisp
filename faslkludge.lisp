;;; Some tests utilize illegal zero-length fasls, and yet actually want to "load"
;;; them which is expected to silently succeed. That's just absurd, because
;;; if we're to fake creation of binary artifacts, why not either (1) do it right,
;;; or (2) mock out the loading of it too? Welll, I guess arguably this interceptor
;;; is doing the latter, which is certainly better than having a special-case in
;;; the general loading loop to ignore bogus fasls.
(sb-int:encapsulate 'bazel.main::load-file 'length-check
 (compile nil
  '(lambda (realfun name &rest rest)
     (unless (zerop (with-open-file (in name) (file-length in)))
       (apply realfun name rest)))))
