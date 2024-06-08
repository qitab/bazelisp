#|(in-package sb-impl)
(defun safe-gethash3 (key table default)
  (let ((layout (sb-kernel:%instance-layout table)))
    (if (/= 0 (sb-kernel:get-lisp-obj-address layout))
        (funcall (hash-table-gethash-impl table) key table default)
        (let ((s #.(format nil "table messup~%")))
          (sb-unix:unix-write 2 s 0 (length s))
          (sb-ext:search-roots (sb-ext:make-weak-pointer table) :print :verbose)
          (error "can't go on- table ~X borked"
                 (sb-kernel:get-lisp-obj-address table))))))
(compile 'safe-gethash3)
(setf (symbol-function 'gethash3) #'safe-gethash3)

;(defvar *in-convert-table* nil)
;(defun wrap-internal-make-hash-table (realfun &rest args)
;  (let ((result (apply realfun args)))
;    (when (and *in-convert-table* (neq (heap-allocated-p result) :dynamic))
;      (let ((s (format nil "table ~x not on heap: ~s~%"
;                       (sb-kernel:get-lisp-obj-address result)
;                       (sb-debug:list-backtrace :count 20))))
;        (sb-sys:with-pinned-objects (s)
;          (sb-unix:unix-write 2 s 0 (length s)))))
;    result))
;(compile 'wrap-internal-make-hash-table)
;(sb-int:encapsulate 'sb-impl::%make-hash-table 'trace #'wrap-internal-make-hash-table)

(in-package sb-pcl)
(defun wrap-convert-table (realfun a b c)
  (let ((result (funcall realfun a b c)))
    (when (and (hash-table-p result)
               (plusp (hash-table-count result))
               (neq (heap-allocated-p result) :dynamic))
      (let ((s (with-output-to-string (s)
                 (format s "converted table ~x keys" (sb-kernel:get-lisp-obj-address result))
                 (maphash (lambda (k v) v (format s " ~s" k)) result)
                 (format s "~%~a~%" (sb-debug:list-backtrace :count 20)))))
        (setq s (coerce s 'base-string))
        (sb-sys:with-pinned-objects (s)
          (sb-unix:unix-write 2 s 0 (length s)))))
    result))
(compile 'wrap-convert-table)
(sb-int:encapsulate 'convert-table 'trace #'wrap-convert-table)

(in-package "CL-USER")
|#
;; Fancy save steps
(defun save-and-exit (name &key toplevel save-runtime-options verbose
                           precompile-generics executable)
  "Saves the current Lisp image and dies.
 Arguments:
  NAME - the file name to save the image.
  TOPLEVEL - the name of the toplevel function.
  SAVE-RUNTIME-OPTIONS - indicates if the runtime options shall be saved to the C runtime.
      This is usually permanent.
  VERBOSE - if true, the output streams are not muted before dumping the image.
  PRECOMPILE-GENERICS - will precompile the generic functions before saving.
  EXECUTABLE - Whether to combine the launcher with the image to create an executable."
  (unintern 'save-and-exit)
;  (disable-debugger)
;  (when precompile-generics
;    (bazel.sbcl::precompile-generic-functions :verbose bazel.log:*verbose*))
;  (unless verbose (bazel.sbcl:mute-output-streams))
;  (fold-identical-code :aggressive t)
;  (setf (extern-alien "gc_coalesce_string_literals" char) 2)
  (save-lisp-and-die name :executable executable
                          :toplevel toplevel
                          :save-runtime-options save-runtime-options)
  (sb-int:bug "Unreachable"))
