;; Fancy save steps
(defun remove-extra-debug-info ()
  "Removes debug info like docstrings and xrefs."
  (dolist (x (sb-vm:list-allocated-objects
              :all
              :test (lambda (x) (typep x '(or class generic-function standard-method
                                              package sb-kernel:closure
                                              sb-kernel:defstruct-description)))))
    (typecase x
      (sb-kernel:closure
       (when (documentation x 'function)
         (setf (documentation x 'function) nil)))
      (sb-kernel:defstruct-description
       (setf (sb-kernel::dd-doc x) nil))
      (t
       (when (ignore-errors (documentation x t)) ; maybe slot-unbound because ridiculous MOP
         (setf (documentation x t) nil)))))
  (sb-vm::map-allocated-objects
   (lambda (obj type size)
     (declare (ignore size))
     (when (= type sb-vm:code-header-widetag)
       (dotimes (i (sb-kernel:code-n-entries obj))
         (let ((f (sb-kernel:%code-entry-point obj i)))
           (setf (sb-kernel:%simple-fun-info f) 'function)
           ;; Preserve source forms, assuming we want them if they exist.
           (setf (sb-kernel:%simple-fun-source f)
                 (sb-kernel:%simple-fun-lexpr f))))))
   :all)
  #+nil ; Can re-enable after ace.flag library test ceases depending on docstrings
  (do-all-symbols (s)
    (when (documentation s 'variable)
      (setf (documentation s 'variable) nil)))
  (fmakunbound 'remove-extra-debug-info)) ; remove this function too!

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
  (when (sb-c::policy  sb-c::*policy* (and (= speed 3) (= debug 0)))
    (remove-extra-debug-info))
  (unintern 'save-and-exit)
  (disable-debugger)
  (when precompile-generics
    (let ((n 0))
      (dolist (p (remove-if (lambda (x) (not (eql (mismatch (package-name x) "CL-PROTOBUFS.") 13)))
                            (list-all-packages)))
        (do-symbols (s p)
          (when (and (fboundp s)
                     (sb-pcl::generic-function-p (symbol-function s))
                     (not (sb-mop:generic-function-methods (symbol-function s))))
            (fmakunbound s)
            (unintern s p)
            (incf n))))
      (when (plusp n) (format *error-output* "~&Removed ~D generic functions~%" n))) ; NOLINT
    (bazel.sbcl::precompile-generic-functions :verbose bazel.log:*verbose*)
    ;; We don't bother with ICF when we don't care about precompiled GFs.
    #+x86-64 (fold-identical-code :aggressive t)
    ;; Similarly we perform string-deduplication only when core size is a concern.
    (setf (extern-alien "gc_coalesce_string_literals" char) 2))
  ;; Not really sure what output we're trying to suppress...
  (unless verbose (bazel.sbcl:mute-output-streams))
  (let* ((is-elf-output (string= name ".o" :start1 (- (length name) 2)))
         (exec-option (cond (is-elf-output
                             (assert (not executable)) ; a relocatable '.o' file is not executable
                             :elf-object)
                            (executable t)
                            (t nil))))
    (save-lisp-and-die name :toplevel toplevel
                            :save-runtime-options (if save-runtime-options :accept-runtime-options)
                            :executable exec-option))
  (sb-int:bug "Unreachable"))
