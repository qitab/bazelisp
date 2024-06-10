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
  (disable-debugger)
  (when precompile-generics
    (bazel.sbcl::precompile-generic-functions :verbose bazel.log:*verbose*))
  (unless verbose (bazel.sbcl:mute-output-streams))
  (fold-identical-code :aggressive t)
  (setf (extern-alien "gc_coalesce_string_literals" char) 2)
  (save-lisp-and-die name :executable executable
                          :toplevel toplevel
                          :save-runtime-options save-runtime-options)
  (sb-int:bug "Unreachable"))
