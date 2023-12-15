;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Utilities for Bazel Lisp and their implementation in SBCL.
;;;

;; Default optimization settings.
; #-dbg (declaim (optimize (speed 3) (safety 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; MD5 pulls in SB-ROTATE-BYTE which makes it impossible
  ;; to compile either of those from fresh upstream sources without some magic.
  (require :sb-md5))

(defpackage #:bazel.sbcl
  (:use #:common-lisp #:sb-thread #:sb-alien #:bazel.utils)
  (:import-from #:sb-md5 #:md5sum-file)
  (:export #:compile-files
           #:exit
           #:run
           #:inline-function-p
           #:function-has-transforms-p
           #:getenv #:unsetenv #:chdir
           #:command-line-arguments #:program-name
           #:default-toplevel-loop
           #:mute-output-streams
           #:save-lisp-and-die
           #:dump-alien-symbols
           #:dump-extern-symbols
           #:dump-dynamic-list-lds
           #:combine-run-time-and-core
           #:md5sum-file
           #:set-interpret-mode
           #:set-interactive-mode
           #:setup-readtable
           #:remove-extra-debug-info
           #:name-closure
           #:with-creating-find-package
           #:with-default-package
           ;; threading
           #:make-thread
           #:join-thread
           #:with-recursive-lock
           #:make-mutex #:mutex
           #:pmapcar
           #:pprog1))

(in-package #:bazel.sbcl)

(defun exit (&optional (code 0))
  "Exit the process with a return CODE."
  (sb-ext:exit :code code))

(defun run (program &key args input output (error :output) dir)
  "Run a PROGRAM suppling ARGS and return the exit code.
 Arguments:
  ARGS - a list of string arguments to the program,
  INPUT - a spec for the standard input for the program,
  OUTPUT - a spec for the standard output for the program,
  ERROR - a spec for the error output for the program.
  DIR - the directory used for the program to run.

 The specification for INPUT, OUTPUT, and ERROR can be:
  NIL - the stream is mapped to /dev/null,
  T - the standard input, output, or error stream of this process is used,
  pathname - the file functions as input or output,
  stream - the stream functions as input or output,
  :OUTPUT - indicates that the error stream equals the output stream."
  (sb-ext:process-exit-code
   (sb-ext:run-program program args :input input :output output :error error :directory dir)))

(declaim (ftype (function (function &rest list) (values list &optional)) pmapcar))
(defun pmapcar (function &rest lists)
  "Takes a FUNCTION and LISTS of arguments and executes the function for each argument tuple.
 The function is run is separate threads. The first tuple of arguments is run in the current one."
  (flet ((run (args) (make-thread function :arguments args)))
    (declare (dynamic-extent #'run) (inline run))
    (let* ((args (apply #'mapcar #'list lists))
           (threads (mapcar #'run (rest args))))
      (when args
        (list* (apply function (first args)) (mapcar #'join-thread threads))))))

(defmacro pprog1 (form1 &rest forms)
  "Take each of the FORMS and run them in a separate thread. Join threads at the end.
 Returns the result of the first form. FORM1 is executed in the current thread."
  (let ((functions (gensym "F"))
        (threads (gensym "T")))
    `(let* ((,functions (list ,@(loop for form in forms collect `(lambda () ,form))))
            (,threads (mapcar #'make-thread ,functions)))
       (prog1 ,form1
         (mapc #'join-thread ,threads)))))

(defun inline-function-p (function)
  "Returns non-nil when the FUNCTION is declared inline."
  (eq (sb-int:info :function :inlinep function) 'inline))

(defun function-has-transforms-p (function)
  "Returns non-nil if the FUNCTION has transforms."
  (or (sb-c::info :function :source-transform function)
      (let ((info (sb-c::info :function :info function)))
        (and info (sb-c::fun-info-transforms info)))))

(defun getenv (variable)
  "Returns the value of the environment VARIABLE."
  (sb-ext:posix-getenv variable))

(defun unsetenv (variable)
  "Removes the VARIABLE from the environment."
  (alien-funcall
   (extern-alien "unsetenv" (function sb-alien:int sb-alien:c-string))
   variable))

(defun command-line-arguments ()
  "Returns the command-line arguments without the program name."
  (rest sb-unix::*posix-argv*))

(defun program-name ()
  "Returns the name of the program."
  (first sb-unix::*posix-argv*))

(defun default-toplevel-loop ()
  "Gives control to the default toplevel REPL."
  (sb-ext:enable-debugger)
  (sb-impl::toplevel-init))

(defun mute-output-streams ()
  "Mute SBCL image write messages."
  ;; Set runtime --noinform option to 1, which also hides the "[writing...]" noise
  (setf (extern-alien "lisp_startup_options" int) 1)
  nil)

(defun terminate-other-threads ()
  "Terminates all non-system threads but the current one."
  (let ((threads (remove-if (lambda (x)
                              (or (thread-ephemeral-p x)
                                  (eq x *current-thread*)))
                            (list-all-threads))))
    (mapc #'terminate-thread threads)
    (mapc (lambda (thread) (join-thread thread :default nil)) threads)))

(defun name-closure (closure name)
  "Return CLOSURE with the NAME changed, so it prints nicely."
  ;; This is not necessary, except for debugging and aesthetics.
  (setf (sb-kernel:%fun-name closure) name)
  closure)

(defun remove-extra-debug-info ()
  "Removes debug info like docstrings and xrefs."
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
   :all))

;;;
;;; Precompile generic functions.
;;; TODO(czak): This needs to go into SBCL upstream.
;;;
;;; For more information see:
;;; http://www.sbcl.org/sbcl-internals/Discriminating-Functions.html
;;; http://www.advogato.org/person/crhodes/diary/162.html
;;; http://www.advogato.org/person/crhodes/diary/160.html
;;; http://christophe.rhodes.io/notes/blog/posts/2014/generic_function_precompilation/


(defun eql-specializer-p (spec)
  "True if SPEC is an eql specializer."
  (typep spec 'sb-mop:eql-specializer))

(defun trivial-class-specializer-p (spec)
  "True if SPEC is a trivial class specializer."
  (or (eq spec #.(find-class t))
      (eq spec #.(find-class 'standard-object))
      (eq spec #.(find-class 'sb-pcl::slot-object))
      (eq spec #.(find-class 'sb-pcl::structure-object))))

(defun non-trivial-class-specializer-p (spec)
  "True if SPEC is non-trivial class specializer."
  (not (or (eql-specializer-p spec)
           (trivial-class-specializer-p spec))))

(defun simple-specializer-p (spec)
  "True if SPEC is not a class specializer with subclasses."
  (or (eql-specializer-p spec)
      (trivial-class-specializer-p spec)
      ;; Precompute the discriminating function only for shallow class hierarchies.
      (null (sb-mop:class-direct-subclasses spec))))

(defun gf-specializers-list (gf)
  "Returns a list of method specializers for the generic function GF."
  (let ((methods (sb-mop:generic-function-methods gf))
        (specializers-list nil))
    (dolist (method methods (nreverse specializers-list))
      (pushnew (sb-mop:method-specializers method) specializers-list :test #'equalp))))

(defun precompile-generic-function (gf &key verbose)
  "Precompiles the dispatch code for the generic function GF.
 When VERBOSE is larger than 2, print some debug info.
 Returns true when the GF has been precompiled."
  (when (sb-pcl::special-case-for-compute-discriminating-function-p gf)
    ;; TODO(czak): Those special cases are handled differently by SBCL.
    (return-from precompile-generic-function))
  (let ((methods (sb-mop:generic-function-methods gf))
        (simple-p t)
        (class-specializers-p nil)
        (eql-specializers-p nil)
        (specializers-list (gf-specializers-list gf)))
    (dolist (method methods)
      (let ((specializers (sb-mop:method-specializers method))
            (count-not-simple 0))
        (dolist (spec specializers)
          (unless (simple-specializer-p spec)
            (when (> (incf count-not-simple) 1)
              ;; If we have more than one class specializer with subclasses,
              ;; we run the danger of an exponential combination of those subclasses.
              ;; Precompilation might then explode the caches and takes time.
              (when (> verbose 2) (format t "!SIMPLE: ~S~%" gf))
              (return-from precompile-generic-function)))
          (cond ((non-trivial-class-specializer-p spec)
                 (setf class-specializers-p t)
                 ;; Finalize the inheritance of those classes.
                 ;; This is useful for accessor functions.
                 (unless (sb-mop:class-finalized-p spec)
                   (sb-mop:finalize-inheritance spec)))
                ((eql-specializer-p spec)
                 (setf eql-specializers-p t))))
        (when (plusp count-not-simple)
          (setf simple-p nil))))

    (unless simple-p
      ;; Enumerate all the subclasses for not simple specializers.
      (dolist (specializers specializers-list)
        (let ((pos (position-if-not #'simple-specializer-p specializers)))
          (when pos
            (labels ((augment (spec)
                       (dolist (sub (sb-mop:class-direct-subclasses spec))
                         (let ((new (copy-list specializers)))
                           (setf (nth pos new) sub)
                           (pushnew new specializers-list :test #'equal))
                         (augment sub))))
            (augment (nth pos specializers)))))))

    ;; Making a caching discriminating function for EQL specializers fails.
    ;; A dispatching discriminating function is expensive for class specializers.
    (when (and class-specializers-p eql-specializers-p
               (> (max (length methods) (length specializers-list)) 10))
      (when (> verbose 2) (format t "!C+E: ~S: ~D specs~%" gf (length specializers-list)))
      (return-from precompile-generic-function))

    (setf (sb-pcl::gf-precompute-dfun-and-emf-p (sb-pcl::gf-arg-info gf)) t)
    (multiple-value-bind (dfun cache info)
        (cond ((and eql-specializers-p
                    (or (cdr methods) (cdr specializers-list) (cdar specializers-list)))
               ;; Make a dispatching discriminating function.
               (when (> verbose 2) (format t "DISPATCH: ~S~%" gf))
               (sb-pcl::make-final-dispatch-dfun gf))
              (t
               ;; Make a caching discriminating function.
               ;; The MAKE-FINAL-DFUN-INTERNAL will also optimize for other cases.
               (when (> verbose 2)
                 (format t "DEFAULT: ~S: ~D specs~:[~;, EQL~]~:[~;, CLS~]~%"
                         gf (length specializers-list) eql-specializers-p class-specializers-p))
               (sb-pcl::make-final-dfun-internal gf specializers-list)))
      (sb-pcl::update-dfun gf dfun cache info))
    t))

;; This list contains packages holding symbols of generic functions which will not be precompiled.
(defvar *skip-precompile-packages* nil)

(defun precompile-generic-functions (&key (verbose 0))
  "Enumerates all generic functions and pre-compiles their dispatch functions.
 When VERBOSE is larger then 0, print some debug info.
 Returns a count of successfully precompiled dispatch functions."
  (let ((count 0)
        (all 0))
    (flet ((precompile (s)
             (let ((f (and (fboundp s) (fdefinition s))))
               (when (typep f 'standard-generic-function)
                 (incf all)
                 (when (precompile-generic-function f :verbose verbose)
                   (incf count))))))
      (do-all-symbols (s)
        (unless (find (symbol-package s) *skip-precompile-packages*)
          (when (precompile s)
            (precompile `(setf ,s)))))
      (when (plusp verbose)
        (bazel.log:info "Precompiled ~D (~D% out of ~D) generic functions.~%"
                        count (round (* 100 count) all) all))
      (values count all))))

;;;
;;; Generate an image.
;;;

(defun save-lisp-and-die (name &key toplevel save-runtime-options verbose
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
  (sb-ext:disable-debugger)
  (terminate-other-threads)
  (when precompile-generics
    (precompile-generic-functions :verbose bazel.log:*verbose*))
  (unless verbose (mute-output-streams))
  ;; (sb-ext:fold-identical-code :aggressive t)
  (setf (extern-alien "gc_coalesce_string_literals" char) 2)
  (sb-ext:save-lisp-and-die
   name
   :executable executable
   :toplevel toplevel
   :save-runtime-options save-runtime-options)

  (assert (not "Expected the image to survive after save-lisp-and-die."))) ; NOLINT

(defun set-interpret-mode (compile-mode)
  "Set the mode of eval to :interpret if COMPILE-MODE is :LOAD. Otherwise, set it to :COMPILE."
  (declare (optimize (speed 1) (safety 3) (compilation-speed 1) (debug 1)))
  (setf sb-ext:*evaluator-mode* (if (eq compile-mode :load) :interpret :compile))
  (bazel.log:vvv "Set interpret mode to: ~A"  sb-ext:*evaluator-mode*)
  sb-ext:*evaluator-mode*)

(defun set-interactive-mode (&optional (interactive-p t))
  "If INTERACTIVE-P is true, the debugger will be enabled."
  (if interactive-p
      (sb-ext:enable-debugger)
      (sb-ext:disable-debugger)))

;;;
;;; Reading lisp files.
;;;

(defun setup-readtable (rt)
  (setf (sb-ext:readtable-base-char-preference rt) :both)
  rt)

(defvar *in-find-package* nil "Prevents cycles in make-package")
(defvar *with-creating-find-package-mutex* (make-mutex :name "with-creating-find-package-mutex"))

(defun call-with-augmented-find-package (body &key (use '("COMMON-LISP")) (default nil))
  "Calls the BODY after making sure that the reader
 will not error on unknown packages or not exported symbols.
 USE is the set of packages to use by the new package.
 This affects _all_ threads' calls to FIND-PACKAGE, and
 is generally not appropriate to use in production code"
  (declare (function body))
  ;; The instant that ENCAPSULATE stores the new definition of FIND-PACKAGE, we must
  ;; accept that any thread - whether already running, or newly created - can access
  ;; our local function as a consequence of needing FIND-PACKAGE for any random reason.
  ;; Were the closure allocated on this thread's stack, then this function's frame
  ;; would be forbidden from returning until no other thread was executing the code
  ;; that was made globally visible. Since there's no way to determine when the last
  ;; execution has ended, the FLET body has indefinite, not dynamic, extent.
  (flet ((creating-find-package (f name)
           (or (funcall f name)
               default
               (unless *in-find-package*
                 (let ((*in-find-package* t))
                   (make-package name :use use))))))
    (with-recursive-lock (*with-creating-find-package-mutex*)
      (sb-int:encapsulate 'find-package 'create #'creating-find-package)
      (unwind-protect
           (handler-bind ((package-error #'continue))
             (funcall body))
        (sb-int:unencapsulate 'find-package 'create)))))

(defmacro with-creating-find-package ((&key (use '("COMMON-LISP"))) &body body)
  "Executes body in an environment where FIND-PACKAGE will not signal an unknown package error.
 Instead it will create the package with the missing name with the provided USE packages."
  `(call-with-augmented-find-package (lambda () ,@body) :use ',use))

(defmacro with-default-package ((default) &body body)
  "Executes body in an environment where FIND-PACKAGE will not signal an unknown package error.
 Instead it will return the DEFAULT package."
  `(call-with-augmented-find-package (lambda () ,@body) :default ,default))

(defun compile-files (names &rest rest)
  "Call COMPILE-FILE on NAMES, which must be singular despite being named NAMES,
passing through REST unaltered."
  (if (typep names '(or atom (cons string null)))
      (apply #'compile-file (if (atom names) names (car names)) rest)
      (error "Multiple file support is incomplete")))
