;;; Utilities for Blaze Lisp and their implementation in SBCL.
;;;

;; Default optimization settings.
; #-dbg (declaim (optimize (speed 3) (safety 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets)
  (require :sb-cltl2)
  (require :sb-introspect)
  (require :sb-md5)
  (require :sb-sprof)
  (require :sb-posix))

(defpackage #:bazel.sbcl
  (:use #:common-lisp #:sb-thread #:sb-md5 #:sb-alien #:bazel.utils)
  (:export #:exit
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
  (sb-int:info :function :inlinep function))

(defun function-has-transforms-p (function)
  "Returns non-nil if the FUNCTION has transforms."
  (or (sb-c::info :function :source-transform function)
      (let ((info (sb-c::info :function :info function)))
        (and info (sb-c::fun-info-transforms info)))))

(defun getenv (variable)
  "Returns the value of the environment VARIABLE."
  (sb-posix:getenv variable))

(defun unsetenv (variable)
  "Removes the VARIABLE from the environment."
  (sb-posix:unsetenv variable))

(defun chdir (dir)
  "Sets the process current working directory to DIR."
  (sb-posix:chdir dir))

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
  "Mute SBCL image write messages and redirect all file handles to /dev/null."
  #+unix
  ;; SBCL core dumping code is in C. There is no way to suppress the info message
  ;; written by SBCL at image dump time from Lisp. The only way to suppress the info is by using
  ;; dup2 to /dev/null on the open file descriptor handles. The handles enumerate from 0 up to
  ;; the latest open file handle which is (not by accident) the handle of the open /dev/null below.
  (with-open-file (null-fd "/dev/null" :direction :io :if-exists :append)
    (loop with null-fd = (sb-sys:fd-stream-fd null-fd)
          for fd fixnum from 0 below null-fd do
            (alien-funcall (extern-alien "dup2" (function int int int)) null-fd fd))))

(defun terminate-other-threads ()
  "Terminates all threads but the current one."
  (let ((threads (remove *current-thread* (list-all-threads))))
    (mapc #'terminate-thread threads)
    (mapc (lambda (thread) (join-thread thread :default nil)) threads)))

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

(defun save-lisp-and-die (name &key toplevel compression save-runtime-options verbose
                               precompile-generics)
  "Saves the current Lisp image and dies.
 Arguments:
  NAME - the file name to save the image.
  TOPLEVEL - the name of the toplevel function.
  COMPRESSION - indicates the compression to be used for the image.
      Will decompress in memory instead of mmapping the image.
  SAVE-RUNTIME-OPTIONS - indicates if the runtime options shall be saved to the C runtime.
      This is usually permanent.
  VERBOSE - if true, the output streams are not muted before dumping the image.
  PRECOMPILE-GENERICS - will precompile the generic functions before saving."
  (sb-ext:disable-debugger)
  (terminate-other-threads)
  (when precompile-generics
    (precompile-generic-functions :verbose bazel.log:*verbose*))
  (unless verbose (mute-output-streams))
  (sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die
   name :executable t :compression compression :toplevel toplevel
        :save-runtime-options save-runtime-options)

  (assert (not "Expected the image to survive after save-lisp-and-die."))) ; NOLINT

(defun alien-symbols ()
  "Returns all alien symbols in the current image."
  (append (sb-alien::list-dynamic-foreign-symbols)
          (sb-alien::list-undefined-foreign-symbols)))

(defun dump-alien-symbols (output-file)
  "Dumps the alien symbols found in this image to the OUTPUT-FILE."
  (with-open-file (out output-file :direction :output :if-exists :supersede)
    (format out "~{~A~%~}" (alien-symbols))))

(defun dump-extern-symbols (output-file)
  "Dumps alien symbols as extern symbols to the OUTPUT-FILE in assembler code."
  (with-open-file (out output-file :direction :output :if-exists :supersede)
    (format out ".section .note.GNU-stack,\"\",@progbits~%~
                 .section rodata~%~
                 ~{.long ~A~%~}"
            (alien-symbols))))

(defun dump-dynamic-list-lds (output-file)
  "Dumps alien symbols to be added to the dynamic table using a linker script in OUTPUT-FILE."
  (with-open-file (out output-file :direction :output :if-exists :supersede)
    (format out "{ malloc;~{~%  ~A;~}~%};" (alien-symbols))))

(defun set-interpret-mode (compile-mode)
  "Set the mode of eval to :interpret if COMPILE-MODE is :LOAD. Otherwise, set it to :COMPILE."
  (declare (optimize (speed 1) (safety 3) (compilation-speed 1) (debug 1)))
  (if (eq compile-mode :load)
      (handler-case (set 'sb-ext:*evaluator-mode* :fast-interpret)
        (type-error () (setq sb-ext:*evaluator-mode* :interpret)))
      (setf sb-ext:*evaluator-mode* :compile))
  (bazel.log:vvv "Set interpret mode to: ~A"  sb-ext:*evaluator-mode*)
  sb-ext:*evaluator-mode*)

(defun set-interactive-mode (&optional (interactive-p t))
  "If INTERACTIVE-P is true, the debugger will be enabled."
  (if interactive-p
      (sb-ext:enable-debugger)
      (sb-ext:disable-debugger)))

;;;
;;; Combining C++ run-time and Lisp image core.
;;;

(defvar *core-magic* (map 'octets #'char-code #(#\L #\C #\B #\S #\Null #\Null #\Null #\Null))
  "The magic number used to start and end an SBCL's Lisp core.")
(defconstant +magic-size+ 8 "The size of the magic number used for SBCL's Lisp core.")
(defconstant +offset-type-size+ 8 "The size of the Lisp core offset.")

(defun find-lisp-core-start (in)
  "Search for Lisp core in the IN stream. Return the offset or NIL."
  (flet ((find-magic (position)
           (let ((bytes (make-array +magic-size+ :element-type 'octet)))
             (file-position in position)
             (read-sequence bytes in)
             (let ((found (equalp bytes *core-magic*)))
               (bazel.log:verbose "~:[No m~;M~]agic found in ~S at x~X."
                                  found (enough-namestring in) position)
               found))))
    ;; 1. plain core.
    (when (find-magic 0)
      (return-from find-lisp-core-start 0))
    ;; 2. appended core.
    (unless (find-magic (- (file-length in) +magic-size+))
      (return-from find-lisp-core-start))
    ;; Magic found. Read the offset.
    (let ((offset-position (- (file-length in) +magic-size+ +offset-type-size+)))
      (file-position in offset-position)
      (let ((offset (read-u64 in)))
        (bazel.log:verbose "Read Lisp core offset x~X [at x~X]." offset offset-position)
        ;; Verify that there is the magic at the offset.
        (unless (find-magic offset)
          (return-from find-lisp-core-start))
        (bazel.log:verbose "Found Lisp core at x~X in ~S." offset (enough-namestring in))
        offset))))

(defconstant +image-alignment+ -32768 "Alignment requirement for the Lisp image.")

(defun read-run-time (run-time)
  "Read RUN-TIME part of the image. Return octets stripped of a core and aligned."
  (with-open-file (run-time run-time :direction :input :element-type 'octet)
    (let* ((end (or (find-lisp-core-start run-time) (file-length run-time)))
           (aligned-size (- end (mod end +image-alignment+)))
           (octets (make-array aligned-size :element-type 'octet)))
      (file-position run-time 0)
      (assert (= end (read-sequence octets run-time :end end))) ; NOLINT
      octets)))

(defun read-lisp-core (core)
  "Read Lisp CORE part of the image. Return the octets."
  (with-open-file (core core :direction :input :element-type 'octet)
    (let* ((start (or (find-lisp-core-start core)
                      (bazel.log:fatal "Cannot find Lisp core in ~S." (enough-namestring core))))
           (octets (make-array (- (file-length core) start) :element-type 'octet)))
      (file-position core start)
      (assert (= (length octets) (read-sequence octets core))) ; NOLINT
      octets)))

(defun combine-run-time-and-core (run-time core output)
  "Combine the C++ RUN-TIME and the Lisp CORE and save it to the OUTPUT stream."
  (declare (string run-time core output))
  ;; Lisp core image is stored with a magic number tag in the file.
  ;; If combined the magic number tag can be found at the end of the file
  ;; and preceded by the 8 byte (x64) offset into the file where the core starts.
  (let* ((octets (pmapcar #'funcall `(,#'read-run-time ,#'read-lisp-core) `(,run-time ,core)))
         (run-time (first octets))
         (core (second octets))
         (core-offset (length run-time))
         (index-offset (- (length core) +magic-size+ +offset-type-size+)))
    ;; TODO(czak): This is 64 bits.
    (declare (octets run-time core) (type (unsigned-byte 64) core-offset index-offset))
    ;; Fix core tail offset.
    (dotimes (i 8)
      (setf (aref core (+ index-offset i)) (ldb (byte 8 (* i 8)) core-offset)))
    (with-open-file (output output :direction :output :element-type 'octet)
      (write-sequence run-time output)
      (write-sequence core output))))

;;;
;;; Reading lisp files.
;;;

(defvar *in-find-package* nil "Prevents cycles in make-package")
(defconstant +find-package-function+ (symbol-function 'find-package))
(defvar *with-creating-find-package-mutex* (make-mutex :name "with-creating-find-package-mutex"))

(defun call-with-augmented-find-package (body &key (use '("COMMON-LISP")) (default nil))
  "Calls the BODY after making sure that the reader
 will not error on unknown packages or not exported symbols.
 USE is the set of packages to use by the new package."
  (declare (function body))
  (flet ((creating-find-package (name)
           (or (funcall +find-package-function+ name)
               default
               (unless *in-find-package*
                 (let ((*in-find-package* t))
                   (make-package name :use use))))))
    (declare (dynamic-extent #'creating-find-package))
    (with-recursive-lock (*with-creating-find-package-mutex*)
      #+sbcl
      (sb-ext:with-unlocked-packages ("COMMON-LISP")
        (setf (symbol-function 'find-package) #'creating-find-package))
      (unwind-protect
           (handler-bind ((package-error #'continue))
             (funcall body))
        #+sbcl
        (sb-ext:with-unlocked-packages ("COMMON-LISP")
          (setf (symbol-function 'find-package) +find-package-function+))))))

(defmacro with-creating-find-package ((&key (use '("COMMON-LISP"))) &body body)
  "Executes body in an environment where FIND-PACKAGE will not signal an unknown package error.
 Instead it will create the package with the missing name with the provided USE packages."
  `(call-with-augmented-find-package (lambda () ,@body) :use ',use))

(defmacro with-default-package ((default) &body body)
  "Executes body in an environment where FIND-PACKAGE will not signal an unknown package error.
 Instead it will return the DEFAULT package."
  `(call-with-augmented-find-package (lambda () ,@body) :default ,default))

;;;
;;; Overrides.
;;;

(in-package "SB-C")

(sb-ext:without-package-locks
  (locally (declare (sb-ext:muffle-conditions sb-kernel:redefinition-with-defun))
    (handler-bind ((sb-kernel:redefinition-with-defun #'muffle-warning))
      ;; Override this to remove traces of absolute paths in forge.
      (defun make-file-source-info (file external-format &optional form-tracking-p)
        "Override the SB-C::MAKE-FILE-SOURCE-INFO.
 FILE is the file name.
 EXTERNAL-FORMAT is the external format for the file.
 FORM-TRACKING-P is passed down in our copy'n'paste the same way SBCL does."
        (declare (pathname file))
        (truename file)
        (make-source-info
         :file-info (make-file-info
                     :name file
                     :untruename file
                     :external-format external-format
                     ;; this copy'n'paste business in QPX has to stop
                     :subforms
                     (if form-tracking-p
                                  (make-array 100 :fill-pointer 0 :adjustable t))
                     :write-date (file-write-date file)))))))
