;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; A simple utility for blazing lisp.
;;;
;;; Command line invocation:
;;; bazel-lisp compile -v 2 -W "optional-and-key" "test.lisp" test.fasl
;;;

(defpackage #:bazel.main
  (:use #:common-lisp #+sbcl #:bazel.sbcl #:bazel.utils)
  (:import-from #:bazel.log
                #:verbose #:vv #:vvv #:*verbose*
                #:info #:message #:fatal
                #:fatal-error #:non-fatal-error
                #:with-safe-io-syntax)
  (:export #:save-image
           ;; Main entry point for bazel-lisp
           #:main
           ;; List of all files compiled into the image with src hashes.
           #:*compiled-sources*
           #:compile-source
           #:load-file ; for interactive use
           #:add-features
           #:add-feature))

(in-package #:bazel.main)

;;;
;;; BUILD-action model
;;;

(deftype compilation-mode () '(member :opt :fastbuild :dbg))
(deftype optimization-mode () '(or compilation-mode (eql :load)))

(defstruct action
  "The bazel-lisp action contains the input parameters and
  the state of the current BUILD action."
  (command nil :type keyword)
  ;; The arguments passed to the program.
  (args nil :type list)
  ;; The first of the output files.
  (output-files nil :type list)
  ;; The root directory for generated files.
  (bindir nil :type (or null string))
  ;; Flag indicating that the dependencies have been processed
  ;; and the outstanding files are sources for this BUILD action.
  (processing-sources-p nil :type boolean)
  ;; A list of source files to be compiled.
  (source-files nil :type list)
  ;; Flag indicating that the compilation should commence even with errors.
  (force-compilation-p nil :type boolean)
  ;; Used to precompile the generic functions.
  (precompile-generics-p nil :type boolean)
  ;; Flag indicating that the final binary should have its runtime options burned.
  ;; Value T will prevent such target binary from interpreting those options from the command line.
  (save-runtime-options-p nil :type boolean)
  ;; The main function for a binary.
  (main-function nil :type (or null symbol string))
  ;; Whether block compilation is enabled.
  (block-compile-p nil :type boolean)
  ;; Whether to only consider combining top-level forms into a block when those are between explicit
  ;; (START-BLOCK) and (STOP-BLOCK) annotations.
  (block-compile-specified-only nil :type boolean)
  ;; A list warning handlers.
  (warning-handlers nil :type list)
  ;; The compile mode. One of :dbg, :opt, or :fastbuild.
  (compilation-mode nil :type compilation-mode)
  ;; A list of failures.
  (failures nil :type list)
  ;; A count of muffled infos.
  (muffled-infos-count 0 :type fixnum)
  ;; A count of muffled warnings.
  (muffled-warnings-count 0 :type fixnum))

(defmethod cl:print-object ((action action) stream)
  "Prints the FASL file object as an unreadable object."
  (print-unreadable-object (action stream :type t)
    (format stream ":command ~S :outputs ~D~@[ :main ~S~] :compilation-mode ~S ~
                    :failures ~D :muffled ~D :infos ~D"
            (action-command action)
            (length (action-output-files action))
            (action-main-function action)
            (action-compilation-mode action)
            (length (action-failures action))
            (action-muffled-warnings-count action)
            (action-muffled-infos-count action))))

(declaim (type (or null action) *action*))
;; All of the state of the current bazel-lisp BUILD action.
;; The action is shared among threads.
(defvar *action* nil)

;; The current file being processed.
(declaim (type (or null string) *current-source-file*))
;; TODO(dougk): This doesn't provide a way to distinguish which file is associated with errors
;; raised when compile-files is processing multiple srcs at once.
(defvar *current-source-file* nil
  "Contains the name of the currently processed file. Used by error reporting.")

;; The set of compiled sources with their md5 checksums.
;; Constructed on demand
(define-symbol-macro *compiled-sources* (get-or-make-md5sum-table))
(defvar %namestring-to-md5sum nil) ; NOLINT
(defun get-or-make-md5sum-table ()
  "Construct hash-table from debug-infos"
  (or %namestring-to-md5sum
      #-sbcl (error "Implement")
      #+sbcl
      (let ((ht (make-hash-table :test 'equal)))
        (dolist (c (sb-vm:list-allocated-objects
                    :all :type sb-vm:code-header-widetag)
                   (setf %namestring-to-md5sum ht))
          (when (typep (sb-kernel:%code-debug-info c) 'sb-c::debug-info)
            (let* ((di (sb-kernel:%code-debug-info c))
                   (src (sb-c::debug-info-source di)))
              (unless (typep src 'sb-c::core-debug-source)
                (let ((md5sum (getf (sb-c::debug-source-plist src) :md5sum)))
                  (when md5sum
                    (setf (gethash (sb-c::debug-source-namestring src) ht)
                          md5sum))))))))))

(defun action-add-failure (warning &optional (action *action*))
  "Add a WARNING to the failures list of the ACTION."
  (verbose "Added failure: ~S '~A'" (type-of warning) warning)
  (pushnew (list *current-source-file* (type-of warning) warning)
           (action-failures action) :test #'equalp))

;;;
;;; Functions dealing with compiler warnings and deferred warnings.
;;; This requires the bazel.warning package.
;;;

(defun resolve-warning-handler (handler &key (fail-on-error t))
  "Tries to resolve the warning HANDLER in the bazel.warning package.
Signals a fatal error if FAIL-ON-ERROR is true and HANDLER is not resolved.
Returns a function or string if not resolved."
  (etypecase handler
    (function handler)
    (symbol
     (cond ((null handler)
            (when fail-on-error
              (fatal "Cannot resolve NIL warning handler.")))
           ((handler-case (subtypep handler 'condition) (t nil))
            (let ((closure (lambda (condition) (typep condition handler))))
              ;; Name it so it prints nicely.
              (setf closure (name-closure closure handler))
              (vv "Resolving handler ~S to ~S." handler closure)
              closure))
           ((fboundp handler)
            ;; Return the handler function.
            (symbol-function handler))
           (handler
            ;; Return the symbol.
            handler)))
    (string
     (or (with-standard-io-syntax
           (let ((*package* (find-package "BAZEL.WARNING")))
             (multiple-value-bind (%handler %error)
                 (ignore-errors (read-from-string handler))
               (vv "Read ~S from ~S" %handler handler)
               (etypecase %handler
                 (null
                  (when fail-on-error
                    (fatal
                     "The warning handler ~S resolved to NIL~@[ [~S:~]~@[~A]~]."
                     handler (and %error (type-of %error)) %error))
                  handler)
                 (symbol (resolve-warning-handler %handler))
                 (function %handler)))))
         handler))))

(defun action-add-nowarn (nowarn &optional (action *action*))
  "Add a NOWARN condition/handler at the end of the nowarn list of the ACTION."
  (declare (type action action) (type (or string symbol function) nowarn))
  (nconcf (action-warning-handlers action)
          ;; Since this is done initially, we may not have all handlers loaded.
          (list (resolve-warning-handler nowarn :fail-on-error nil))))

(defun invoke-warning-handlers (handlers condition)
  "The function invokes all the HANDLERS on the CONDITION until first returns true.
If a handler is specified as a string, it will be resolved in the bazel.warning
package context. This allows for the user to specify their own handlers as a string."
  (declare (list handlers) (condition condition))
  (message :info (if (typep condition 'warning) 2 3)
           "Invoking ~D handler~:P on: ~S (~A)"
           (length handlers) (type-of condition) condition)
  (loop with restart = (find-restart 'muffle-warning)
        with result = (if restart :fail :ignore)
        for handler.rest on handlers
        for handler-designator = (car handler.rest)
        for handler = (if (functionp handler-designator)
                          handler-designator
                          (setf (car handler.rest)
                                (resolve-warning-handler handler-designator)))
        for unresolved-p = (not (functionp handler))
        when (and unresolved-p restart)
          do (fatal "Given condition: ~S (~A)~%; Cannot resolve handler: ~S"
                    (type-of condition) condition handler-designator)
        thereis
        (let ((value (unless unresolved-p
                       (funcall (the function handler) condition))))
          (vvv "Handler ~A => ~A" handler value)
          (case value
            ((nil) nil)
            ((:fail) (return :fail))
            (t       (when restart (return :muffle)))))
        finally (return result)))

(defun handle-warning (warning &optional (action *action*))
  "Invoke the WARNING handlers and adds a failure to the ACTION failure list."
  (unless *current-source-file* (return-from handle-warning nil))
  (let ((result (invoke-warning-handlers (action-warning-handlers action) warning))
        (warning-p (typep warning 'warning)))
    (ecase result
      (:ignore
       (bazel.log:vvv "IGNORE: ~S '~A'" (type-of warning) warning))
      (:muffle
       (bazel.log:vv "MUFFLE: ~S '~A'" (type-of warning) warning)
       (if warning-p
           (incf (action-muffled-warnings-count action))
           (incf (action-muffled-infos-count action)))
       (muffle-warning warning))
      (:fail
       (bazel.log:error "FAIL: ~S '~A'" (type-of warning) warning)
       (action-add-failure warning action)))))

(defun handle-error (error)
  "Print an info about the ERROR context."
  (bazel.log:error "~S while processing: ~S '~A'" (type-of error) *current-source-file* error))

(defun muffle-all-warnings (condition &optional (action *action*))
  "Muffle all warnings for the CONDITION. The ACTION muffled counters are incremented."
  (let ((restart (find-restart 'muffle-warning condition))
        (warning-p (typep condition 'warning)))
    (if warning-p
        (incf (action-muffled-warnings-count action))
        (incf (action-muffled-infos-count action)))
    (message :info (if warning-p 2 3) "Muffled: ~S '~A'" (type-of condition) condition)
    (when restart (invoke-restart restart))))

(defmacro with-all-warnings-muffled (&body body)
  "Macro that muffles all warnings generated by the BODY."
  `(handler-bind ((condition #'muffle-all-warnings))
     ,@body))

(defun print-warning-conditions (header conditions &optional bindir)
  "Outputs a list of CONDITIONS to the *error-output* output stream.
 BINDIR is the directory for output files, that is stripped off when
   printing the CONDITIONS.
 HEADER is a prefix printed before all CONDITIONS."
  (when conditions
    (bazel.log:warning
     "~A:~{~@[~&~3T~A:~]~&~6T ~S '~A'~}" header
     (loop for prev-src = nil then src
           for (src type condition) in conditions
           nconc (list
                  (unless (equal src prev-src) (strip-prefix bindir src))
                  type
                  (with-safe-io-syntax
                    (ignore-errors (format nil "~A" condition))))))))

(defun check-failures (action)
  "Checks for compilation failures stored in ACTION."
  (message :info (if (action-failures action) 0 1)
           "Muffled ~D warning~:P and ~D info~:P (set verbose to 2 or 3 to see them)"
           (action-muffled-warnings-count action)
           (action-muffled-infos-count action))

  (when (action-failures action)
    ;; Terminate with error. Bazel will clean up for us.
    (print-warning-conditions "Failures" (action-failures action) (action-bindir action))
    (unless (action-force-compilation-p action)
      (fatal "Bazel lisp build failed"))))

;;;
;;; Bazel-Lisp specific utilities
;;;

(declaim (type (or symbol function) *entry-point*))
(defvar *entry-point* nil)

(defun restart-image ()
  "Restart function that is called when the image is executed next time.
Calls toplevel-init if no *entry-point* or calls the function specified in LISP_MAIN.
If LISP_MAIN is NIL or T it will call top-level REPL as well."

  (let ((entry-point *entry-point*)
        (LISP_MAIN (getenv "LISP_MAIN")))

    (when LISP_MAIN
      (unsetenv "LISP_MAIN")
      (handler-case
          (setf entry-point (read-from-string LISP_MAIN))
        (error (e)
          (bazel.log:warning "Could not parse $LISP_MAIN: ~S~%  ~S:~A"
                             LISP_MAIN (type-of e) e))))

    (case entry-point
      ((t nil) (setf entry-point #'default-toplevel-loop)))

    (unless (or (functionp entry-point)
                (ignore-errors (fdefinition entry-point)))
      (bazel.log:warning "Could not find function: `~S`" entry-point)
      (setf entry-point #'default-toplevel-loop))

    (funcall entry-point)))

(defun save-image (name main &key save-runtime-options precompile-generics
                        executable)
  "Saves the image to a binary image named 'name'. Exits.
 Arguments:
  NAME - the file name to save the image.
  MAIN - the name of the toplevel function.
      Will decompress in memory instead of mmapping the image.
  SAVE-RUNTIME-OPTIONS - indicates if the runtime options shall be saved to the C runtime.
      This is usually permanent.
  PRECOMPILE-GENERICS - will precompile the generic functions before saving.
  EXECUTABLE - Whether to combine the launcher with the image to create an executable."
  (let ((main-fn (or (if (stringp main)
                         (with-standard-io-syntax (read-from-string main))
                         ;; what else could it be but a string?
                         main)
                     'sb-impl::toplevel-init)))
    (etypecase main-fn
      (symbol
       (unless (fboundp main-fn)
         (fatal "~S is not a known function name." main-fn))))
    (verbose "Saving binary to: ~S~@[ (old-main: ~S)~]~@[ (main: ~S)~]"
             name (unless (eq main-fn *entry-point*) *entry-point*) main-fn)
    (setf *entry-point* main-fn))
  ;; Set to a sane value.
  (in-package "COMMON-LISP-USER")
  (let ((script "third_party/lisp/bazel/imagesave.lisp"))
    (when (probe-file script)
      (load script :verbose nil :print nil)
      (funcall (intern "SAVE-AND-EXIT")
               name
               :toplevel #'restart-image
               :save-runtime-options save-runtime-options
               :precompile-generics precompile-generics
               :executable executable
               :verbose (plusp *verbose*))))
  (sb-ext:save-lisp-and-die name :toplevel #'restart-image :executable t))

(defun set-optimization-mode (optimization-mode)
  "Proclaim the optimization settings based on the OPTIMIZATION-MODE."
  (declare (type optimization-mode optimization-mode))

  (vvv "Set optimization mode: ~S" optimization-mode)

  (destructuring-bind (spEed Debug saFety space Compilation-speed)
      (ecase optimization-mode ; E D F   C
        (:load                '(1 1 1 1 3))
        ((:fastbuild nil)     '(1 #+arm64 1 #-arm64 2 ; arm64 has compiler bugs in debug 2
                                    3 1 1))
        (:opt                 '(3 0 0 1 1))
        (:dbg                 '(1 3 3 1 1)))

    (set-interpret-mode optimization-mode)

    ;; Cause bodies of macroexpanders, including MACROLET and DEFINE-COMPILER-MACRO,
    ;; to be compiled in a policy in which these qualities override the global policy.
    #+sbcl (sb-ext:set-macro-policy '((speed 0) (safety 3)))

    (proclaim `(optimize (speed ,speed) (debug ,debug) (safety ,safety)
                         (space ,space) (compilation-speed ,compilation-speed)
                         ;; always insert array bounds, even in otherwise optimized code;
                         ;; optimizing this out was measured not to be worth the trouble.
                         #+sbcl(sb-c::insert-array-bounds-checks 3)))))

(defun to-feature (feature)
  "Intern FEATURE in the keyword package if a string, or return as-is if a symbol"
  (etypecase feature
    (symbol (the (not null) feature))
    (string
     (assert (not (find #\: feature)))
     (intern (string-upcase feature) "KEYWORD"))))

(defun add-feature (feature)
  "Add a single string FEATURE to *features*."
  (pushnew (to-feature feature) *features*))

(defun add-features (string)
  "Add the features from the STRING first converting them into keywords."
  (let ((new-features (set-difference (mapcar #'to-feature (split string)) *features*)))
    (vv "Adding features: ~S" new-features)
    (mapcar #'add-feature new-features)))

(defun check-features ()
  "Checks that build features are in good shape."
  (assert (not (and (member :opt *features*) (member :dbg *features*))))) ; NOLINT

(defun add-default-features (compilation-mode)
  "Add the default features to *features* including :bazel and COMPILATION-MODE.
 SAFETY level is used to determine if :OPT should be added."
  (declare (type (member :opt :fastbuild :dbg) compilation-mode))

  (add-feature :bazel)

  (case compilation-mode
    (:dbg (add-feature :dbg))
    (:opt (add-feature :opt)))

  (check-features))

(defun load-file (name &key
                       fasl
                       (action *action*)
                       (load-mode (action-compilation-mode action))
                       (muffle-warnings (not (action-processing-sources-p action))))
  "Loads a file with NAME using action-compilation-mode.
 Checks for duplications and marks file as loaded. The warnings are muffled for dependencies.
 Arguments:
  NAME - the name of the file to load,
  FASL - if non-nil, the FASL will be loaded in place of the Lisp file.
  ACTION - the current bazel action object,
  LOAD-MODE - the load mode used to load the file.
  MUFFLE-WARNINGS - if true, as in the case of deps, no warnings will be printed."
  (declare (type (or string pathname) name) (type action action))
  (unless load-mode
    (return-from load-file))
  (with-safe-io-syntax
    (handler-bind ((non-fatal-error #'handle-error))
      (with-compilation-unit (:source-namestring name)
        (let* ((name (namestring name))
               (*default-pathname-defaults* *default-pathname-defaults*)
               (*current-source-file* name)
               (*action* action))
          (set-optimization-mode load-mode)
          (cond (muffle-warnings
                 (with-all-warnings-muffled
                   (handler-bind (((or bazel.warning:redefined-function
                                       bazel.warning:redefined-macro)
                                   #'handle-warning))
                     (load (or fasl name) :external-format :utf-8))))
                (t
                 (load (or fasl name) :external-format :utf-8))))))))

;;;
;;; Main compile/build loop
;;;

(defun %compile-sources (srcs output-file &key
                                          block-compile)
  "Compiles the list of SRCS files into the OUTPUT-FILE. A corresponding FASL will be created.
 Returns (values FASL WARNINGS-P FAILURES-P).
 Parameters:
  BLOCK-COMPILE is whether to block compile, and can be either T or :SPECIFIED.
  ENTRY-POINTS is a list of entry points which are used when block-compiling."
  (multiple-value-bind (fasl warnings-p failures-p)
    (with-compilation-unit (:source-plist `(:md5sum ,(md5sum-file (car srcs)))
                            :source-namestring (car srcs))
      (with-safe-io-syntax
          (let ((*default-pathname-defaults* *default-pathname-defaults*))
            (cond ((eq output-file :anonymous)
                   (assert (not (cdr srcs)))
                   (sb-c:compile-file-to-tempfile (car srcs)
                                                  :external-format :utf-8
                                                  :block-compile block-compile))
                  (t
                   (verbose "~{~A ~} => ~S (~A)" srcs (namestring output-file)
                            *default-pathname-defaults*)
                   (ensure-directories-exist output-file)
                   (let ((output-file (merge-pathnames output-file)))
                     (ignore-errors (delete-file output-file))
                     (compile-files srcs :output-file output-file
                                         :external-format :utf-8
                                         :block-compile block-compile)))))))
    (unless (and warnings-p failures-p)
      (vv "Files ~A compiled without warnings." srcs))
    (when warnings-p
      (verbose "Files ~A compiled with warnings." srcs))
    (with-simple-restart (continue "Ignore compilation failure for ~A and continue." srcs)
      (when failures-p
        (fatal "Files ~A failed to compile." srcs)))
    (values fasl warnings-p failures-p)))

(defun compile-source (src output-file &rest key-args &key block-compile)
  "Compiles the SRC file into the OUTPUT-FILE. A corresponding FASL will be created.
 Returns (values FASL WARNINGS-P FAILURES-P).
 Parameters:
  BLOCK-COMPILE is whether to block compile, and can be either T or :SPECIFIED."
  (declare (ignore block-compile))
  (apply #'%compile-sources (list src) output-file key-args))

(defun defer-undefined-warning (warning)
  "Return true if WARNING is an undefined function warning and is therefore ignorable."
  ;; Separately compiled units see hundreds if not thousands of this kind of warning.
  ;; Missing functions become relevant (and are detected) only when producing a core file.
  (values (bazel.warning:undefined-function-p warning)))

;;;
;;; File handlers
;;;

(defun process-file (file &aux (action *action*) (type (pathname-type file)))
  "Process FILE"
  (let ((*current-source-file* file))
    (vvv "~:[dep~;src~]: ~S" (action-processing-sources-p action) file)
    (cond
      ((or (string= type "lisp") (string= type "lsp"))
       (unless (and (action-processing-sources-p action)
                    (eq (action-command action) :compile))
         (load-file file :action action :load-mode :load)))
      ((string= type "fasl")
       (load-file file :fasl file :action action
                       :load-mode (action-compilation-mode action)))
      (t
       ;; Maybe this should error instead of skip, but it's possible for files to be included in
       ;; the build command-line just to forward those to things analyzing the compilation with
       ;; extra actions (i.e. .meta files forwarded to the Kythe indexer):
       ;; https://docs.bazel.build/versions/master/be/extra-actions.html
       (verbose "File skipped: ~S [~A]" file type)))))

(defun process-dependencies (deps collect-undefs)
  "Iterates through the DEPS dependencies and invokes process-file on the DEPS.
if COLLECT-UNDEFS then return the unresolved function references"
  (verbose "Processing ~D dependencie~:P..." (length deps))
  (let ((undefs (make-hash-table :test 'equal))
        (saved-hook sb-int:*setf-fdefinition-hook*))
    ;; Technically we only want to observe the calls to ENSURE-LINKAGE-INDEX that are
    ;; a consequence of the FASL asking for such via APPLY-FASL-FIXUPS, but I can't
    ;; see any way to hit this interceptor other than via the fasloader, except possibly
    ;; a COMPILE action in a different thread. We can ignore that little glitch
    ;; since there can't be another thread.
    (when collect-undefs
      #+x86-64
      (sb-int:encapsulate 'sb-int:ensure-linkage-index 'intercept
        (lambda (realfun fname &optional quiet)
          (when (and (not quiet)
                     (boundp 'sb-fasl::*current-fasl-group*))
            (cond ((and (fboundp fname)
                        ;; If FNAME is FBOUNDP to a function that is not inlineable,
                        ;; there's nothing further to do with it.
                        (not (inline-function-p fname))
                        (or (listp fname)
                            (not (macro-function fname)))))
                  (t
                   ;; Record each source file that referenced the potentially-undefined name
                   (let ((source-file (sb-fasl::fasl-group-header-label
                                       sb-fasl::*current-fasl-group*)))
                     (pushnew source-file (gethash fname undefs))))))
          (funcall realfun fname quiet))))
    (unwind-protect
         (with-all-warnings-muffled
             (when collect-undefs
               (push (lambda (fname defn)
                       (declare (ignore defn))
                       (unless (inline-function-p fname)
                         (remhash fname undefs)))
                     sb-int:*setf-fdefinition-hook*))
           (with-compilation-unit ()
             (map nil #'process-file deps)))
      (untrace)
      (when collect-undefs
        (setf sb-int:*setf-fdefinition-hook* saved-hook)
        #+x86-64
        (sb-int:unencapsulate 'sb-int:ensure-linkage-index 'intercept)))
    undefs))

;;;
;;; Command handlers
;;;

(defun check-and-save-image (action command)
  "Save the binary from this image."
  (check-failures action)
  (check-features)

  ;; Assure things are in a defined state.
  ;; Save image. Exit.
  (save-image (first (action-output-files action))
              (action-main-function action)
              :save-runtime-options (action-save-runtime-options-p action)
              :precompile-generics (action-precompile-generics-p action)
              :executable (eq command :binary)))

(defun finish-action (action command) "Finish ACTION + COMMAND"
  (ecase command
    ((:binary :core) ; executable core, nonexecutable core respectively
     (check-and-save-image action command))
    (:compile ; "finishing" a compilation means calling COMPILE-FILE
     ;; Currently SBCL is not binding *compile-file-pathname* when raising undefined-function.
     ;; So handle this in at least the one-file case.
     (let* ((srcs (action-source-files action))
            (out (first (action-output-files action)))
            (*current-source-file*
             (unless (rest srcs)
               (first srcs))))
       (assert (string= (pathname-type out) "fasl"))
       (%compile-sources srcs out
                      :block-compile (if (and (action-block-compile-p action)
                                              (action-block-compile-specified-only action))
                                         :specified
                                         (action-block-compile-p action)))
       (check-failures action)))))

(defun parse-specs (specs)
  "Parse the SPECS file and return values for SRCS, DEPS, LOAD."
  (let (srcs deps load)
    (with-open-file (in specs :direction :input :element-type 'character)
      (loop for spec = (read in nil in)
            until (eq spec in)
            do
         (ecase (first spec)
           (:srcs (setf srcs (rest spec)))
           (:deps (setf deps (rest spec)))
           (:load (setf load (rest spec))))))
    (values srcs deps load)))

(defun coverage-exclude-p (src)
  "Return T if SRC should never be coverage-instrumented"
  (let ((dir (pathname-directory src)))
    (when (or (eql (mismatch src "third_party/lisp/") 17)
              ;; Don't instrument generated sources
              ;;   (:relative "bazel-out" ignore_this {genfiles|bin})
              ;; where ignore_this is probably "k8-opt" but doesn't matter.
              (and (eq (first dir) :relative)
                   (string= (second dir) "bazel-out")
                   (stringp (fourth dir))
                   (find (fourth dir) '("bin" "genfiles") :test 'string=)))
      t)))

;;;
;;; Main Processing Loop
;;;

(defun process (command &rest args
                   &key deps load srcs outs bindir
                   specs
                   (compilation-mode :fastbuild)
                   block-compile
                   block-compile-specified-only
                   force
                   main features nowarn
                   precompile-generics
                   save-runtime-options
                   coverage
                   verbose)
  "Main processing function for bazel.main. The keyword arguments of this function are flags for
the compilation image.
 Arguments:
  ARGS - all the arguments
  COMMAND - one of :core, :binary, or :compile
  DEPS - dependencies,
  LOAD - files to be loaded after dependencies.
  SRCS - sources for a binary core or for compilation,
  OUTS - the output files,
  BINDIR - the directory for the output files (for debug),
  COMPILATION-MODE - from bazel -c <compilation-mode>
  BLOCK-COMPILE - Whether to enable block compilation.
  BLOCK-COMPILE-SPECIFIED-ONLY - Whether to only combine top-level-forms into a block within
    explicit (START-BLOCK) and (END-BLOCK), otherwise considering each top-level-form individually.
    If not specified, all forms are combined into a single block.
  FORCE - if true, the compilation may run to completion even with errors.
  MAIN - the name of the main function for a binary,
  FEATURES - features to be set before reading sources,
  NOWARN - list of warnings to be muffled,
  PRECOMPILE-GENERICS - if non-nil, precompile-generics before saving core,
  SAVE-RUNTIME-OPTIONS - will save the runtime options for the C runtime.
  COVERAGE - if the results should be instrumented with coverage information.
  VERBOSE - Verbosity level from 0 to 3."
  (declare (ignore verbose))  ; handled in execute-command
  (multiple-value-setq (srcs deps load)
    (if specs
        (parse-specs specs)
        (values (split srcs)
                (split deps)
                (split load))))

  (let* ((command (to-keyword command))
         (outs (split outs))
         (compilation-mode (to-keyword compilation-mode))
         (action
           (make-action :args args
                        :command command
                        :source-files srcs
                        :output-files outs
                        :bindir bindir
                        :compilation-mode compilation-mode
                        :main-function main
                        :force-compilation-p force
                        :precompile-generics-p precompile-generics
                        :save-runtime-options-p save-runtime-options
                        :block-compile-p block-compile
                        :block-compile-specified-only block-compile-specified-only))

         (*compile-verbose* (>= *verbose* 1))
         (*compile-print* (>= *verbose* 3))
         (*load-verbose* (>= *verbose* 2))
         (*load-print* (>= *verbose* 3)))

    (declare (list deps srcs outs))

    ;; Rebind globally.
    (setf *action* action)

    (unless outs
      (fatal "Missing output file. Called with:~%~{~12T~A: ~A~%~}" args))

    (add-features features)
    (add-default-features compilation-mode)

    (mapc (lambda (nowarn) (action-add-nowarn nowarn action)) (split nowarn))

    ;; All notes are discarded here.
    (action-add-nowarn 'bazel.warning:uninteresting-condition)
    (action-add-nowarn #'defer-undefined-warning)

    #+sbcl
    (when (and coverage (not (and (sb-int:singleton-p srcs)
                                  (coverage-exclude-p (car srcs)))))
      (bazel.log:verbose "Turning on coverage-instrumented code generation.")
      (proclaim '(optimize (sb-c:store-coverage-data 3))))

    ;; :core is a nonexecutable core, :binary prepends the SBCL C runtime
    (let ((undefs (process-dependencies deps (case command ((:binary :core) t)))))
      (when (and undefs (plusp (hash-table-count undefs)))
        ;; We want to detect these situations:
        ;;  - name got defined but is a macro
        ;;  - name got defined but is an inline function
        ;;  - name was never defined
        ;; A few things are tricky about getting the errors entirely right,
        ;; and we can't afford false positives because they will spuriously break
        ;; the build. Better to have false negatives.
        ;; - Inline functions with a locally NOTINLINE should reference the global name.
        ;; - Reference to #'NAME is also usually ok.
        ;; - Compiler-macros may decline to expand.
        (let ((pivot (make-hash-table))) ; pathnames can be compared by EQ
          ;; Take the mapping from function name to list of pathnames mentioning it and
          ;; pivot it to a mapping from source file to list of functions it refers to.
          (maphash (lambda (fname pathnames)
                     (when t #+nil (or (not (fboundp fname))
                                       (and (symbolp fname) (macro-function fname)))
                       (dolist (pathname pathnames)
                         (pushnew fname (gethash pathname pivot)))))
                   undefs)
          (maphash (lambda (pathname fnames)
                     (message :error 0 "~A has linkage errors:~:{~% ~S - missing ~A~}"
                              (namestring pathname)
                              (mapcar (lambda (fname)
                                        (list fname
                                              (cond ((and (symbolp fname)
                                                          (macro-function fname))
                                                     "macro definition")
                                                    ((inline-function-p fname)
                                                     "inline definition")
                                                    (t
                                                     "definition"))))
                                      fnames)))
                   pivot)
          (fatal "Build failed"))))

    (handler-bind ((condition #'handle-warning)
                   (non-fatal-error #'handle-error))
      (verbose "Loading ~D source file~:P..." (length load))
      (mapc #'process-file load)

      ;; Switch to source file processing.
      (setf (action-processing-sources-p action) t)
      (verbose "Processing ~D source file~:P..." (length srcs))
      (mapc #'process-file srcs)

      (verbose "Finalizing the ~A action..." command)
      (set-optimization-mode (action-compilation-mode action))
      (finish-action action command))))

;;;
;;; Main entry point
;;;

(defun to-keyword-arg (thing)
  "Converts a command line argument option name to a keyword."
  (and thing (to-keyword
              (cond ((prefixp "--" thing)
                     (subseq thing 2))
                    ((prefixp "-" thing)
                     (subseq thing 1))
                    (t
                     thing)))))

(defun parse-rest-command-args (args)
  "Parses the remaining command-line ARGS."
  (loop while args
        for arg = (to-keyword-arg (pop args))
        when arg
          nconc (list arg (or (null args)
                              (prefixp "-" (car args))
                              (pop args)))))

(defun parse-command-args (args)
  "Parses the command-line and returns ARGS as list of keyword value pairs."
  (list* (to-keyword (first args)) (parse-rest-command-args (rest args))))

(defun execute-command (command &rest args &key verbose &allow-other-keys) ; NOLINT
  ;; Process some meta-level options.
  (when verbose (setf *verbose* (read-from-string verbose)))

  (verbose "Program name: ~A" (program-name))
  (vv "Command line: ~{'~A'~^ ~}" (command-line-arguments))
  (verbose "Current dir: ~A" *default-pathname-defaults*)

  ;; core saving seems extremely shaky as of late, failing in either of the follows ways:
  ;;
  ;; 1) SB-SYS:MEMORY-FAULT-ERROR: Unhandled memory fault at #x59. while executing: CORE
  ;; 2) pre-GC failure
  ;; Ptr 0x1200c312c7 @ b8009e6020 (lispobj b8009e600f,pg-1,h=70e0cb5835) sees junk
  ;; fatal error encountered in SBCL pid 7999 tid 7999:
  ;; Verify failed: 1 errors
  ;; 3: fp=0x7f5d3ea07590 pc=0x55db1677ec64 Foreign function (null)
  ;; 4: fp=0x7f5d3ea07620 pc=0x55db1675ccef Foreign function hexdump_and_verify_heap
  ;; 5: fp=0x7f5d3ea076c0 pc=0x55db1677c8bf Foreign function collect_garbage
  ;; 6: fp=0x7f5d3ea07730 pc=0x55db1674e78c Foreign function gc_and_save
  ;;
  ;; Maybe we can get a little more information by enabling GC debugging here.
  (when (member command '(:binary :core))
    (setf (sb-alien:extern-alien "pre_verify_gen_0" sb-alien:int) 0)
    (setf (sb-alien:extern-alien "verify_gens" sb-alien:char) 0))

  (handler-bind ((error (lambda (e)
                          (format *error-output*
                                  "~&~S: ~A while executing: ~A~%"
                                  (type-of e) e command)
                          (unless verbose
                            (exit 1)))))
    (prog1 (apply #'process command args)
      (verbose "BAZEL ~A finished" command))))

(defun main ()
  "Main entry point."
  (when (zerop (sb-alien:alien-funcall
                (sb-alien:extern-alien "isatty" (function sb-alien:int sb-alien:int)) 0))
    (sb-ext:disable-debugger))
  (apply #'execute-command (parse-command-args (command-line-arguments))))
