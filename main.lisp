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

;; Default compilation settings for bazel-lisp.
#-dbg (declaim (optimize (speed 3) (safety 1)))

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
           ;; Splits a string by space.
           ;; List of all files compiled into the image with src hashes.
           #:*compiled-sources*
           ;; A hash-map by source file name of the form-path line and column numbers.
           #:*path-locations*
           ;; This should be bound to the current source file being processed.
           #:*current-source-file*
           ;; A generic method called for each compiled lisp source file.
           #:compile-source
           ;; Generic processing for files.
           #:process-file
           ;; Generics for specific commands.
           #:init-action
           #:finish-action
           #:load-file
           ;; bazel-lisp warning handler.
           #:handle-warning
           ;; Post entry generic handler for each command.
           #:execute-command
           ;; Action model accessors
           #:*action*
           #:action
           #:action-command
           #:action-args
           #:action-output-files
           #:action-processing-sources-p
           #:action-save-runtime-options-p
           #:action-main-function
           #:action-warning-handlers
           #:action-compilation-mode
           #:action-lisp-load-mode
           #:action-fasl-load-mode
           #:action-source-files
           #:action-find-output-file
           #:action-failures
           #:action-deferred-warnings
           ;; Misc
           #:add-features
           #:add-feature))

(in-package #:bazel.main)

;;;
;;; Basic Utilities
;;;

(defstruct file
  "Represents a file with name and contents."
  (name nil :type string)
  (contents nil :type (simple-array octet)))

(defmethod cl:print-object ((file file) stream)
  "Prints the file object as an unreadable object."
  (print-unreadable-object (file stream :type t)
    (format stream "~S (~D)" (file-name file) (length (file-contents file)))))

(defun get-file-contents (file-name)
  "Read contents of a file with FILE-NAME and return them as an array of octets."
  (with-open-file (stream file-name :element-type 'octet)
    (let* ((length (file-length stream))
           (contents (make-array (the fixnum length) :element-type 'octet)))
      (assert (= length (read-sequence contents stream))) ; NOLINT
      contents)))

(defun get-file (file-name)
  "Returns a file object with contents for FILE-NAME."
  (make-file
   :name file-name
   :contents (get-file-contents file-name)))

(defun stem-file-name (name)
  "Removes the type of file from the end of its NAME."
  (declare (string name))
  (let ((type (pathname-type name)))
    (if type
        (subseq name 0 (- (length name) (length (the string type)) 1))
        name)))

;;;
;;; BUILD-action model
;;;

(deftype compilation-mode () '(member :opt :fastbuild :dbg))
(deftype compile-load-mode () '(member :opt :fastbuild :dbg :load))

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
  ;; The source file to be compiled.
  (source-file nil :type (or null string))
  ;; Flag indicating that the cfasl needs to be generated.
  (emit-cfasl-p nil :type boolean)
  ;; Flag indicating that the compilation should commence even with errors.
  (force-compilation-p nil :type boolean)
  ;; Used to precompile the generic functions.
  (precompile-generics-p nil :type boolean)
  ;; Flag indicating that the final binary should have its runtime options burned.
  ;; Value T will prevent such target binary from interpreting those options from the command line.
  (save-runtime-options-p nil :type boolean)
  ;; Indicates that source forms positions should be recorded.
  (record-path-location-p nil :type boolean)
  ;; The main function for a binary.
  (main-function nil :type (or null symbol string))
  ;; A list warning handlers.
  (warning-handlers nil :type list)
  ;; The compile mode. One of :dbg, :opt, :fastbuild or :load.
  (compilation-mode nil :type compilation-mode)
  ;; The mode used to load the .lisp or .srcs files.
  (lisp-load-mode nil :type (or null compile-load-mode))
  ;; The mode used to load the .fasl and .deps files.
  (fasl-load-mode nil :type (or null compile-load-mode))
  ;; A list of failures.
  (failures nil :type list)
  ;; A list of deferred-warnings.
  (deferred-warnings nil :type list)
  ;; A count of muffled infos.
  (muffled-infos-count 0 :type fixnum)
  ;; A count of muffled warnings.
  (muffled-warnings-count 0 :type fixnum)
  ;; Readtable used for this action.
  (readtable *readtable* :type readtable))

(defmethod cl:print-object ((action action) stream)
  "Prints the FASL file object as an unreadable object."
  (print-unreadable-object (action stream :type t)
    (format stream ":command ~S :outputs ~D~@[ :main ~S~] :compilation-mode ~S ~
                    :failures ~D :deferred ~D :muffled ~D :infos ~D"
            (action-command action)
            (length (action-output-files action))
            (action-main-function action)
            (action-compilation-mode action)
            (length (action-failures action))
            (length (action-deferred-warnings action))
            (action-muffled-warnings-count action)
            (action-muffled-infos-count action))))

(declaim (type (or null action) *action*))
;; All of the state of the current bazel-lisp BUILD action.
;; The action is shared among threads.
(defvar *action* nil)
(declaim (type mutex *action-mutex*))
(defvar *action-mutex* (make-mutex :name "bazel-lisp-action-mutex")
  "Action mutex guards *action* global variable.")

(defun print-action-full (&key
                          args
                          (action *action*)
                          (verbose *verbose*)
                          (stream *standard-output*))
  "Print the ACTION using VERBOSE mode to the output STREAM."
  (declare (optimize (debug 3) (speed 0)))
  (let* ((*verbose* verbose)
         (args (copy-list (if action (action-args action) args)))
         (deps (split (getf args :deps)))
         (srcs (split (getf args :srcs)))
         (specs (getf args :specs))
         (load (split (getf args :load)))
         (outs (split (getf args :outs)))
         (warnings (split (getf args :warning)))
         (hashes (split (getf args :hashes)))
         (bindir (getf args :bindir)))
    (when (< verbose 2)
      (when (> (length deps) 1) (remf args :deps))
      (when (> (length srcs) 1) (remf args :srcs))
      (when (> (length load) 1) (remf args :load)))
    (when (< verbose 3)
      (when (> (length warnings) 1) (remf args :warnings))
      (when (> (length hashes) 1) (remf args :hashes)))

    (verbose "Program name: ~A" (program-name))
    (vv "Command line: ~{'~A'~^ ~}" (command-line-arguments))
    (verbose "Current dir: ~A" *default-pathname-defaults*)
    (verbose "Params:~{~&~3T~A: ~A~%~}" args)
    #+sbcl
    (vv "Environment:~{~%~3T~S~}~%" (sb-unix::posix-environ))
    (verbose "Action: ~A~%" action)
    (flet ((strip-bindir (name) (if bindir (strip-prefix bindir name) name)))
      (cond ((< verbose 2)
             (verbose "Deps: ~A" (length deps))
             (verbose "Srcs: ~A" (length srcs))
             (verbose "Load: ~A" (length load)))
            (t
             (vv "Deps:~{~%~3T~A~}" (mapcar #'strip-bindir deps))
             (vv "Srcs:~{~%~3T~A~}" (mapcar #'strip-bindir srcs))
             (vv "Load:~{~%~3T~A~}" (mapcar #'strip-bindir load))))
      (verbose "Outs:~{~%~3T~A~}" (mapcar #'strip-bindir outs))
      (when (< verbose 3)
        (verbose "Hashes: ~A" (length hashes))
        (verbose "Warnings: ~A" (length warnings))))
    (when (and (>= verbose 3) specs (probe-file specs))
      (verbose "Specs file: ~S contents" specs)
      (with-open-file (in specs
                          :element-type 'character
                          :external-format :utf-8)
        (loop :for line = (read-line in nil)
              :while line
              :do (write-string line stream)
                  (terpri stream))))))

;; The current file being processed.
(declaim (type (or null string) *current-source-file*))
(defvar *current-source-file* nil
  "Contains the name of the currently processed file. Used by error reporting.")

;; The set of compiled sources with their md5 checksums.
;; TODO(czak): Rename source-file-hash.
(declaim (type hash-table *compiled-sources*))
(defvar *compiled-sources* (make-hash-table :test #'equal)
  "Stores compiled source names relative to google3 with the corresponding md5 hashes.")

(defun action-add-failure (warning &optional (action *action*))
  "Add a WARNING to the failures list of the ACTION."
  (verbose "Added failure: ~S '~A'" (type-of warning) warning)
  (with-recursive-lock (*action-mutex*)
    (pushnew (list *current-source-file* (type-of warning) warning)
             (action-failures action) :test #'equalp)))

(defun action-find-output-file (action type)
  "Searches in the ACTION output files for a file ending with the string TYPE."
  (declare (type action action) (string type))
  (find type (action-output-files action) :key #'pathname-type :test #'equalp))

;;;
;;; Functions dealing with compiler warnings and deferred warnings.
;;; This requires the bazel.warning package.
;;; TODO(czak): Add support for those into UIOP.
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
            ((:show) (setf result :show))
            ((:fail) (return :fail))
            (t       (when restart (return :muffle)))))
        finally (return result)))

(defun save-deferred-warnings (warning-file warnings)
  "Saves the WARNINGS to the WARNING-FILE."
  (declare (string warning-file) (list warnings))
  (verbose "Saving ~A warning~:P: ~S" (length warnings) warning-file)
  (delete-read-only warning-file)
  (with-open-file (out warning-file :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (format out "~@[(~{~S~^~%~})~]" warnings))))

(defun read-deferred-warnings (action warnings-file)
  "Reads warnings from the WARNINGS-FILE and appends those to the ACTION deferred-warnings."
  (with-open-file (in warnings-file)
    (with-standard-io-syntax
      (loop with count fixnum = 0
            for warnings = (read in nil :eof)
            until (eq warnings :eof) do
              (incf count (length (the list warnings)))
              (setf (action-deferred-warnings action)
                    (union (action-deferred-warnings action) warnings :test #'equalp))
            finally
         (message :info (if (plusp count) 1 3)
                  "Read ~D warning~:P from: ~A" count warnings-file)))))

(defun resolve-deferred-warnings (warnings)
  "Try to resolve deferred WARNINGS. Return a list of unresolved ones."
  (declare (list warnings))
  (let ((length (length warnings)))
    (message :info (if (plusp length) 1 2) "Resolving ~D deferred warning~:P" length))
  (loop for warning in warnings
        for (src kind-of-warning data) = warning
        for not-resolved
          = (if (and (eq kind-of-warning :undefined-function)
                     (fboundp data))
                (cond ((inline-function-p data)
                       ;; Inline functions cannot be deferred.
                       `((,src :undefined-inline-function ,data)))
                      ((and (symbolp data) (macro-function data))
                       `((,src :undefined-macro ,data)))
                      ((compiler-macro-function data)
                       `((,src :undefined-compiler-macro-function ,data)))
                      ((function-has-transforms-p data)
                       `((,src :undefined-function-transforms ,data))))
                (list warning))
        do
     (message :info (if not-resolved 1 2) "~8T~S => ~:[~;not ~]resolved." warning not-resolved)
        nconc not-resolved))

(defun handle-warning (warning &optional (action *action*))
  "Invoke the WARNING handlers and adds a failure to the ACTION failure list."
  (unless *current-source-file* (return-from handle-warning nil))
  (let ((result (invoke-warning-handlers (action-warning-handlers action) warning))
        (warning-p (typep warning 'warning)))
    (ecase result
      (:ignore
       (bazel.log:vvv "IGNORE: ~S '~A'" (type-of warning) warning))
      (:show
       (bazel.log:info "SHOW: ~S '~A'" (type-of warning) warning))
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

(defun delete-doc-strings ()
  "Delete all the symbol doc strings."
  (do-all-symbols (var)
    (dolist (type '(function type structure variable setf method-combination compiler-macro))
      (when (documentation var type)
        (setf (documentation var type) nil)))))

(declaim (type (or symbol function) *entry-point*))
(defvar *entry-point* nil)

(defun restart-image ()
  "Restart function that is called when the image is executed next time.
 Calls toplevel-init if no *entry-point* or calls the function specified in LISP_MAIN."

  (let ((entry-point *entry-point*)
        (LISP_MAIN (getenv "LISP_MAIN")))

    ;; Provided UIOP is loaded, apply its image restore protocol.
    (funcall-named "UIOP:CALL-IMAGE-RESTORE-HOOK")

    (when LISP_MAIN
      (let ((main (read-from-string LISP_MAIN)))
        (unsetenv "LISP_MAIN")
        (setf entry-point (unless (eq main t) main))))

    (if entry-point
        (funcall entry-point)
        (default-toplevel-loop))))

(defun derive-entry-point (main)
  "Returns NIL, SYMBOL, or FUNCTION based on the MAIN function specification."
  (let* ((main-exp
          (if (stringp main)
              (with-standard-io-syntax
                (read-from-string main))
              main)))
    (typecase main-exp
      (null nil)
      (function main-exp)
      (symbol
       (unless (fboundp main-exp)
         (fatal "~S is not a known function name." main-exp))
       main-exp)
      (cons
       (let ((fname (first main-exp)))
         (cond ((or (not (symbolp fname))
                    (macro-function fname)
                    (special-operator-p fname))
                (lambda () (eval main-exp)))
               ((fboundp fname)
                (lambda () (apply fname (rest main-exp))))
               (t
                (fatal "~S is not a known function name." fname)))))
      (t
       (fatal "Cannot use ~S as an entry point." main-exp)))))

(defun save-image (name main &key save-runtime-options precompile-generics remove-debug-info
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
  (let ((main-fn (derive-entry-point main)))
    (verbose "Saving binary to: ~S~@[ (old-main: ~S)~]~@[ (main: ~S)~]"
             name (unless (eq main-fn *entry-point*) *entry-point*) main-fn)
    (setf *entry-point* main-fn))
  (when remove-debug-info
    (remove-extra-debug-info))
  ;; Provided UIOP is loaded, apply its image dump protocol.
  (funcall-named "UIOP:CALL-IMAGE-DUMP-HOOK")
  ;; Set to a sane value.
  (in-package "COMMON-LISP-USER")
  ;; Finally call the Lisp implementation function.
  (save-lisp-and-die
   name
   :toplevel #'restart-image
   :save-runtime-options save-runtime-options
   :precompile-generics precompile-generics
   :executable executable
   :verbose (plusp *verbose*)))

(defun set-compilation-mode (compilation-mode)
  "Proclaim the optimization settings based on the COMPILATION-MODE."

  (vvv "Set compilation mode: ~S" compilation-mode)

  (destructuring-bind (spEed Debug saFety space Compilation-speed)
      (ecase compilation-mode ; E D F   C
        (:load                '(1 1 1 1 3))
        ((:fastbuild nil)     '(1 2 3 1 1))
        (:opt                 '(3 0 0 1 1))
        (:dbg                 '(1 3 3 1 1)))

    (set-interpret-mode compilation-mode)

    ;; Cause bodies of macroexpanders, including MACROLET and DEFINE-COMPILER-MACRO,
    ;; to be compiled in a policy in which these qualities override the global policy.
    #+sbcl (sb-ext:set-macro-policy '((speed 0) (safety 3)))

    (proclaim `(optimize (speed ,speed) (debug ,debug) (safety ,safety)
                         (space ,space) (compilation-speed ,compilation-speed)
                         ;; always insert array bounds, even in otherwise optimized code;
                         ;; optimizing this out was measured not to be worth the trouble.
                         #+sbcl(sb-c::insert-array-bounds-checks 3)))))

(defun to-feature (feature)
  "Return a symbol feature derived from a FEATURE string or symbol.

By default the string is read into the KEYWORD package.
If the feature string is package prefixed, the package
is instantiated unless already provided.

If the feature parses as anything other than a symbol,
it will signal an error."
  (typecase feature
    (symbol feature)
    (string
     (multiple-value-bind (value error)
         (ignore-errors
          (let ((*package* (find-package "KEYWORD")))
            (with-creating-find-package ()
              (values (read-from-string feature)))))
       (cond ((and (symbolp value) value))
             (error
              (bazel.log:fatal
               "Could not parse ~S as a feature due to~% ~S: ~A~%"
               feature (type-of error) error)
              nil)
             (t
              (bazel.log:fatal "Cannot parse ~S as a feature." feature)
              nil))))
    (t
     (bazel.log:fatal "~S is not a feature." feature))))

(defun add-feature (feature)
  "Add a single string FEATURE to *features*."
  (let ((feature (to-feature feature)))
    (when feature
      (pushnew feature *features*))))

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
                       (muffle-warnings (not (action-processing-sources-p action)))
                       (readtable (action-readtable action)))
  "Loads a file with NAME using action-compilation-mode.
 Checks for duplications and marks file as loaded. The warnings are muffled for dependencies.
 Arguments:
  NAME - the name of the file to load,
  FASL - if non-nil, the FASL will be loaded in place of the Lisp file.
  ACTION - the current bazel action object,
  LOAD-MODE - the load mode used to load the file.
  MUFFLE-WARNINGS - if true, as in the case of deps, no warnings will be printed.
  READTABLE - is the readtable to be used while loading."
  (declare (type (or string pathname) name) (type action action))
  (unless load-mode
    (return-from load-file))
  (with-open-file (in (or fasl name))
    (unless (plusp (file-length in))
      (bazel.log:verbose "Not loading an empty file: ~S." name)
      (return-from load-file)))
  (with-safe-io-syntax
    (handler-bind ((non-fatal-error #'handle-error))
      (with-compilation-unit (:source-namestring name)
        (let* ((name (namestring name))
               (*default-pathname-defaults* *default-pathname-defaults*)
               (*current-source-file* name)
               (*readtable* (setup-readtable readtable))
               (*action* action))
          (set-compilation-mode load-mode)
          (cond (muffle-warnings
                 (with-all-warnings-muffled
                   ;; TODO(czak): use bazel.warning:redefine-warning.
                   ;;   For this we need to know the NOWARN info
                   ;;   for each package.
                   (handler-bind (((or bazel.warning:redefined-function
                                       bazel.warning:redefined-macro
                                       ;; bazel.warning:changed-ftype-proclamation
                                       bazel.warning:conflicting-ftype-declaration
                                       ;; TODO(czak): someone fix cl-pb.
                                       ;; bazel.warning:redefined-generic
                                       ;; bazel.warning:redefined-method
                                       bazel.warning:redefined-package
                                       bazel.warning:inline-used-before-definition
                                       bazel.warning:compiler-macro-after-function-use)
                                   #'handle-warning))
                     (load (or fasl name) :external-format :utf-8))))
                (t
                 (load (or fasl name) :external-format :utf-8))))))))

;;;
;;; Main compile/build loop
;;;

(defgeneric compile-source (src output-file &key emit-cfasl save-locations readtable)
  (:documentation "Compile the source to a FASL output-file.
 EMIT-CFASL unless nil causes the compilation process to emit the CFASL file.
 SAVE-LOCATIONS unless nil causes the compilation process to record
 line and column numbers for all forms read from SRC.
 READTABLE is the readtable to be used for compilation."))

(defmethod compile-source (src output-file &key
                                           emit-cfasl
                                           save-locations
                                           (readtable (copy-readtable)))
  "Compiles the SRC file into the OUTPUT-FILE. A corresponding FASL will be created.
 Returns (values FASL WARNINGS-P FAILURES-P).
 Parameters:
  EMIT-CFASL set to non-nil will also emit the corresponding CFASL file.
  SAVE-LOCATIONS when non-nil will save the path locations to the FASL file as well.
  READTABLE is the readtable to be used for compiling the SRC file."
  (verbose "~S => ~S (~A)" src (namestring output-file) *default-pathname-defaults*)
  (ensure-directories-exist output-file)
  (multiple-value-bind (fasl warnings-p failures-p)
      (with-compilation-unit (:source-namestring src)
        (with-safe-io-syntax
            (let ((output-file (merge-pathnames output-file))
                  (*default-pathname-defaults* *default-pathname-defaults*)
                  (*readtable* (setup-readtable readtable))
                  (sb-regalloc:*register-allocation-method* :iterative))
              (delete-read-only output-file)
              (compile-file
               src :output-file output-file
               :emit-cfasl emit-cfasl
               :external-format :utf-8
               :block-compile
               (not
                (or
                 (string= (pathname-name src) "api")
                 ;; cl-ppcre, quote-meta-chars
                 (string= (pathname-name src) "lisp-deps")
                 ;; failed AVER: (EQ SB-C::PHYSENV (SB-C::LAMBDA-PHYSENV (SB-C::LAMBDA-VAR-HOME
                 ;; SB-C::THING)))
                 (string= (pathname-name src) "c-tableau")
                 ;; qpx/shared/ failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "define-proto")
                 ;; loop-analyze NIL is not of REAL
                 (string= (pathname-name src) "log")
                 ;; regalloc error on pack-load-tn even with :iterative *register-allocation-method*
                 (string= (pathname-name src) "sbcl")
                 ;; qpx/lisp_lib/sbcl ir2-convert-enclose entry-info is NIL
                 (string= (pathname-name src) "sparse-sets")
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "rejected-fare-reasons")
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "bit-array")
                 ;; failed AVER: (NULL (SB-C::COMPONENT-NEW-FUNCTIONALS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "general-booking-codes")
                 ;; qpx/shared failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "preconditions")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/preconditions.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "ATPCO-logic")
                 ;; qpx/shared/ FAIL: SB-INT:CONSTANT-MODIFIED 'Destructive function (SETF SVREF)
                 ;; called on constant data: #(0 0 0 -- lots of 0s
                 (string= (pathname-name src) "context")
                 ;; qpx/shared failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "faring-markets")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/shared/structs/faring-markets.lisp" ' failed AVER: (MEMBER
                 ;; SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))

                 ;; (string= (pathname-name src) "pos-set-tracker")
                 (string= (pathname-name src) "macro")
                 ;; ace/core/macro UNBOUND-VARIABLE: The variable ACE.CORE.MACRO::+NUM-TO-STR+ is
                 ;; unbound. while executing: CORE
                 (string= (pathname-name src) "journey-consistency")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/journey-consistency.lisp"
                 ;; ' failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "swank-presentation-streams")
                 ;; third_party/slime/contrib failed AVER: (EQ SB-C::PHYSENV (SB-C::LAMBDA-PHYSENV
                 ;; (SB-C::LAMBDA-VAR-HOME SB-C::THING)))
                 (string= (pathname-name src) "taxes")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/itineraries/taxes.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "faring-atom-construction")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/shared/faring-atom-construction.lisp" ' failed AVER: (MEMBER
                 ;; SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "fare-list")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/fare-list.lisp" ' failed
                 ;; AVER: (EQ (SB-C::COMPONENT-KIND SB-C::NEW) (SB-C::COMPONENT-KIND SB-C::OLD))
                 (string= (pathname-name src) "fares")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/fares.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))

                 ;; (string= (pathname-name src) "api-threads")
                 (string= (pathname-name src) "global-indicator")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/global-indicator.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "c-flightmanager2")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/entities/c-flightmanager2.lisp"
                 ;; ' failed AVER: (NULL (SB-C::COMPONENT-NEW-FUNCTIONALS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "cabins")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/cabins.lisp" ' failed
                 ;; AVER: (NULL (SB-C::COMPONENT-NEW-FUNCTIONALS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "cat-169")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/rules/record-3/cat-169.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "cat-6")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/cat-6.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "xslt")
                 ;; ace/core/ ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/xslt/xslt.fasl" 'The variable
                 ;; QPX.XSLT::+%VALIDATION-MODE-KEYWORD-NUMERAL-MAP%+ is unbound.'
                 (string= (pathname-name src) "cat-17")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/cat-17.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "types")
                 ;; cffi/ UNDEFINED-FOREIGN-TYPE-ERROR: Unknown CFFI type :BOOLEAN while executing:
                 ;; CORE (string= (pathname-name src) "cat-12-surcharge-data-info")
                 (string= (pathname-name src) "faresets")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/faresets.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (search "uiop" (namestring src))
                 ;; package errors
                 (search "asdf" (namestring src))
                 ;; package errors
                 (search "proto" (namestring src))
                 ;; I don't actually know why this is required, but without it, package errors
                 ;; follow
                 (search "rpc2" (namestring src))
                 (search "google/type" (namestring src))
                 ;; seems like compiling .proto files is bad
                 (search "swank-" (namestring src))
                 ;; swank seems to have some weird packaging stuff, so just get rid of them all
                 (search "cflag" (namestring src))
                 ;; idk in what file this is in ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/lisp/cflag/cflag.fasl" 'The variable
                 ;; GOOGLE.CFLAG::+%TYPE-KEYWORD-NUMERAL-MAP%+ is unbound.'
                 (string= (pathname-name src) "reissue-methods")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/shared/reissues/reissue-methods.lisp" ' failed AVER: (MEMBER
                 ;; SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "routings")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/routings.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "record-checkers")
                 ;;  ERROR: SB-INT:BUG while processing: "travel/qpx/shared/record-checkers.lisp" '
                 ;;  failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;;  (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "tableau-tracker")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/tableau-tracker.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "mfp-evaluation")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/rules/optional-services/mfp-evaluation.lisp" ' failed AVER:
                 ;; SB-C::SUCC
                 (string= (pathname-name src) "tsis")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/tsis.lisp" ' failed AVER:
                 ;; (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "cat-12")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/shared/cat-12.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "cat-5")
                 ;;  ERROR: SB-INT:BUG while processing: "travel/qpx/shared/cat-5.lisp" ' failed
                 ;;  AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;;  SB-C::CLAMBDA)))
                 (string= (pathname-name src) "hash-set")
                 ;; ERROR: SB-INT:BUG while processing: "lisp/container/hash-set.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "booking-logic-memo")
                 ;;  ERROR: SB-INT:BUG while processing: "travel/qpx/shared/booking-logic-memo.lisp"
                 ;;  ' failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;;  (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "record-s1")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/structs/record-s1.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "baggage")
                 ;;  ERROR: SB-INT:BUG while processing: "travel/qpx/farecomplex/baggage.lisp" '
                 ;;  failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;;  (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "record-2c")
                 ;;  pERROR: SB-INT:BUG while processing: "travel/qpx/fares/record-2c.lisp" ' failed
                 ;;  AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;;  SB-C::CLAMBDA)))
                 (string= (pathname-name src) "baggage-travel")
                 ;;  ERROR: SB-INT:BUG while processing:
                 ;;  "travel/qpx/rules/optional-services/baggage-travel.lisp" ' failed AVER: (MEMBER
                 ;;  SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "service-fee-pruning")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/rules/surcharges/service-fee-pruning.lisp" ' failed AVER: (MEMBER
                 ;; SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "new")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/farecomplex/new.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "directionality")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/fmsets/directionality.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "combinability-checker")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/fares/combinability-checker.lisp" ' failed AVER: (MEMBER
                 ;; SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "restrictions")
                 ;;  ERROR: SB-INT:BUG while processing: "travel/qpx/availability/restrictions.lisp"
                 ;;  ' failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;;  (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "faring-atom-graph")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/fares/faring-atom-graph.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "write-database")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/availability/write-database.lisp" ' failed AVER: (MEMBER
                 ;; SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "logic")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/availability/logic.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "combinability")
                 ;;  ERROR: SB-INT:BUG while processing: "travel/qpx/fares/combinability.lisp" '
                 ;;  failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;;  (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "availability-based-pruning")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/itineraries/availability-based-pruning.lisp" ' failed AVER: (MEMBER
                 ;; SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "cat-8")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/rules/record-3/cat-8.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "hips")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/iata-checks/hips.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "yqyr-conditional-amounts")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/rules/surcharges/yqyr-conditional-amounts.lisp" ' failed AVER: (NULL
                 ;; (SB-C::COMPONENT-NEW-FUNCTIONALS (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "usca-taxes")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/tax/usca-taxes.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "refactor")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/fmsets/refactor.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "fare-query")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/fares/fare-query.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "multislice")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/fares/multislice.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "yqyr-estimates")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/rules/surcharges/yqyr-estimates.lisp" ' failed AVER: (MEMBER
                 ;; SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "rate-tax-recomputation")
                 ;; ERROR: SB-INT:BUG while processing:
                 ;; "travel/qpx/pricing-solutions/rate-tax-recomputation.lisp" ' failed AVER:
                 ;; (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "fmsets")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/fmsets/fmsets.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "build")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/pricing-graph/build.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "fxs-and-fcs-plans")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/fares/fxs-and-fcs-plans.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "fmset-graph")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/fares/links/fmset-graph.lisp" '
                 ;; failed AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS
                 ;; (SB-C::LAMBDA-COMPONENT SB-C::CLAMBDA)))
                 (string= (pathname-name src) "links")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/fares/links/links.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "replutils")
                 ;; ERROR: SB-INT:BUG while processing: "travel/qpx/repl/replutils.lisp" ' failed
                 ;; AVER: (MEMBER SB-C::CLAMBDA (SB-C::COMPONENT-LAMBDAS (SB-C::LAMBDA-COMPONENT
                 ;; SB-C::CLAMBDA)))
                 (string= (pathname-name src) "proto-defs")
                 ;;  LOL it ran out of memory (Forge action was terminated due to Out of Memory
                 ;;  (action used 12884901888 bytes). See http://go/forge-oom for more detail.)
                 (string= (pathname-name src) "shared-cl-pb")
                 ;;  oom
                 (string= (pathname-name src) "integer-utils")
                 ;; /qpx/lisp_lib/integer-utils UNBOUND-VARIABLE: The variable
                 ;; QPX.INTEGER-UTILS::+UNPADDED-INTEGER-STRINGS+ is unbound. while executing: CORE
                 (string= (pathname-name src) "times-and-dates")
                 ;; ERROR: UNDEFINED-FUNCTION while processing:
                 ;; "bazel-out/k8-opt-exec-A570EE19/bin/travel/qpx/lisp_lib/times-and-dates/times-and-dates.fasl"
                 ;; 'The function QPX.TIMES-AND-DATES:MAKE-DURATION is undefined.'

                 ;; (string= (pathname-name src) "profiler") ;;
                 ;; SB-KERNEL::UNDEFINED-ALIEN-FUNCTION-ERROR: The alien function
                 ;; "ProfilingIsEnabledForAllThreads" is undefined.
                 (string= (pathname-name src) "three-letter-acronyms")
                 ;; ERROR: UNDEFINED-FUNCTION while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/tlas/tlas.fasl" 'The function
                 ;; QPX.TLAS:DESIGNATOR2TCA is undefined.'
                 (string= (pathname-name src) "qpx_api-cl-pb")
                 ;;  oom
                 (string= (pathname-name src) "x1-taxes")
                 ;; oom
                 (string= (pathname-name src) "trace-context")
                 ;; ERROR: CFFI::UNDEFINED-FOREIGN-TYPE-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/lisp/rpc2/trace-context/trace-context.fasl" 'Unknown
                 ;; CFFI type GOOGLE.RPC2.TRACE-CONTEXT::INITIALIZER'ERROR:
                 ;; CFFI::UNDEFINED-FOREIGN-TYPE-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/lisp/rpc2/trace-context/trace-context.fasl" 'Unknown
                 ;; CFFI type GOOGLE.RPC2.TRACE-CONTEXT::INITIALIZER'
                 (string= (pathname-name src) "rpc2-service")
                 ;; ERROR: CFFI::UNDEFINED-FOREIGN-TYPE-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/lisp/rpc2/rpc2.fasl" 'Unknown CFFI type
                 ;; RPC2:STREAM-RESPONSE'
                 (string= (pathname-name src) "trace-context")
                 ;; ERROR: CFFI::UNDEFINED-FOREIGN-TYPE-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/lisp/rpc2/rpc2.fasl" 'Unknown CFFI type
                 ;; RPC2:STREAM-RESPONSE'
                 (string= (pathname-name src) "with-time-logging")
                 ;; ERROR: UNDEFINED-FUNCTION while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/time-logging/with-time-logging.fasl"
                 ;; 'The function QPX.WITH-TIME-LOGGING:TIME-LOG-ARRAY-FROM-PAIRS is undefined.'
                 (string= (pathname-name src) "time-log-metadata")
                 ;;  ERROR: UNDEFINED-FUNCTION while processing:
                 ;;  "bazel-out/k8-fastbuild/bin/travel/qpx/shared/time-logging/with-time-logging.fasl"
                 ;;  'The function QPX.WITH-TIME-LOGGING:TIME-LOG-ARRAY-FROM-PAIRS is undefined.'
                 (string= (pathname-name src) "c-places")
                 ;; ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/structs/c-places.fasl" 'The
                 ;; variable QPX.STRUCTS.C-PLACES:+AIRCRAFT-CATEGORY-VECTOR+ is unbound.'
                 (string= (pathname-name src) "fare-lookup-key")
                 ;; ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/structs/fare-lookup-key.fasl" 'The
                 ;; variable QPX.STRUCTS.FARE-LOOKUP-KEY:+QPXMASTER-FOOTNOTE-PRUNING-ALL-CATS+ is
                 ;; unbound.'
                 (string= (pathname-name src) "locations")
                 ;; ERROR: UNDEFINED-FUNCTION while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/structs/locations.fasl" 'The
                 ;; function QPX.STRUCTS.LOCATIONS:MAKE-LOCATION-REGION-DATA is undefined.'
                 (string= (pathname-name src) "point-of-sale")
                 ;; ERROR: UNDEFINED-FUNCTION while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/structs/point-of-sale.fasl" 'The
                 ;; function QPX.STRUCTS.POINT-OF-SALE:MAKE-ACCOUNT-CODE is undefined.'
                 (string= (pathname-name src) "qryparam-shared-struct")
                 ;; ERROR: SB-PCL:CLASS-NOT-FOUND-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/structs/qryparams.fasl" 'There is
                 ;; no class named QPX.SQP:SHARED-QUERY-PARAMS.'
                 (string= (pathname-name src) "precondition-tags")
                 ;; ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/precondition-tags.fasl" 'The
                 ;; variable QPX.PRECONDITION-TAGS::+PRECONDITION-TAG-ALL-SET-MASK+ is unbound.'
                 (string= (pathname-name src) "legs2")
                 ;; ERROR: UNDEFINED-FUNCTION while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/flights/flights.fasl" 'The
                 ;; function QPX.FLIGHTS::AIRCRAFT-CONFIG-CABIN-BV is undefined.'
                 (string= (pathname-name src) "fees")
                 ;; ERROR: SIMPLE-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/structs/fees.fasl" 'Unknown
                 ;; pseudotype QPX.STRUCTS.FEES:FEE-BITS.'
                 (string= (pathname-name src) "currencies")
                 ;; ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/currencies.fasl" 'The variable
                 ;; QPX.CURRENCIES::+SHIFT-DECIMAL-PLACES-SHIFTS+ is unbound.'
                 (string= (pathname-name src) "qryparam-struct")
                 ;; ERROR: SB-PCL:CLASS-NOT-FOUND-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/structs/query-parameters.fasl" 'There is
                 ;; no class named QPX.QP:QUERY-PARAMETERS.'
                 (string= (pathname-name src) "markers")
                 ;; ERROR: UNDEFINED-FUNCTION while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/markers.fasl" 'The function
                 ;; QPX.MARKERS:GET-MARKER is undefined.'
                 (string= (pathname-name src) "condition-bits")
                 ;; ERROR: SIMPLE-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/condition-bits.fasl" 'Unknown
                 ;; pseudotype QPX.CONDITION-BITS::SLICE-BC-BINS.'
                 (string= (pathname-name src) "e2e-restriction")
                 ;; ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/structs/e2e-restriction.fasl" 'The
                 ;; variable QPX.STRUCTS.E2E-RESTRICTION:+NULL-E2E-RESTRICTION+ is unbound.'
                 (string= (pathname-name src) "itinerary-condition")
                 ;; ERROR: UNDEFINED-FUNCTION while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/pricing/itinerary-condition.fasl" 'The
                 ;; function QPX.ITINERARY-CONDITION:MAKE-SLICE-CONDITION-CONSTANT is undefined.'
                 (string= (pathname-name src) "queries")
                 ;; ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/query-handling/queries.fasl" 'The
                 ;; variable QPX.RQL.QUERIES::+%QUERY-PRINTING-KEYWORD-NUMERAL-MAP%+ is unbound.'
                 (string= (pathname-name src) "qryparam-process-market-struct")
                 ;; Another struct is unbound
                 (string= (pathname-name src) "qryparam-process-market-client-struct")
                 ;; another struct is unbound
                 (string= (pathname-name src) "qpx-logwriter")
                 ;; ERROR: SB-SYS:MEMORY-FAULT-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/shared/qpx-logwriter.fasl" 'Unhandled
                 ;; memory fault at #x0.'
                 (string= (pathname-name src) "baggage-services")
                 ;; ERROR: UNDEFINED-FUNCTION while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/rules/optional-services/baggage-services.fasl"
                 ;; 'The function QPX.BAGGAGE-SERVICES::MAKE-BAGGAGE-SERVICE-TYPE is undefined.'
                 (string= (pathname-name src) "s1-tree")
                 ;; ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/rules/surcharges/s1-tree.fasl" 'The
                 ;; variable QPX.S1-TREE:+S1-NODE-EMPTY-SEG-IDX+ is unbound.'
                 (string= (pathname-name src) "peak-heap")
                 ;; ERROR: SB-PCL:CLASS-NOT-FOUND-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/query-handling/peak-heap.fasl" 'There is
                 ;; no class named QPX.PEAK-HEAP::QUERY-PARAMETERS.'
                 (string= (pathname-name src) "compression")
                 ;; ERROR: CFFI::UNDEFINED-FOREIGN-TYPE-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/lisp_lib/compression/compression.fasl"
                 ;; 'Unknown CFFI type (:STRUCT COMPRESSION::C-BYTES-AND-LEN)'
                 (string= (pathname-name src) "qryparam-preprocess-slice-struct")
                 ;; ERROR: SB-PCL:CLASS-NOT-FOUND-ERROR while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/preprocess-slice/structs/qryparams.fasl"
                 ;; 'There is no class named QPX.PPSQP:PREPROCESS-SLICE-QUERY-PARAMS.'
                 (string= (pathname-name src) "pc-b-or-defer-array")
                 ;; ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/rules/surcharges/pc-b-or-defer-array.fasl"
                 ;; 'The variable QPX.PC-B-OR-DEFER-ARRAY::+POD-ARRAY-INITIAL-ELEMENT+ is unbound.'
                 (string= (pathname-name src) "rql-parse-utils")
                 ;; ERROR: UNBOUND-VARIABLE while processing:
                 ;; "bazel-out/k8-fastbuild/bin/travel/qpx/query-handling/rql-parse-utils.fasl" 'The
                 ;; variable QPX.RQL-PARSE-UTILS::+RQL-BACKSLASH-CONVERSIONS+ is unbound.'
                 (string= (pathname-name src) "heapz")
                 ;; unbound variable
                 (string= (pathname-name src) "") ;;
                 ))))))
    (unless (and warnings-p failures-p)
      (vv "File ~S compiled without warnings." src))
    (when warnings-p
      (verbose "File ~S compiled with warnings." src))
    (with-simple-restart (continue "Ignore compilation failure for ~A and continue." src)
      (when failures-p
        (fatal "File ~S failed to compile." src)))
    (when save-locations
      (funcall-named* "BAZEL.PATH:SAVE-LOCATIONS" src output-file :readtable readtable))
    (values fasl warnings-p failures-p)))

(defun write-file-hash (src hash-file)
  "Compute the hash of the SRC file and write it to the HASH-FILE."
  (assert (equalp (pathname-type hash-file) "hash")) ; NOLINT
  (let ((md5sum (md5sum-file src)))
    (delete-read-only hash-file)
    (with-open-file (out hash-file :direction :output :if-exists :supersede :element-type 'octet)
      (write-stringz src out)
      (write-sequence md5sum out))
    (vv "Saved MD5 sum ~S to ~S." md5sum hash-file)))

(defun defer-undefined-warning (warning &optional (action *action*))
  "If the WARNING is an undefined function warning, add it to ACTION's deferred warnings."
  (multiple-value-bind (undefined function) (bazel.warning:undefined-function-p warning)
    (when undefined
      (verbose "Added deferred warning: ~S '~A'" (type-of warning) warning)
      (pushnew `(,*current-source-file* :undefined-function ,function)
               (action-deferred-warnings action) :test #'equalp)
      t)))

;;;
;;; File handlers
;;;

(defgeneric process-file (action file type)
  (:documentation "Process each input FILE of TYPE for this BUILD ACTION."))

(defmethod process-file ((action action) (file string) type)
  "Skips the given file for which there is no other handler."
  ;; Maybe this should error instead of skip, but it's possible for files to be included in the
  ;; build command-line just to forward those to things analyzing the compilation with extra
  ;; actions (i.e. .meta files forwarded to the Kythe indexer):
  ;; https://docs.bazel.build/versions/master/be/extra-actions.html
  (verbose "File skipped: ~S [~A]" file type))

(defmethod process-file ((action action) (file string) (type (eql :lisp)))
  "Process a file with the .lisp or .lsp extensions. Loads file if not loaded, yet."
  (cond ((and (action-processing-sources-p action)
              (eq (action-command action) :compile))
         ;; The .lisp sources in a :compile action are processed by finish-action handler instead.
         (assert (null (action-source-file action)) nil ; NOLINT
          "Only once source file supported for the COMPILE action.")
         (setf (action-source-file action) file))
        (t
         (load-file file :action action :load-mode (action-lisp-load-mode action)))))

(defmethod process-file ((action action) (file string) (type (eql :lsp)))
  "Alias for function processing .lisp files."
  (process-file action file :lisp))

(defmethod process-file ((action action) (file string) (type (eql :fasl)))
  "Loads a FASL file."
  (prog1 (load-file file :fasl file :action action
                         :load-mode (action-fasl-load-mode action))
    ;; Sort some heap after every FASL.
    #+sbcl (sb-ext:gc)))

(defmethod process-file ((action action) (file string) (type (eql :cfasl)))
  "Loads a CFASL file. Those are dependencies only loaded when compiling or building a binary."
  (unless (action-processing-sources-p action)
    (load-file file :action action :load-mode (action-fasl-load-mode action))))

(defmethod process-file ((action action) (file string) (type (eql :warnings)))
  "Loads a deferred warnings file. Deferred warnings are only checked in a binary (final) target."
  (cond ((member (action-command action) '(:core :binary))
         ;; For binary target read the deferred warnings here so those can be checked
         ;; when the action is finalized later.
         (read-deferred-warnings action file))))

(defmethod process-file ((action action) (file string) (type (eql :hash)))
  "Loads an MD5 hash file."
  (with-open-file (in file :element-type 'octet)
    ;; NAMESTRING canonicalizes to base-char (in SBCL at least), and furthermore
    ;; returns a shareable string memoized on the corresponding pathname object.
    (let ((src (namestring (read-stringz in)))
          (md5 (make-array 16 :element-type 'octet)))
      (assert (= 16 (read-sequence md5 in))) ; NOLINT
      (setf (gethash src *compiled-sources*) md5))))

(defun process-file* (file &optional (action *action*))
  "Sets the environment before processing the file."
  (let ((*current-source-file* file))
    (vvv "~:[dep~;src~]: ~S" (action-processing-sources-p action) file)
    (process-file action file (to-keyword (pathname-type file)))))

(defun process-dependencies (deps)
  "Iterates through the DEPS dependencies and invokes process-file on the DEPS."
  (verbose "Processing ~D dependencie~:P..." (length deps))
  (with-all-warnings-muffled
    (with-compilation-unit ()
      (map () #'process-file* deps)))
  (values))

;;;
;;; Command handlers
;;;

(defgeneric execute-command (command &rest arguments &key &allow-other-keys)
  (:documentation "Executes a COMMAND with the command line ARGUMENTS."))

(defgeneric init-action (action command)
  (:documentation "Initializes the action based on the command")
  (:method ((action action) command) #| noop |#))

(defgeneric finish-action (action command)
  (:documentation "Given a finished BUILD action execute the final command."))

(defun check-and-save-image (action command)
  "Save the binary from this image."
  (nconcf (action-failures action)
          (resolve-deferred-warnings
            (shiftf (action-deferred-warnings action) nil)))
  (check-failures action)
  (check-features)

  ;; Assure things are in a defined state.
  ;; Save image. Exit.
  (save-image (first (action-output-files action))
              (action-main-function action)
              :remove-debug-info (eq (action-compilation-mode action) :opt)
              :save-runtime-options (action-save-runtime-options-p action)
              :precompile-generics (action-precompile-generics-p action)
              :executable (eq command :binary)))

(defmethod finish-action ((action action) (command (eql :binary)))
  (check-and-save-image action command))
(defmethod finish-action ((action action) (command (eql :core)))
  (check-and-save-image action command))

(defmethod finish-action ((action action) (command (eql :compile)))
  "Compiles the last source file."
  ;; TODO(czak): Design a valid model for this action with more than 1 source file.
  (assert (action-source-file action) nil ; NOLINT
          "Exactly one Lisp source file needed for the COMPILE action.")
  (let* ((source-file (action-source-file action))
         (*current-source-file* source-file))
    (pprog1
     ;; The file should compile in the current-thread context.
     (compile-source source-file (action-find-output-file action "fasl")
                     :emit-cfasl (action-emit-cfasl-p action)
                     :save-locations (action-record-path-location-p action)
                     :readtable (action-readtable action))
     (write-file-hash source-file (action-find-output-file action "hash")))
    (check-failures action)
    (save-deferred-warnings
     (action-find-output-file action "warnings")
     (action-deferred-warnings action))))

(defun parse-specs (specs)
  "Parse the SPECS file and return values for SRCS, DEPS, LOAD, WARNINGS, and HASHES."
  (let (srcs deps load warnings hashes)
    (with-open-file (in specs :direction :input :element-type 'character)
      (loop for spec = (read in nil in)
            until (eq spec in)
            do
         (ecase (first spec)
           (:srcs (setf srcs (rest spec)))
           (:deps (setf deps (rest spec)))
           (:load (setf load (rest spec)))
           (:warnings (setf warnings (rest spec)))
           (:hashes (setf hashes (rest spec))))))
    (values srcs deps load warnings hashes)))

;;;
;;; Main Processing Loop
;;;

(defun process (command &rest args
                   &key deps load srcs outs bindir
                   warnings hashes
                   specs
                   (compilation-mode :fastbuild)
                   force
                   main features nowarn
                   precompile-generics
                   save-runtime-options
                   coverage
                   emit-cfasl
                   &allow-other-keys)
  "Main processing function for bazel.main.
 Arguments:
  ARGS - all the arguments,
  COMMAND - one of :core, :binary, or :compile,
  DEPS - dependencies,
  LOAD - files to be loaded after dependencies.
  SRCS - sources for a binary core or for compilation,
  OUTS - the output files,
  BINDIR - the directory for the output files (for debug),
  WARNINGS - is a list of files that contain deferred warnings,
  HASHES - is a list of files with defined source hashes,
  COMPILATION-MODE - from bazel -c <compilation-mode>
  FORCE - if true, the compilation may run to completion even with errors.
  MAIN - the name of the main function for a binary,
  FEATURES - features to be set before reading sources,
  NOWARN - list of warnings to be muffled,
  PRECOMPILE-GENERICS - if non-nil, precompile-generics before saving core,
  SAVE-RUNTIME-OPTIONS - will save the runtime options for the C runtime.
  COVERAGE - if the results should be instrumented with coverage information.
  EMIT-CFASL - will emit also .CFASL file in addition to the FASL file."
  (multiple-value-setq (srcs deps load warnings hashes)
    (if specs
        (parse-specs specs)
        (values (split srcs)
                (split deps)
                (split load)
                (split warnings)
                (split hashes))))

  (let* ((command (to-keyword command))
         (outs (split outs))
         (compilation-mode (to-keyword compilation-mode))
         (action
           (make-action :args args
                        :command command
                        :output-files outs
                        :bindir bindir
                        :compilation-mode compilation-mode
                        :main-function main
                        :force-compilation-p force
                        :precompile-generics-p precompile-generics
                        :save-runtime-options-p save-runtime-options
                        :emit-cfasl-p emit-cfasl
                        :record-path-location-p (and coverage t)
                        ;; Load lisp dependencies when compiling or making srcs image
                        :lisp-load-mode (when (member command '(:binary :core :compile)) :load)
                        ;; Load fasl dependencies when compiling or creating a binary image.
                        :fasl-load-mode
                        (when (member command '(:binary :core :compile)) compilation-mode)))

         (*compile-verbose* (>= *verbose* 1))
         (*compile-print* (>= *verbose* 3))
         (*load-verbose* (>= *verbose* 2))
         (*load-print* (>= *verbose* 3)))

    (declare (list deps srcs outs))

    ;; Rebind globally.
    (setf *action* action)

    (when (>= *verbose* 1)
      (print-action-full))

    (unless outs
      (fatal "Missing output file. Called with:~%~{~12T~A: ~A~%~}" args))

    (when (and (eq command :compile) (/= (length srcs) 1))
      ;; This assertion is arbitrary.
      ;; If changed, make sure processing source files below is adapted.
      (fatal "Compile command assumes only one source. Given:~{~%~3T~A~}~%" srcs))

    (init-action action command)

    (add-features features)
    (add-default-features compilation-mode)

    (mapc (lambda (nowarn) (action-add-nowarn nowarn action)) (split nowarn))

    ;; Compiler-note failures must precede uninteresting-condition.
    (action-add-nowarn #'bazel.warning:fail-inline-expansion-limit)
    ;; (action-add-nowarn #'bazel.warning:fail-stack-allocate-notes)
    ;; (action-add-nowarn #'bazel.warning:stack-allocate-note)
    ;; All notes are discarded here.
    (action-add-nowarn 'bazel.warning:uninteresting-condition)
    (action-add-nowarn #'defer-undefined-warning)

    #+sbcl
    (when coverage
      (bazel.log:verbose "Turning on coverage-instrumented code generation.")
      (proclaim '(optimize (sb-c:store-coverage-data 3))))

    (process-dependencies deps)
    ;; Load in any source hash information files.
    (mapc #'process-file* hashes)

    (handler-bind ((condition #'handle-warning)
                   (non-fatal-error #'handle-error))
      (verbose "Loading ~D source file~:P..." (length load))
      (mapc #'process-file* load)

      ;; Switch to source file processing.
      (setf (action-processing-sources-p action) t)
      (verbose "Processing ~D source file~:P..." (length srcs))
      (mapc #'process-file* srcs)

      (verbose "Processing ~D deferred warning file~:P..." (length warnings))
      (mapc #'process-file* warnings)

      (verbose "Finalizing the ~A action..." command)
      (set-compilation-mode (action-compilation-mode action))
      (finish-action action command))))

(defmethod execute-command ((command (eql :compile)) &rest args)
  (apply #'process command args))
(defmethod execute-command ((command (eql :binary)) &rest args)
  (apply #'process command args))
(defmethod execute-command ((command (eql :core)) &rest args)
  (apply #'process command args))

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

(defmethod execute-command :around (command
                                    &rest args
                                    &key force verbose interactive
                                    &allow-other-keys)
  ;; Process some meta-level options.
  (when verbose (setf *verbose* (read-from-string verbose)))
  (set-interactive-mode interactive)

  (verbose "Program name: ~A" (program-name))
  (vv "Command line: ~{'~A'~^ ~}" (command-line-arguments))
  (verbose "Current dir: ~A" *default-pathname-defaults*)

  (handler-bind ((error (lambda (e)
                          (format *error-output*
                                  "~&~S: ~A while executing: ~A~%"
                                  (type-of e) e command)
                          (print-action-full
                           :args args :stream *error-output*))))
    (with-continue-on-error (:when force)
      (call-next-method))))

(defmethod execute-command :after (command &rest ignore)
  (declare (ignore ignore))
  (verbose "BAZEL ~A finished" command))

(defun main ()
  "Main entry point."
  (apply #'execute-command (parse-command-args (command-line-arguments))))
