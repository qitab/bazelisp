;;; A simple utility for blazing lisp.
;;;
;;; Command line invocation:
;;; blaze-lisp compile -v 2 -W "optional-and-key" "test.lisp" test.fasl
;;;

;; Default compilation settings for blaze-lisp.
#-dbg (declaim (optimize (speed 3) (safety 1)))

(defpackage #:bazel.main
  (:use #:common-lisp #+sbcl #:bazel.sbcl #:bazel.log #:bazel.utils)
  (:export #:save-binary
           ;; Main entry point for blaze-lisp
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
           ;; blaze-lisp warning handler.
           #:handle-warning
           ;; Action model accessors
           #:*action*
           #:action
           #:action-command
           #:action-args
           #:action-output-files
           #:action-processing-sources-p
           #:action-dump-alien-symbols-p
           #:action-compressed-p
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
           #:add-features))

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
  "The blaze-lisp action contains the input parameters and
  the state of the current BUILD action."
  (command nil :type keyword)
  ;; The arguments passed to the program.
  (args nil :type list)
  ;; The first of the output files.
  (output-files nil :type list)
  ;; The root directory for generated files.
  (gendir nil :type (or null string))
  ;; Flag indicating that the dependencies have been processed
  ;; and the outstanding files are sources for this BUILD action.
  (processing-sources-p nil :type boolean)
  ;; The source file to be compiled.
  (source-file nil :type (or null string))
  ;; Flag indicating that the cfasl needs to be generated.
  (emit-cfasl-p nil :type boolean)
  ;; A flag that will cause the binary to write alien symbols to a file.
  (dump-alien-symbols-p nil :type boolean)
  ;; A file name to dump the extern symbols to.
  (dump-extern-symbols-file nil :type (or null string))
  ;; A file name to dump the dynamic list lds script file.
  (dump-dynamic-list-lds-file nil :type (or null string))
  ;; Flag indicating that the output image should be compressed.
  (compressed-p nil :type boolean)
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

(declaim (type action *action*))
;; All of the state of the current blaze-lisp BUILD action.
;; The action is shared among threads.
(defvar *action*)
(declaim (type mutex *action-mutex*))
(defvar *action-mutex* (make-mutex :name "blaze-lisp-action-mutex")
  "Action mutex guards *action* global variable.")

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

(defun resolve-warning-handler (handler)
  "Tries to resolve the warning HANDLER in the bazel.warning package.
 Returns a function or string if not resolved."
  (etypecase handler
    (function handler)
    (symbol
     (cond ((handler-case (subtypep handler 'condition) (t nil))
            ;; Return a predicate.
            #+sbcl
            (eval `(sb-int:named-lambda ',handler (condition) (typep condition ',handler)))
            #-sbcl
            (lambda (condition) (typep condition handler)))
           ((fboundp handler)
            ;; Return the handler function.
            (symbol-function handler))
           (handler
            ;; Return the symbol.
            handler)))
    (string
     (or (with-standard-io-syntax
           (let* ((*package* (find-package "BAZEL.WARNING"))
                  (handler (ignore-errors (read-from-string handler))))
             (unless (stringp handler)
               (resolve-warning-handler handler))))
         handler))))

(defun action-add-nowarn (nowarn &optional (action *action*))
  "Add a NOWARN condition/handler at the end of the nowarn list of the ACTION."
  (declare (type action action) (type (or string symbol function) nowarn))
  (nconcf (action-warning-handlers action)
          (list (resolve-warning-handler nowarn))))

(defun invoke-warning-handlers (handlers condition)
  "The function invokes all the HANDLERS on the CONDITION until first returns true.
If a handler is specified as a string, it will be resolved in the bazel.warning package context.
This allows for the user to specify their own handlers as a string."
  (declare (list handlers) (condition condition))
  (message :info (if (typep condition 'warning) 2 3)
           "Invoking ~D handler~:P on: ~S (~A)"
           (length handlers) (type-of condition) condition)
  (loop with restart = (find-restart 'muffle-warning)
        with result = (if restart :fail :ignore)
        for %handlers on handlers
        for handler-designator = (car %handlers)
        for handler = (if (functionp handler-designator)
                          handler-designator
                          (setf (car %handlers) (resolve-warning-handler handler-designator)))
        for unresolved-p = (not (functionp handler))
        when (and unresolved-p restart)
          do (fatal "Given condition: ~S (~A)~%; Cannot resolve handler: ~S"
                    (type-of condition) condition handler-designator)
        thereis
        (let ((value (unless unresolved-p (funcall (the function handler) condition))))
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
         (message :info (if (plusp count) 1 2)
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
    (message :info (if warning-p 2 3) "~A: ~S '~A'" result (type-of warning) warning)
    (ecase result
      ((:show :ignore))
      (:muffle
       (if warning-p
           (incf (action-muffled-warnings-count action))
           (incf (action-muffled-infos-count action)))
       (muffle-warning warning))
      (:fail
       (action-add-failure warning action)))))

(defun handle-error (error)
  "Print an info about the ERROR context."
  (info "Error while processing: ~S '~A'" *current-source-file* error))

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

(defun print-conditions (header conditions &optional gendir)
  "Outputs a list of CONDITIONS to the output stream.
 GENDIR is the directory for genfiles, that is stripped off when printing the CONDITIONS.
 HEADER is a prefix printed before all CONDITIONS."
  (when conditions
    (info "~A:~{~@[~&~3T~A:~]~&~6T ~S '~A'~}" header
          (loop for prev-src = nil then src
                for (src type condition) in conditions
                nconc (list (unless (equal src prev-src) (strip-prefix gendir src))
                            type (with-standard-io-syntax
                                   (or (ignore-errors (format nil "~S" condition))
                                       (format nil "~A" condition))))))))

(defun check-failures (action)
  "Checks for compilation failures stored in ACTION."
  (message :info (if (action-failures action) 0 1)
           "Muffled ~D warning~:P and ~D info~:P (set verbose to 2 or 3 to see them)"
           (action-muffled-warnings-count action)
           (action-muffled-infos-count action))

  (when (action-failures action)
    ;; Terminate with error. Blaze will clean up for us.
    (print-conditions "Failures" (action-failures action) (action-gendir action))
    (fatal "Blaze lisp build failed")))

;;;
;;; Blaze-Lisp specific utilities
;;;

(defun delete-doc-strings ()
  "Delete all the symbol doc strings."
  (do-all-symbols (var)
    (dolist (type '(function type structure variable setf method-combination compiler-macro))
      (when (documentation var type)
        (setf (documentation var type) nil)))))

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
        (default-toplevel-loop))

    (funcall-named* "BAZEL.COVERAGE:SAVE")))

(defun save-binary (name main &key compression save-runtime-options precompile-generics)
  "Saves the image to a binary image named 'name'. Exits.
 Arguments:
  NAME - the file name to save the image.
  MAIN - the name of the toplevel function.
  COMPRESSION - indicates the compression to be used for the image.
      Will decompress in memory instead of mmapping the image.
  SAVE-RUNTIME-OPTIONS - indicates if the runtime options shall be saved to the C runtime.
      This is usually permanent.
  PRECOMPILE-GENERICS - will precompile the generic functions before saving."

  (when (stringp main)
    (with-standard-io-syntax
      (setf main (read-from-string main))))

  (when (and main (not (fboundp main)))
    (fatal "The main entry point ~S has not been defined" main))

  (verbose "Saving binary to: ~S~@[ (old-main: ~S)~]~@[ (main: ~S)~]"
        name (unless (eq main *entry-point*) *entry-point*) main)

  ;; Provided UIOP is loaded, apply its image dump protocol.
  (funcall-named "UIOP:CALL-IMAGE-DUMP-HOOK")

  ;; Set to a sane value
  (in-package "COMMON-LISP-USER")

  (setf *entry-point* main)

  (save-lisp-and-die
   name
   :toplevel #'restart-image
   :compression compression
   :save-runtime-options save-runtime-options
   :precompile-generics precompile-generics
   :verbose (plusp *verbose*)))

(defun set-compilation-mode (compilation-mode)
  "Proclaim the optimization settings based on the COMPILATION-MODE."

  (vvv "Set compilation mode: ~S" compilation-mode)

  (destructuring-bind (spEed Debug saFety space Compilation-speed)
      (ecase compilation-mode ; E D F   C
        (:opt                 '(3 0 0 1 1))
        ((:fastbuild nil)     '(1 2 3 1 1))
        (:dbg                 '(1 3 3 1 1))
        (:load                '(1 1 1 1 3)))

    (set-interpret-mode compilation-mode)

    ;; Cause bodies of macroexpanders, including MACROLET and DEFINE-COMPILER-MACRO,
    ;; to be compiled in a policy in which these qualities override the global policy.
    #+sbcl (sb-ext:set-macro-policy '((speed 0) (safety 3)))

    (proclaim `(optimize (speed ,speed) (debug ,debug) (safety ,safety)
                         (space ,space) (compilation-speed ,compilation-speed)
                         ;; always insert array bounds, even in otherwise optimized code;
                         ;; optimizing this out was measured not to be worth the trouble.
                         #+sbcl(sb-c::insert-array-bounds-checks 3)))))

(defun add-feature (feature)
  "Add a single string FEATURE to *features*."
  (pushnew (to-keyword feature) *features*))

(defun add-features (string)
  "Add the features from the STRING first converting them into keywords."
  (let ((new-features (set-difference (mapcar #'to-keyword (split string)) *features*)))
    (vv "Adding features: ~S" new-features)
    (mapcar #'add-feature new-features)))

(defun check-features ()
  "Checks that build features are in good shape."
  (assert (not (and (member :opt *features*) (member :dbg *features*))))) ; NOLINT

(defun add-default-features (compilation-mode)
  "Add the default features to *features* including :google3 and COMPILATION-MODE."
  (declare (type (member :opt :fastbuild :dbg) compilation-mode))

  (case compilation-mode
    ((:opt :dbg) (add-feature compilation-mode)))

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
  ACTION - the current blaze action object,
  LOAD-MODE - the load mode used to load the file.
  MUFFLE-WARNINGS - if true, as in the case of deps, no warnings will be printed.
  READTABLE - is the readtable to be used while loading."
  (declare (type (or string pathname) name) (type action action))
  (when load-mode
    (with-standard-io-syntax
      (handler-bind ((non-fatal-error #'handle-error))
        (with-compilation-unit (:source-namestring name)
          (let* ((name (namestring name))
                 (*default-pathname-defaults* *default-pathname-defaults*)
                 (*current-source-file* name)
                 (*readtable* readtable)
                 (*print-readably* nil)
                 (*print-circle* t))
            (set-compilation-mode load-mode)
            (if muffle-warnings
                (with-all-warnings-muffled
                  (load (or fasl name) :external-format :utf-8))
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
  (with-standard-io-syntax
    (multiple-value-bind (fasl warnings-p failures-p)
        (with-compilation-unit (:source-namestring src)
          (let ((output-file (merge-pathnames output-file))
                (*default-pathname-defaults* *default-pathname-defaults*)
                (*readtable* readtable)
                (*print-readably* nil)
                (*print-circle* t))
            (compile-file src :output-file output-file
                              :emit-cfasl emit-cfasl
                              :external-format :utf-8)))
      (unless (and warnings-p failures-p)
        (vv "File ~S compiled without warnings." src))
      (when warnings-p
        (verbose "File ~S compiled with warnings." src))
      (when failures-p
        (fatal "File ~S failed to compile." src))
      (when save-locations
        (funcall-named* "BAZEL.PATH:SAVE-LOCATIONS" src output-file :readtable readtable))
      (values fasl warnings-p failures-p))))

(defun write-file-hash (src hash-file)
  "Compute the hash of the SRC file and write it to the HASH-FILE."
  (assert (equalp (pathname-type hash-file) "hash")) ; NOLINT
  (let ((md5sum (md5sum-file src)))
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
  (load-file file :action action :load-mode (action-fasl-load-mode action)))

(defmethod process-file ((action action) (file string) (type (eql :cfasl)))
  "Loads a CFASL file. Those are dependencies only loaded when compiling or building a binary."
  (unless (action-processing-sources-p action)
    (load-file file :action action :load-mode (action-fasl-load-mode action))))

(defmethod process-file ((action action) (file string) (type (eql :warnings)))
  "Loads a deferred warnings file. Deferred warnings are only checked in a binary (final) target."
  (cond ((eq (action-command action) :binary)
         ;; For binary target read the deferred warnings here so those can be checked
         ;; when the action is finalized later.
         (read-deferred-warnings action file))))

(defmethod process-file ((action action) (file string) (type (eql :hash)))
  "Loads an MD5 hash file."
  (with-open-file (in file :element-type 'octet)
    (let ((src (read-stringz in))
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
      (mapc #'process-file* deps)))
  (values))

;;;
;;; Command handlers
;;;

(defgeneric init-action (action command)
  (:documentation "Initializes the action based on the command")
  (:method ((action action) command) #| noop |#))

(defgeneric finish-action (action command)
  (:documentation "Given a finished BUILD action execute the final command."))

(defmethod finish-action ((action action) (command (eql :binary)))
  "Save the binary from this image."
  ;; TODO(jyknight/b/22174442): temporarily commented out; either fix QPX to
  ;; not require docstrings, or decide that deleting docstrings isn't useful.
  ;; (when (eq compilation-mode :opt)
  ;;   (delete-doc-strings))
  (nconcf (action-failures action)
          (resolve-deferred-warnings (action-deferred-warnings action)))
  (check-failures action)
  (check-features)

  (when (action-dump-alien-symbols-p action)
    (dump-alien-symbols (action-find-output-file action "aliensyms")))
  (when (action-dump-extern-symbols-file action)
    (dump-extern-symbols (action-dump-extern-symbols-file action)))
  (when (action-dump-dynamic-list-lds-file action)
    (dump-dynamic-list-lds (action-dump-dynamic-list-lds-file action)))

  ;; Assure things are in a defined state.
  ;; Save as an executable image. Exit.
  (save-binary (first (action-output-files action))
               (action-main-function action)
               :compression (action-compressed-p action)
               :save-runtime-options (action-save-runtime-options-p action)
               :precompile-generics (action-precompile-generics-p action)))

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

;;;
;;; Main Processing Loop
;;;

(defun process (&rest args
                   &key command deps load srcs outs gendir
                   (compilation-mode :fastbuild)
                   main features nowarn
                   compressed
                   precompile-generics
                   save-runtime-options
                   coverage
                   dump-alien-symbols
                   dump-extern-symbols
                   dump-dynamic-list-lds
                   emit-cfasl
                   deps-already-loaded
                   &allow-other-keys)
  "Main processing function for bazel.main.
 Arguments:
  ARGS - all the arguments,
  COMMAND - one of :binary or :compile,
  DEPS - dependencies,
  LOAD - files to be loaded after dependencies.
  SRCS - sources for a binary core or for compilation,
  OUTS - the output files,
  GENDIR - the directory for the generated results (for debug),
  COMPILATION-MODE - from blaze -c <compilation-mode>,
  MAIN - the name of the main function for a binary,
  FEATURES - features to be set before reading sources,
  NOWARN - list of warnings to be muffled,
  COMPRESSED - if the binary should be compressed,
  PRECOMPILE-GENERICS - if non-nil, precompile-generics before saving core,
  SAVE-RUNTIME-OPTIONS - will save the runtime options for the C runtime.
  COVERAGE - if the results should be instrumented with coverage information.
  DUMP-ALINE-SYMBOLS, DUMP-EXTERN-SYMBOLS, DUMP-DYNAMIC-LIST-LDS -
        dumps the C symbols that the Lisp sources depend on.
  EMIT-CFASL - will emit also .CFASL file in addition to the FASL file.
  DEPS-ALREADY-LOADED - true when files in DEPS are already loaded in the image."
  (let* ((command (to-keyword command))
         (deps (unless deps-already-loaded (split deps)))
         (load (split load))
         (srcs (split srcs))
         (outs (split outs))
         (compilation-mode (to-keyword compilation-mode))
         (action
           (make-action :args args
                        :command command
                        :output-files outs
                        :gendir gendir
                        :compilation-mode compilation-mode
                        :main-function main
                        :compressed-p compressed
                        :precompile-generics-p precompile-generics
                        :save-runtime-options-p save-runtime-options
                        :emit-cfasl-p emit-cfasl
                        :record-path-location-p (and coverage t)
                        ;; Load lisp dependencies when compiling or making srcs image
                        :lisp-load-mode (when (member command '(:binary :compile)) :load)
                        ;; Load fasl dependencies when compiling or creating a binary image.
                        :fasl-load-mode (when (member command '(:binary :compile)) compilation-mode)
                        :dump-alien-symbols-p dump-alien-symbols
                        :dump-extern-symbols-file dump-extern-symbols
                        :dump-dynamic-list-lds-file dump-dynamic-list-lds))

         (*compile-verbose* (>= *verbose* 1))
         (*compile-print* (>= *verbose* 3))
         (*load-verbose* (>= *verbose* 2))
         (*load-print* (>= *verbose* 3)))

    (declare (list deps srcs outs))

    (when (>= *verbose* 1)
      (let ((args (copy-list args)))
        (unless (or (>= *verbose* 2) (= (length deps) 1))
          (remf args :deps)
          (nconcf args (list :deps (length deps))))
        (unless (or (>= *verbose* 2) (= (length srcs) 1))
          (remf args :srcs)
          (nconcf args (list :srcs (length srcs))))

        (verbose "Params:~{~&~3T~A: ~A~%~}" args)

        #+sbcl
        (when (>= *verbose* 2)
          (verbose "Environment:~{~%~3T~S~}~%" (sb-unix::posix-environ)))))

    ;; Rebind globally.
    (setf *action* action)

    (when (>= *verbose* 1)
      (flet ((strip-gendir (name) (if gendir (strip-prefix gendir name) name)))
        (cond ((>= *verbose* 2)
               (vv "Deps:~{~%~3T~A~}" (mapcar #'strip-gendir deps))
               (vv "Srcs:~{~%~3T~A~}" (mapcar #'strip-gendir srcs)))
              (t
               (verbose "Deps: ~A" (length deps))
               (verbose "Srcs: ~A" (length srcs))))
        (verbose "Outs:~{~%~3T~A~}" (mapcar #'strip-gendir outs))))

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
    (action-add-nowarn 'bazel.warning:uninteresting-condition)
    (action-add-nowarn #'defer-undefined-warning)

    ;; Feature available only in google3 depot.
    (when coverage
      (funcall-named "BAZEL.COVERAGE:TURN-ON-DATA-COLLECTION"))

    (process-dependencies deps)

    (handler-bind ((condition #'handle-warning)
                   (non-fatal-error #'handle-error))
    (verbose "Loading ~D source file~:P..." (length load))
      (mapc #'process-file* load)
      ;; Switch to source file processing.
      (setf (action-processing-sources-p action) t)
      (verbose "Processing ~D source file~:P..." (length srcs))
      (mapc #'process-file* srcs)
      (verbose "Finalizing the ~A action..." command)
      (set-compilation-mode (action-compilation-mode action))
      (finish-action action command))

    (verbose "BAZEL ~A finished" command)))

;;;
;;; Used to combine images
;;;

(defun combine (&key command run-time core output &allow-other-keys)
  "Combines the RUN-TIME with the CORE and saves it to OUTPUT."
  (assert (eq :combine command)) ; NOLINT
  (combine-run-time-and-core run-time core output))

;;;
;;; Main entry point
;;;

(defvar *shortcuts*
  '(("-c" :compilation-mode)
    ("-v" :verbose)
    ("-f" :features)
    ("-W" :nowarn)
    ("-I" :interactive)))

(defun to-keyword-arg (thing &optional shortcuts)
  "Converts a command line argument option name to a keyword."
  (declare (string thing))
  (and thing
       (or (second (assoc thing shortcuts :test #'equal))
           (to-keyword (if (prefixp "--" thing)
                           (subseq thing 2)
                           thing)))))

(defun parse-command-args (args &optional shortcuts)
  "Parses the command-line and returns args as list of keyword value pairs."
  (list* :command (to-keyword (pop args))
         (loop while args
               for arg = (to-keyword-arg (pop args) shortcuts)
               when arg
                 nconc (list arg (or (null args)
                                     (prefixp "-" (car args))
                                     (pop args))))))
(defun main ()
  "Main entry point."
  (let ((command-args (parse-command-args (command-line-arguments) *shortcuts*)))
    (destructuring-bind (&key verbose interactive &allow-other-keys) command-args
      (when verbose
        (setf *verbose* (read-from-string verbose)))
      (set-interactive-mode interactive))

    (verbose "Program name: ~A" (program-name))
    (verbose "Current dir: ~A" *default-pathname-defaults*)
    (vv "Command line: ~{'~A'~^ ~}" (command-line-arguments))

    (case (getf command-args :command)
      (:combine (apply 'combine command-args))
      (t        (apply 'process command-args)))))
