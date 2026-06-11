;;; Copyright 2015-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Utilities for Bazel Lisp and their implementation in SBCL.
;;;

(defpackage #:bazel.sbcl
  (:use #:common-lisp #:sb-thread #:sb-alien)
  (:export #:always-block-compile-file-p
           #:compile-files
           #:c++-mangle
           #:c-symbol-existsp
           #:exit
           #:run
           #:inline-function-p
           #:getenv #:unsetenv
           #:command-line-arguments #:program-name
           #:default-toplevel-loop
           #:mute-output-streams
           #:save-lisp-and-die
           #:md5sum-file
           #:set-interpret-mode
           #:remove-extra-debug-info
           #:name-closure
           #:with-creating-find-package
           #:with-default-package))

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

(defun inline-function-p (function)
  "Returns non-nil when the FUNCTION is declared inline."
  (eq (sb-int:info :function :inlinep function) 'inline))

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

(defun name-closure (closure name)
  "Return CLOSURE with the NAME changed, so it prints nicely."
  ;; This is not necessary, except for debugging and aesthetics.
  (setf (sb-kernel:%fun-name closure) name)
  closure)

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
      (values count all))))

;;;
;;; Generate an image.
;;;

(defun set-interpret-mode (compile-mode)
  "Set the mode of eval to :interpret if COMPILE-MODE is :LOAD. Otherwise, set it to :COMPILE."
  (declare (optimize (speed 1) (safety 3) (compilation-speed 1) (debug 1)))
  (setf sb-ext:*evaluator-mode* (if (eq compile-mode :load) :interpret :compile))
  sb-ext:*evaluator-mode*)

;;;
;;; Reading lisp files.
;;;

(defun always-block-compile-file-p (file)
  "Return true if :block-compile should be enabled for FILE"
  (declare (ignore file))
  nil)

(defun compile-files (names &rest rest)
  "Call COMPILE-FILE on NAMES, which must be singular despite being named NAMES,
passing through REST unaltered."
  (if (typep names '(or atom (cons string null)))
      (let ((source (if (atom names) names (car names))))
        (when (find-package "SB-COVER")
          ;; no effect if coverage isn't enabled
          (funcall (find-symbol "ENABLE-COVERAGE-LOGGING" "SB-COVER")))
        (apply #'compile-file source
               :block-compile (or (getf rest :block-compile)
                                  (always-block-compile-file-p source))
               rest))
      (error "Multiple file support is incomplete")))

(defun md5sum-file (file)
  "Run external md5sum program on FILE"
  (let ((process (sb-ext:run-program "md5sum" (list (namestring (merge-pathnames file)))
                                     :output :stream :search t)))
    (assert (zerop (sb-ext:process-exit-code process)))
    (let ((hex (subseq (read-line (sb-ext:process-output process)) 0 32))
          (result (make-array 16 :element-type '(unsigned-byte))))
    (dotimes (i 16 result)
      (setf (aref result i)
            (parse-integer hex :start (* i 2) :end (* (1+ i) 2) :radix 16))))))

;;; This belongs somewhere in SB-ALIEN, but only in theory, because the mangling
;;; algorithm depends technically on the C compiler. It so happens that we use LLVM
;;; which uses the mangling specification developed for Itanium. The real mangler
;;; takes over 6 thousand lines of code to express. This is a far cry from that.
(defun c++-mangle (name arg-types &optional const) ; NOLINT
  "Produce the C linkage name for C++ function NAME with ARG-TYPES"
  (labels ((typemangle (spec)
             (apply #'concatenate 'string
                    (mapcar #'mangle-modifier (sb-int:ensure-list spec))))
           (mangle-modifier (x)
             (string
              (cond ((case x
                       (* #\P)
                       (integer #\i)
                       (sb-alien:long #\l)
                       (sb-alien:unsigned-long #\m)
                       (sb-alien:double #\d)
                       (sb-alien:void #\v)
                       (character #\c)
                       (boolean #\b)))
                    ;; package-insensitive comparison
                    ((string= x "CONST") #\K)
                    ((string= x "REF") #\R)
                    ((string= x "string_view")
                     ;; the mangled string demangles to
                     ;;   "std::__u::basic_string_view<char, std::__u::char_traits<char> >"
                     ;; #\N => nesting, "St" => std::, #\I => template parameter list
                     ;; "S_" => backreference to std::__u and so on
                     (if (member :msan *features*)
                         "NSt6__msan17basic_string_viewIcNS_11char_traitsIcEEEE"
                         "NSt3__u17basic_string_viewIcNS_11char_traitsIcEEEE"))
                    ((stringp x) (format nil "~D~A" (length x) x))
                    (t (error "Unhandled C++ name"))))))
    (format nil "_Z~A~{~A~}"
            (if (stringp name)
                (format nil "~D~A" (length name) name)
                (with-output-to-string (s)
                  (write-char #\N s)
                  (when const (write-char #\K s))
                  (dolist (part name) (format s "~D~A" (length part) part))
                  (write-char #\E s)))
            (if arg-types (mapcar #'typemangle arg-types) '(#\v)))))

(defun c-symbol-existsp (sym)
  "True if and only if &SYM is nonzero in an ELF binary"
  (let ((alien-linkage-space-start (find-symbol "ALIEN-LINKAGE-SPACE-START" "SB-VM")))
    (unless alien-linkage-space-start
      (return-from c-symbol-existsp nil))
    (macrolet ((compute-offset ()
                 (let ((accessor (find-symbol "ALIEN-LINKAGE-ELEMENT-OFFSET" "SB-VM")))
                   (if accessor
                       `(,accessor index t)
                       `(+ (* index sb-vm:alien-linkage-table-entry-size) 8)))))
      (let ((index (gethash sym (car sb-sys:*linkage-info*))))
        (and index
             ;; check that alien linkage table entry doesn't point to undefined-tramp
             (/= (sb-sys:sap-ref-word (sb-sys:int-sap (symbol-value alien-linkage-space-start))
                                      (compute-offset))
                 (sb-fasl:get-asm-routine (or (find-symbol "UNDEFINED-ALIEN-TRAMP" "SB-VM")
                                              (find-symbol "UNDEFINED-TRAMP" "SB-VM")))))))))

(defun maybe-save-coverage ()
  "If ${COVERAGE} is set, writes coverage data to a file in ${COVERAGE_DIR}. The file has the .dat
  extension and is in LCOV format. By convention, for Lisp coverage the filname starts with
  'lispcov'."
  (when (getenv "COVERAGE")
    (let ((coverage-dir (getenv "COVERAGE_DIR")))
      (unless coverage-dir
        (error "COVERAGE is set, but COVERAGE_DIR is not."))
      (funcall (find-symbol "LCOV-REPORT" "SB-COVER")
       (format nil "~A/lispcov-~A.dat" coverage-dir (sb-unix:unix-getpid))))))

(pushnew 'maybe-save-coverage sb-ext:*exit-hooks*)
