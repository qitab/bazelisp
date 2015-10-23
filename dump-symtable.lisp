;;; Output an asm file with symbol definitions for all lisp functions.  This allows system
;;; profilers to do things like dump a symbolized backtrace for our lisp code.

;;; This script is intended to be run on an already-dumped lisp image, to output an
;;; assembly file with symbol definitions for all of the functions in the lisp image. Note
;;; that the lisp functions dumped into the image are mmapped at a constant location, and
;;; do not get GC'd or moved.

;;; No code is output in the asm file, just symbol definitions. This asm file is then
;;; linked into the C runtime by the blaze lisp_binary rule.

;; Keep track of which names have been output already, so that duplicate symbols aren't
;; emitted.
(defvar *used-names*)

;; The allocation pointer is initially at the beginning of free space, right after the end
;; of the mmap'ed image file.  This is a hacky, but good enough, way of excluding the
;; runtime-compiled functions in the following script from the output.
(defvar *dynamic-space-image-end* (sb-kernel:get-lisp-obj-address #.sb-vm::*allocation-pointer*))

(defun lambda-args (args)
  "Returns the parameter symbols of a lambda list ARGS."
  (loop for arg in args
        collect (if (atom arg) arg (car arg))))

(defun clean-name-obj (name)
  "Remove useless forms from the debug NAME of a function."
  (let ((name (if (and (consp name)
                       (member (first name)
                               '(sb-c::xep sb-c::tl-xep sb-c::&more-processor
                                 sb-c::&optional-processor)))
                  (second name)
                  name))
        (*package* (find-package "COMMON-LISP"))
        (*print-gensym* nil)
        (*print-case* :downcase))

    (cond ((stringp name)
           name)
          ((not (consp name))
           (format nil "~S" name))

          ((and (eq (first name) 'lambda) (stringp (fourth name)))
           (format nil "(lambda~{ ~A~})" (lambda-args (second name))))
          ((eq (first name) 'lambda)
           (format nil "~@[~S:~](lambda~{ ~A~})" (fourth name) (lambda-args (second name))))

          ((and (member (first name) '(flet labels)) (stringp (fourth name)))
           (format nil "flet:~S" (second name)))
          ((member (first name) '(flet labels))
           (format nil "~@[~S:~]~S" (fourth name) (second name)))

          ((eq (first name) 'sb-c::top-level-form)
           (format nil "top-level:~A" (first (second name))))
          (t
           (format nil "~{~S~^ ~}" name)))))

(defun clean-char (char)
  "Turn the CHAR into an acceptable by the assembler."
  (cond ((or (member char '(#\. #\_ #\$))
             (and (<= (char-int char) 128) (alphanumericp char)))
         char)
        ((eq char #\:)
         #\.)
        (t
         #\_)))

(defun remove-char-dups (string &optional (chars '(#\. #\_)))
  "Removes duplicated characters from STRING if found in the CHARS list."
  ;; TODO(czak): This should not be necessary once the chars can be escaped.
  (declare (simple-string string))
  (coerce (the list (loop for prev-char = (first chars) then char
                          for char across string
                          unless (and (char= prev-char char) (member char chars))
                            collect char))
          'string))


(defun prefixp (prefix string)
  "Test if STRING starts with the PREFIX."
  (declare (string string prefix))
  (let ((len (length prefix)))
    (and (<= len (length string)) (string= string prefix :end1 len))))

(defun clean-name (name)
  "Turn the function NAME symbol/list into a string,
  and clean up the character set so that it is acceptable to the assembler."
  ;; TODO(jyknight): When binutils 2.24 is used, we can generate
  ;; symbols with other characters in the names (e.g. dash and
  ;; parens), by quoting the symbol. However, until then, quoted
  ;; symbols are not accepted, so translate to the supported
  ;; character-set.
  (let ((name (map 'string #'clean-char (clean-name-obj name))))
    (when (plusp (length name))
      ;; A unix token gets replaced with " 1 ".
      (when (prefixp "unix." name)
        (setf name (replace name "unix_")))
      (let ((.unix. (search ".unix." name)))
        (when .unix.
          (setf name (replace name "_unix_" :start1 .unix.))))
      (when (or (digit-char-p (char name 0)) (member (char name 0) '(#\. #\$)))
        (setf name (format nil "_~A" name)))
      (remove-char-dups name))))

(defun clean-file-name (name)
  "Removes hashed directory prefixes from NAME."
  (declare (type (or null simple-string) name))

  ;; TODO(czak): Fix ASDF cache paths for contribs.
  (cond ((null name) nil)

        ((prefixp "SYS:SRC;" name)
         (format nil "third_party/lisp/sbcl/src/src/~(~A~)"
                 (substitute #\/ #\; (subseq name 8))))

        ((prefixp "SYS:CONTRIB;" name)
         (format nil "third_party/lisp/sbcl/src/contrib/~(~A~)"
                 (substitute #\/ #\; (subseq name 12))))

        ((and (prefixp "blaze-out" name)
              (search "/genfiles/" name))
         ;; Adjust file names for fasl compiled functions.
         (subseq name (+ (search "/genfiles/" name) 10)))

        ((and (prefixp "/tmp/" name)
              (let ((/src/ (search "/src/" name)))
                ;; Remove SBCL build hash dir names.
                (when /src/
                  (format nil "third_party/lisp/sbcl~A" (subseq name /src/))))))

        ((and (prefixp "/build/work/" name)
              (search "/google3/" name))
         ;; Remove Forge build hash dir names.
         (subseq name (+ (search "/google3/" name) 9)))

        ((prefixp "/build/" name)
         ;; Unknown forge build hash pattern.
         nil)

        (t
         name)))

(defun derive-file-name (file-name fun-name)
  "Clean the FILE-NAME. FUN-NAME contains the file-name for lambdas."
  (clean-file-name
   (if (and (null file-name) (consp fun-name)
            (eq (first fun-name) 'lambda)
            (stringp (fourth fun-name)))
       (fourth fun-name)
      file-name)))

(defun emit (file-name fun-name start end)
  "Emit one symbol's information to stdout. FILE-NAME and FUN-NAME describe the symbol.
 START and END describe the symbol code starting and ending address."
  ;; TODO(czak): Make gas emit file info for absolute symbols.
  ;;  .file will not emit debug info unless text coded is included.
  (let* ((file-name (pathname-name (or (derive-file-name file-name fun-name) "lisp")))
         (name (format nil "~A$~A$" (clean-name fun-name) (clean-name file-name)))
         (dupcount (incf (the fixnum (gethash name *used-names* 0))))
         (name (if (> dupcount 1) (format nil "~A~D" name dupcount) name)))
    (format t "~A =0x~X~%.size ~A, ~D~%.type ~A, @function~%" name start name (- end start) name)))


(defun dump-code-component-symbols (code)
  "Given a CODE lisp object, emit the names of all functions contained within
  it. A code component is, roughly, one toplevel defun, and it can have multiple functions
  if there are local functions within."
  (let ((info (sb-kernel:%code-debug-info code)))
    (unless info (return-from dump-code-component-symbols nil))

    (let* ((code-header-len (* (sb-kernel:get-header-data code) sb-vm:n-word-bytes))
           (pc-offset (+ (- (sb-kernel:get-lisp-obj-address code)
                            sb-vm:other-pointer-lowtag)
                         code-header-len))
           (fun-map (sb-c::compiled-debug-info-fun-map info))
           (len (length fun-map))
           (last-name nil)
           (last-start 0)
           (last-end 0)
           (source (sb-c::compiled-debug-info-source info))
           (file-name (and source (clean-file-name (sb-c::debug-source-namestring source)))))
      (declare (type simple-vector fun-map))

      (flet ((emit-dedup (name start end)
               "Save up last name so that if the next contiguous area
                has the same name, it turns into a single symbol"
               (if (and (equal name last-name) (= start last-end))
                   (setf last-end end)
                   (progn
                     (when last-name
                       (emit file-name last-name last-start last-end))
                     (setf last-name name
                           last-start start
                           last-end end)))))
        ;;(format "START: ~D ~D~%" code-header-len pc-offset)
        (loop for start = 0 then end
              for i from 1 by 2 below (1+ len)
              for end = (if (< i len)
                            (svref fun-map i)
                            (* (sb-vm::%code-code-size code) sb-vm:n-word-bytes))
              for debug-fun = (svref fun-map (1- i))
              for name = (sb-c::compiled-debug-fun-name debug-fun)
              ;;do (format t "FOO: ~D ~D ~D~%" start i end)
              when name do (emit-dedup name (+ pc-offset start) (+ pc-offset end)))
        ;; emit final saved name
        (emit-dedup nil 0 0)))))

(defun dump-symtable ()
  "Dump all code-components found by walking the image-backed part of the dynamic space."
  (let ((*used-names* (make-hash-table :test #'equal)))
    (flet ((dump-code-component (obj type size) (declare (ignore size))
             (when (and (< (sb-kernel:get-lisp-obj-address obj) *dynamic-space-image-end*)
                        (eql type sb-vm:code-header-widetag))
               (dump-code-component-symbols obj))))
      (sb-vm::map-allocated-objects #'dump-code-component :dynamic))))

(dump-symtable)
;; Magic declaration to tell the assembler we don't need an executable stack.
(format t ".section .note.GNU-stack,\"\",@progbits~%")
