;; Exercise C++ name mangler

(defparameter *testcases*
  ;;
  ;; Each test case is (expectation name-parts . signature)
  ;;  EXPECTATION is what c++filt should print when given the mangled test case.
  ;;  NAME-PARTS represent a possibly-namespace-qualified C++ name using an S-expression.
  ;;  SIGNATURE comprises the argument types that become part of the mangled name.
  ;;
  '(("do_math(int, long, unsigned long, double)" ; some basics
     "do_math"
     integer long unsigned-long double)

    ("my_namespace::inner_namespace::foofun(int, void*, char**)" ; namespace
     ("my_namespace" "inner_namespace" "foofun")
     integer (* void) (* * character))

    ("base_logging::SetLogFilenameExtension(char const*)" ; const modifier
     ("base_logging" "SetLogFilenameExtension")
     (* const character))

    ("Process::HandleSignal(int, siginfo_t*)"  ; opaque type
     ("Process" "HandleSignal")
     integer (* "siginfo_t"))

    ("InitSomething(char const*, int*, char***, bool)"
     "InitSomething"
     (* const character) (* integer) (* * * character) boolean))
  "Tests")

(defun main ()
  (dolist (test *testcases*)
    (destructuring-bind (expectation name-parts . args) test
      (let* ((mangled (bazel.sbcl:c++-mangle name-parts args))
             (demangler
              (sb-ext:run-program "c++filt" (list mangled) :output :stream :search t))
             (demangled (read-line (process-output demangler))))
        (process-close demangler)
        (assert (string= demangled expectation))))))
