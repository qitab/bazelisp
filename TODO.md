TODO
====

Things to do to improve Common Lisp support for Bazel:

  * Fix the following repos:

      * `lisp__cxml` (can't locate its `catalog.dtd`).
      * `lisp__mixalot` (requires a `cffi_grovel` rule).
      * `lisp__slime` (missing `google-init.lisp`).
      * `swank-client` (depends on `lisp__slime`).
      * Anything with a TODO entry.

  * Support a `lisp_library` with empty `srcs` (then fix the `fare-quasiquote` BUILD).

  * Add a rule `cffi_grovel` for cffi-grovel generated files.

  * Add interactive support so that you can build Bazel targets from the Lisp REPL,
    and load any updated code into the current image as `.fasl` or `.so` files.

  * Add a bridge so ASDF systems can depend on Bazel BUILDs.

  * Add a converter to convert simple ASDF systems into Bazel BUILDs.

  * Add a `lisp_genrule` that will execute arbitrary Lisp code rather shell code,
    to otherwise generate arbitrary files.

  * Provide a lightweight way to compile Lisp-only BUILDs without involving
    a multi-gigabyte Java application.

  * Convert more Common Lisp systems to use Bazel.

  * Develop a way to track changes, update packages, etc.
    Make it based on Quicklisp?
