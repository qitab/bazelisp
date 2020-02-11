# Bazel Lisp Build Rules

This directory contains the core implementation of build rules
and toolchain for building Common Lisp code with SBCL and Bazel. An early
version of that is open-sourced
([GitHub qitab/bazelisp](https://github.com/qitab/bazelisp)), but subsequent
changes have not been released externally.

TODO(b/24678256): Get this released again, and ensure changes are regularly
released.

google3-specific wrappers for these rules live in google3/lisp/devtools/bazel/.
Basic usage documentation can be found in go/lisp. Full documentation of the
parameters of `lisp_library`, `lisp_binary`, and `lisp_test` can be found in
google3/third_party/lisp/bazel/rules.bzl.
