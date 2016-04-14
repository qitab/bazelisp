# Copyright 2015 Google Inc.  All rights reserved.
# Author: andrzejwalczak@google.com (Andrzej Walczak)
# Description:
# This package implements Common Lisp support for Bazel.
#
# README.md contains an introduction to this package and how to use it.
# doc/ contains a higher-level description of what this is all about.
# bazel/ contains the Lisp driver and Skylark rules.
# build_defs/ contains BUILD and support files for various Lisp projects.
# test/ contains various unit tests for the Bazel support for Common Lisp.
# hello/ contains a trivial example package that uses this Lisp support.
# lisp.WORKSPACE.bzl contains external repository definitions to load from your main WORKSPACE.
# WORKSPACE declares to Bazel the name of this external repository.
# LICENSE is a copy of the Apache License version 2.0, which applies to this package.
# TODO.md contains various TODO items for improving Common Lisp support for Bazel.

licenses(["notice"])  # Apache 2, Google-authored

exports_files(["LICENSE", "bazel/dump-symtable.lisp"] + glob(["build_defs/*.*.*"]))

load(":bazel/rules.bzl", "SBCL_PACKAGE")

genrule(
    name = "make-bazel",
    srcs = [
        SBCL_PACKAGE + "sbcl",
        SBCL_PACKAGE + "sbcl-lib",
        SBCL_PACKAGE + "lib/sbcl/sbcl.core",
        "bazel/utils.lisp",
        "bazel/warning.lisp",
        "bazel/log.lisp",
        "bazel/sbcl.lisp",
        "bazel/main.lisp",
    ],
    outs = ["bazel"],
    cmd = (
        "SBCL_HOME=`pwd`/`dirname $(location %slib/sbcl/sbcl.core)`" % SBCL_PACKAGE +
        " $(location %ssbcl)" % SBCL_PACKAGE +
        " --noinform" +
        " --eval '(setf sb-ext:*evaluator-mode* :compile)'" +
        " --load '$(location bazel/utils.lisp)'" +
        " --load '$(location bazel/warning.lisp)'" +
        " --load '$(location bazel/log.lisp)'" +
        " --load '$(location bazel/sbcl.lisp)'" +
        " --load '$(location bazel/main.lisp)'" +
        " --eval '(bazel.main:save-binary \"$@\" (quote bazel.main:main))'"
    ),
    executable = 1,
    output_to_bindir = 1,
    visibility = ["//visibility:public"],
)

cc_library(
    name = "bazel.cdeps",
    visibility = ["//visibility:public"],
)
