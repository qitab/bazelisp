# Copyright 2015 Google Inc.  All rights reserved.
# Author: andrzejwalczak@google.com (Andrzej Walczak)
# Description:
# The package implements the Lisp plugin to the Bazel.
# This BUILD file describes a Lisp side driver used
# with the Skylark rules which are defined in rules.bzl
# See README for more info about this package.

licenses(["unencumbered"])  # New BSD, Google-authored

exports_files(["LICENSE"])

exports_files(["dump-symtable.lisp"])

filegroup(
    name = "build_rules",
    srcs = glob(["*.bzl"]),
    visibility = ["//visibility:public"],
)

# TODO(czak): This needs to be set to some path reachable from Bazel.
SBCL = "//third_party/lisp/sbcl/binary-distribution/k8:sbcl"

genrule(
    name = "make-bazel",
    srcs = [
        SBCL,
        "utils.lisp",
        "warning.lisp",
        "log.lisp",
        "sbcl.lisp",
        "main.lisp",
    ],
    outs = ["bazel"],
    cmd = (
        "SBCL_HOME=`pwd`/$(location %s)/lib/sbcl" % SBCL +
        " $(location %s)/bin/sbcl" % SBCL +
        " --noinform" +
        " --eval '(setf sb-ext:*evaluator-mode* :compile)'" +
        " --load '$(location utils.lisp)'" +
        " --load '$(location warning.lisp)'" +
        " --load '$(location log.lisp)'" +
        " --load '$(location sbcl.lisp)'" +
        " --load '$(location main.lisp)'" +
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
