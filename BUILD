# Copyright 2015 Google Inc.  All rights reserved.
# Author: andrzejwalczak@google.com (Andrzej Walczak)
# Description:
# The package implements the Lisp plugin to the Bazel.
# This BUILD file describes a Lisp side driver used
# with the Skylark rules which are defined in rules.bzl
# See README for more info about this package.

licenses(["unencumbered"])  # New BSD, Google-authored

exports_files(["LICENSE"])

exports_files(["bazel/dump-symtable.lisp"])

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

# This is no longer needed.
#py_binary(
#    name = "combine-sbcl-image",
#    srcs = ["sbcl_support/combine-sbcl-image.py"],
#    visibility = ["//visibility:public"],
#)

filegroup(
    name = "make-sbcl-genrule",
    srcs = ["sbcl_support/make-sbcl-genrule.sh"],
    visibility = ["//visibility:public"],
)
