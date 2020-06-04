# Copyright 2015 Google Inc.  All rights reserved.
# Author: andrzejwalczak@google.com (Andrzej Walczak)
# Description:
# The package implements the Lisp plugin to the Bazel.
# This BUILD file describes a Lisp side driver used
# with the Skylark rules which are defined in rules.bzl
# See README.md for more info about this package.

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")

licenses(["unencumbered"])

exports_files(["LICENSE"])

bzl_library(
    name = "build_rules",
    srcs = glob(["*.bzl"]),
    visibility = ["//visibility:public"],
    deps = [
        "@rules_cc//cc:action_names.bzl",
        "@rules_cc//cc:find_cc_toolchain.bzl",
    ],
)

SBCL = "@local_sbcl//:sbcl_fileset"

# Using "--define=LISPCORE=<something else>" will allow using a different core
# to build lisp.
vardef("LISPCORE", "sbcl.core")

# This is not the same as @bazel_tools//tools/cpp:msan_build, but that matches whether the
# build is run with --config=msan at all, as opposed to whether particular
# targets are actually compiled in msan (e.g. anything in host config is not).
config_setting(
    name = "msan",
    flag_values = {"@bazel_tools//tools/cpp:compiler": "msan"},
    visibility = ["//visibility:public"],
)

genrule(
    name = "make-image",
    srcs = [
        "utils.lisp",
        "warning.lisp",
        "log.lisp",
        "sbcl.lisp",
        "main.lisp",
        SBCL,
    ],
    outs = ["image"],
    cmd = (
        "$(location {})/bin/sbcl".format(SBCL) +
        " --core $(location {})/lib/sbcl/$(LISPCORE)".format(SBCL) +
        " --noinform" +
        " --eval '(setf sb-ext:*evaluator-mode* :compile)'" +
        " --load '$(location utils.lisp)'" +
        " --load '$(location warning.lisp)'" +
        " --load '$(location log.lisp)'" +
        " --load '$(location sbcl.lisp)'" +
        " --load '$(location main.lisp)'" +
        " --eval '(bazel.main:save-image \"$@\" (quote bazel.main:main) :executable t)'"
    ),
    executable = 1,
    output_to_bindir = 1,
    visibility = ["//visibility:public"],
)

cc_library(
    name = "image.cdeps",
    visibility = ["//visibility:public"],
)

cc_library(
    name = "sbcl-linkable-runtime",
    srcs = ["@local_sbcl//:linkable-runtime"],
    linkopts = ["-ldl"],
    linkstatic = 1,
    visibility = ["//visibility:public"],
)
