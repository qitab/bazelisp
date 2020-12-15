# Copyright 2015-2020 Google LLC
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.

# Author: andrzejwalczak@google.com (Andrzej Walczak)
# Description:
# The package implements the Common Lisp build extension for Bazel.
# This BUILD file describes a Lisp compilation image used with the
# Starlark rules which are defined in rules.bzl. See README.md for
# more info about this package.

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("@bazel_skylib//rules:common_settings.bzl", "bool_flag")
load("@io_bazel_stardoc//stardoc:stardoc.bzl", "stardoc")

licenses(["notice"])

exports_files(["LICENSE"])

bzl_library(
    name = "build_rules",
    srcs = glob(["*.bzl"]),
    visibility = ["//visibility:public"],
    deps = [
        "@rules_cc//cc:find_cc_toolchain.bzl",
        "@bazel_skylib//rules:common_settings",
    ],
)

stardoc(
    name = "rules_stardoc",
    out = "rules.md",
    header_template = "stardoc_header.vm",
    input = "rules.bzl",
    deps = [":build_rules"],
)

sh_test(
    name = "rules_stardoc_test",
    srcs = ["rules_stardoc_test.sh"],
    data = [
        "doc/rules.md",
        "//:rules.md",
    ],
)

SBCL = "//lisp/sbcl"
CORE = "//lisp/sbcl:sbcl.core"

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
        CORE,
    ],
    outs = ["image"],
    cmd = (
        "$(location {})".format(SBCL) +
        "--core $(location {})".format(CORE) +
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

bool_flag(
    name = "additional_dynamic_load_outputs",
    build_setting_default = False,
    visibility = ["//visibility:public"],
)
