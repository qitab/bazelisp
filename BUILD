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

load("@bazel_tools//tools/build_defs/license:license.bzl", "license")
load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("@bazel_skylib//rules:common_settings.bzl", "bool_flag")
load("@io_bazel_stardoc//stardoc:stardoc.bzl", "stardoc")

package(default_applicable_licenses = ["//:license"])

license(
    name = "license",
    package_name = "bazel",
)

licenses(["notice"])

exports_files(["LICENSE"])

bzl_library(
    name = "build_rules",
    srcs = glob(["*.bzl"]),
    visibility = ["//visibility:public"],
    deps = [
        "@bazel_skylib//rules:common_settings",
        "@rules_cc//cc:find_cc_toolchain.bzl",
    ],
)

stardoc(
    name = "rules_stardoc",
    out = "rules.md",
    header_template = "stardoc_header.vm",
    input = "rules.bzl",
    deps = [":build_rules"],
)

# This is not the same as @bazel_tools//tools/cpp:msan_build, but that matches whether the
# build is run with --config=msan at all, as opposed to whether particular
# targets are actually compiled in msan (e.g. anything in host config is not).
config_setting(
    name = "msan_compiler",
    flag_values = {"@bazel_tools//tools/cpp:compiler": "msan"},
    visibility = ["//visibility:private"],
)

config_setting(
    name = "msan-track-origins_compiler",
    flag_values = {"@bazel_tools//tools/cpp:compiler": "msan-track-origins"},
    visibility = ["//visibility:private"],
)

alias(
    name = "msan",
    actual = select({
        ":msan_compiler": ":msan_compiler",
        ":msan-track-origins_compiler": ":msan-track-origins_compiler",
        "//conditions:default": ":msan_compiler",  # arbitrary non-matching choice
    }),
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
        "@local_sbcl//:contrib/sb-md5",
        "@local_sbcl//:contrib/sb-rotate-byte",
        "@local_sbcl//:core",
        "@local_sbcl//:sbcl",
    ],
    outs = ["image"],
    cmd = (
        "$(location @local_sbcl//:sbcl)" +
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

# Elfinator reads an SBCL-native core file and produces two outputs:
# (1) an assembly-language file with a '.text' section whose contents
#     are the code components from the input core file.
#     This file is fully relocatable at link time.
# (2) a '.o' file containing all other Lisp objects.
# The assembly file is compiled, and then the resulting '.o' as well
# as the '.o' from elfinator can both be used as srcs to a cc_binary
# which depends on ":c-support". The SBCL runtime auto-detects
# presence of text-in-ELF and data-in-ELF.
sh_binary(
    name = "elfinate",
    srcs = [
        "elfconvert.sh",
    ],
    data = [
        "@local_sbcl//:core",
        "@local_sbcl//:sbcl",
        "@sbcl//:tools-for-build/corefile.lisp",
        "@sbcl//:tools-for-build/editcore.lisp",
        "@sbcl//:tools-for-build/elftool.lisp",
    ],
    visibility = ["//visibility:public"],
)

bool_flag(
    name = "additional_dynamic_load_outputs",
    build_setting_default = False,
    visibility = ["//visibility:public"],
)
