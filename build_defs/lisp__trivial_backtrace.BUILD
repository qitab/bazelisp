# Description: library for printing backtraces
# NB: Wasn't it obsoleted by UIOP?

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

licenses(["notice"]) # MIT

lisp_library(
    name = "trivial_backtrace",
    srcs = [
        # Serial
        "dev/packages.lisp",
        "dev/utilities.lisp",
        "dev/backtrace.lisp",
        "dev/map-backtrace.lisp",
        "dev/fallback.lisp",
    ],
    nowarn = [
        "sb-ext:early-deprecation-warning",
    ],
    visibility = ["//visibility:public"],
)
