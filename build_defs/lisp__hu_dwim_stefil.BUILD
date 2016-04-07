# Description: Stefil unit testing library for Common Lisp, hu.dwim variant.

licenses(["unencumbered"])  # public domain

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "hu_dwim_stefil",
    srcs = [
        # Order is important here.
        "source/package.lisp",
        "source/duplicates.lisp",
        "source/infrastructure.lisp",
        "source/asserts.lisp",
        "source/test.lisp",
        "source/suite.lisp",
        "source/fixture.lisp",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@lisp__alexandria//:alexandria",
    ],
)

# TODO: add support for the other .asd files there.
