# Description: Common Lisp alexandria library.

licenses(["unencumbered"])  # public domain

exports_files(["LICENSE"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

## Simple solution: compile everything in order
#lisp_library(
#    name = "alexandria",
#    srcs = [
#        # order matters
#        "package.lisp",
#        "binding.lisp",
#        "conditions.lisp",
#        "definitions.lisp",
#        "strings.lisp",
#        "symbols.lisp",
#        "macros.lisp",
#        "control-flow.lisp",
#        "functions.lisp",
#        "hash-tables.lisp",
#        "features.lisp",
#        "lists.lisp",
#        "types.lisp",
#        "arrays.lisp",
#        "io.lisp",
#        "sequences.lisp",
#        "numbers.lisp",
#    ],
#    order = "serial",
#    visibility = ["//visibility:public"],
#)

lisp_library(
    name = "alexandria",
    srcs = [
        "numbers.lisp",
    ],
    deps = [":pass7"],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "package",
    srcs = ["package.lisp"],
)

lisp_library(
    name = "pass2",
    srcs = [
        "binding.lisp",
        "conditions.lisp",
        "definitions.lisp",
        "strings.lisp",
        "symbols.lisp",
    ],
    order = "parallel",
    deps = [":package"],
)

lisp_library(
    name = "macros",
    srcs = [
        "macros.lisp",
    ],
    deps = [":pass2"],
)

lisp_library(
    name = "pass4",
    srcs = [
        "control-flow.lisp",
        "functions.lisp",
        "hash-tables.lisp",
    ],
    order = "parallel",
    deps = [":macros"],
)

lisp_library(
    name = "pass5",
    srcs = [
        "features.lisp",
        "lists.lisp",
    ],
    deps = [":pass4"],
)

lisp_library(
    name = "types",
    srcs = [
        "types.lisp",
    ],
    deps = [":pass5"],
)

lisp_library(
    name = "pass7",
    srcs = [
        "arrays.lisp",
        "io.lisp",
        "sequences.lisp",
    ],
    deps = [":types"],
)
