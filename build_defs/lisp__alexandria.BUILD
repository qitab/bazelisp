# Description: Common Lisp alexandria library.

licenses(["unencumbered"])  # public domain

exports_files(["LICENSE"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "alexandria",
    srcs = [
	# Serially.
        "package.lisp",
        "binding.lisp",
        "conditions.lisp",
        "definitions.lisp",
        "strings.lisp",
        "symbols.lisp",
        "macros.lisp",
        "control-flow.lisp",
        "functions.lisp",
        "hash-tables.lisp",
        "features.lisp",
        "lists.lisp",
        "types.lisp",
        "arrays.lisp",
        "io.lisp",
        "sequences.lisp",
        "numbers.lisp",
    ],
    visibility = ["//visibility:public"],
)
