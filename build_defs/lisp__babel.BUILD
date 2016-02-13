# Description: Common Lisp babel library.

licenses(["notice"])  # MIT

exports_files(["LICENSE"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "babel",
    srcs = [
        # From the src directory:
        "src/packages.lisp",
        "src/encodings.lisp",
        "src/enc-ascii.lisp",
        "src/enc-ebcdic.lisp",
        "src/enc-iso-8859.lisp",
        "src/enc-unicode.lisp",
        "src/enc-cp1251.lisp",
        "src/jpn-table.lisp",
        "src/enc-jpn.lisp",
        "src/external-format.lisp",
        "src/strings.lisp",
        "src/sharp-backslash.lisp",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@lisp__alexandria//:alexandria",
        "@lisp__trivial_features//:trivial_features",
    ],
)
