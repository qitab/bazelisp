# Description: Base 64 encoding and decoding library

licenses(["notice"])  # BSD

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "cl_base64",
    srcs = [
        # Ordered
        "package.lisp",
        "decode.lisp",
        "encode.lisp",
    ],
    visibility = ["//visibility:public"],
)
