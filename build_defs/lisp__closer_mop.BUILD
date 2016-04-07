# Description:
# Closer to MOP, a compatibility layer for the CLOS MOP.

licenses(["notice"])  # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "closer_mop",
    srcs = [
        "closer-mop-packages.lisp",
        "closer-mop-shared.lisp",
        "closer-sbcl.lisp",
    ],
    visibility = ["//visibility:public"],
)
