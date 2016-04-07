# Description: Library for packing/unpacking ints and floats in octet vectors.

licenses(["notice"]) # New BSD

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "nibbles",
    srcs = [
        # Order is important here.
        "package.lisp",
        "types.lisp",
        "macro-utils.lisp",
        "vectors.lisp",
        "streams.lisp",
        "sbcl-opt/fndb.lisp",
        "sbcl-opt/nib-tran.lisp",
        "sbcl-opt/x86-vm.lisp",
        "sbcl-opt/x86-64-vm.lisp",
    ],
    visibility = ["//visibility:public"],
)
