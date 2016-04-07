# Description: bordeaux-threads thread library

licenses(["notice"])  # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "bordeaux_threads",
    srcs = [
        # Ordered
        "src/pkgdcl.lisp",
        "src/bordeaux-threads.lisp",
        "src/impl-sbcl.lisp",
        "src/default-implementations.lisp",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@lisp__alexandria//:alexandria",
    ],
)
