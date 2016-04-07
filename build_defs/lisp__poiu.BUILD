# Description:
#   POIU, Parallel Operators on Independent Units
#   is an extension to the Common Lisp build system ASDF
#   to compile files in parallel on the localhost.
#   It can speed up the build a little, and sometimes fail earlier on missing dependencies.
#
load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

licenses(["notice"]) # MIT

lisp_library(
    name = "poiu",
    srcs = [
        "poiu.lisp",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@lisp__asdf//:asdf",
    ],
)
