# Description: OPTIMA pattern matching library

package(default_visibility = ["//visibility:public"])

licenses(["restricted"]) # LLGPL

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "optima",
    srcs = [
        # serial
        "src/packages.lisp",
        "src/util.lisp",
        "src/pattern.lisp",
        "src/compiler.lisp",
        "src/extra.lisp",
        "src/fail.lisp",
        "src/match.lisp",
        "src/runtime.lisp",
    ],
    deps = [
        "@lisp__alexandria//:alexandria",
        "@lisp__closer_mop//:closer_mop",
    ],
)

lisp_library(
    name = "ppcre",
    srcs = ["lib/ppcre.lisp"],
    deps = [
        ":optima",
        "@lisp__cl_ppcre//:cl_ppcre",
    ],
)

lisp_test(
    name = "test",
    srcs = ["test/suite.lisp"],
    main = "optima.test::run!",
    size = "small",
    deps = [
        ":optima",
        ":ppcre",
        "@lisp__eos//:eos",
    ],
)
