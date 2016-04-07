# Description: EOS test framework

licenses(["notice"])  # MIT, portions BSD

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "eos",
    srcs = [
        # serial
        "src/package.lisp",
        "src/utils.lisp",
        "src/test.lisp",
        "src/check.lisp",
        "src/classes.lisp",
        "src/explain.lisp",
        "src/suite.lisp",
        "src/run.lisp",
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "eos-tests",
    srcs = [
        # serial
        "tests/suite.lisp",
        "tests/tests.lisp",
    ],
    main = "eos::run-self-tests",
    deps = [
        ":eos",
    ],
)
