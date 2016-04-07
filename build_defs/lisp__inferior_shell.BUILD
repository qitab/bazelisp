# Description: inferior-shell, spawn local or remote processes and shell pipes in Common Lisp.

licenses(["unencumbered"]) # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "inferior_shell",
    srcs = [
	# Serially.
        "pkgdcl.lisp",
        "process-spec.lisp",
        "utilities.lisp",
        "macros.lisp",
        "host.lisp",
        "run.lisp",
    ],
    deps = [
        "@lisp__alexandria//:alexandria",
        "@lisp__asdf//:uiop",
        "@lisp__fare_mop//:fare_mop",
        "@lisp__fare_quasiquote//:fare_quasiquote",
        "@lisp__fare_utils//:fare_utils",
        "@lisp__optima//:optima",
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "inferior_shell_test",
    srcs = [
        "test.lisp",
    ],
    deps = [
        ":inferior_shell",
        "@lisp__hu_dwim_stefil//:hu_dwim_stefil",
    ],
    main = "inferior-shell-test::test-suite",
    visibility = ["//visibility:public"],
)
