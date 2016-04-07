# Description: named-readtables, spawn local or remote processes and shell pipes in Common Lisp.

licenses(["unencumbered"]) # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "named_readtables",
    srcs = [
	# Serially.
        "src/package.lisp",
        "src/utils.lisp",
        "src/define-api.lisp",
        "src/cruft.lisp",
        "src/named-readtables.lisp",
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "named_readtables_test",
    srcs = [
        "test/package.lisp",
        "test/rt.lisp",
        "test/tests.lisp",
    ],
    deps = [
        ":named_readtables",
        "@lisp__hu_dwim_stefil//:hu_dwim_stefil",
    ],
    main = "named-readtables-test::do-tests",
    visibility = ["//visibility:public"],
)
