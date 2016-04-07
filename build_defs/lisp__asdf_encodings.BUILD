# Description:
#   Portable interface to character encodings and your implementation's external-format
#
licenses(["unencumbered"])  # MIT

exports_files(["uiop/contrib/debug.lisp"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_binary", "lisp_library", "lisp_test")

lisp_library(
    name = "asdf_encodings",
    srcs = [
	# Serially.1
        "pkgdcl.lisp",
        "encodings.lisp",
        "autodetect.lisp",
        "asdf-support.lisp",
        "initialization.lisp",
    ],
    deps = [
        "@lisp__asdf//:asdf",
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "asdf_encodings_test",
    srcs = [
	# Serially.
        "asdf-encodings-test.lisp",
    ],
    deps = [
        ":asdf_encodings",
        "@lisp__fare_utils//:fare_utils",
        "@lisp__hu_dwim_stefil//:hu_dwim_stefil",
    ],
    main = "asdf-encodings-test::test-suite",
    visibility = ["//visibility:public"],
)
