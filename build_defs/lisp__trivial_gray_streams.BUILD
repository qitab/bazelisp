# Description: Gray I/O streams compatibility layer for Common Lisp

licenses(["unencumbered"]) # MIT

exports_files(["COPYING"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "trivial_gray_streams",
    srcs = [
	# Serially.
        "package.lisp",
        "streams.lisp",
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "trivial_gray_streams_test",
    srcs = [
	# Serially.
        "test/package.lisp",
        "test/test-framework.lisp",
        "test/test.lisp",
    ],
    deps = [
        ":trivial_gray_streams",
    ],
    main = "trivial-gray-streams-test:run-tests",
    visibility = ["//visibility:public"],
)
