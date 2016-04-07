# Description: fare-mop, utilities using the CLOS MOP.

licenses(["unencumbered"]) # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "fare_mop",
    srcs = [
	# Serially.
        "package.lisp",
        "utilities.lisp",
    ],
    deps = [
        "@lisp__asdf//:uiop",
        "@lisp__fare_utils//:fare_utils",
        "@lisp__closer_mop//:closer_mop",
    ],
    visibility = ["//visibility:public"],
)
