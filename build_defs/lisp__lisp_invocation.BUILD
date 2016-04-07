# Description: lisp-invocation, invoking Common Lisp subprocesses from Common Lisp

licenses(["unencumbered"]) # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "lisp_invocation",
    srcs = [
	# Serially.
        "lisp-invocation.lisp",
        "implementations.lisp",
    ],
    deps = [
        "@lisp__asdf//:asdf",
        "@lisp__asdf//:uiop",
    ],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "lisp_invocation_all",
    srcs = [
	# Serially.
        "non-special.lisp",
        "allegro-variants.lisp",
    ],
    deps = [
        ":lisp_invocation",
        "@lisp__fare_utils//:fare_utils",
    ],
    visibility = ["//visibility:public"],
)
