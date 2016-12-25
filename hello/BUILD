# Description: hello world example

load("@lisp__bazel//:bazel/rules.bzl", "lisp_binary")

lisp_binary(
    name = "hello",
    srcs = [
        "hello.lisp",
    ],
    main = "hello:main",
    visibility = ["//visibility:public"],
    deps = [
        "@lisp__alexandria//:alexandria",
    ],
)

load("@lisp__bazel//:bazel/rules.bzl", "ld_use_symbol")

cc_library(
    name = "chello_lib",
    # This is ugly, but we need it, for now :-/
    linkopts = [ld_use_symbol("hello_string")],
    srcs = [
        "chello_lib.c",
    ],
)

lisp_binary(
    name = "chello",
    srcs = [
        "chello.lisp",
    ],
    main = "chello:main",
    visibility = ["//visibility:public"],
    deps = [
        "@lisp__cffi//:cffi",
    ],
    cdeps = [
        ":chello_lib"
    ],
)
