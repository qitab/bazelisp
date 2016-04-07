# Description: Common Lisp scripting utilities

licenses(["unencumbered"]) # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "failure",
    srcs = ["failure.lisp"],
    deps = [
        "@lisp__fare_utils//:fare_utils",
    ],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "commands",
    srcs = ["commands.lisp"],
    deps = [
        ":failure",
        "@lisp__asdf//:uiop",
        "@lisp__cl_launch//:dispatch",
    ],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "cl_scripting",
    srcs = ["cl-scripting.lisp"],
    deps = [
        ":commands",
        ":failure",
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "cl_scripting_test",
    srcs = [
        # Serially
        "test/suite.lisp",
	"test/failure.lisp",
    ],
    deps = [
        ":cl_scripting",
        "@lisp__hu_dwim_stefil//:hu_dwim_stefil",
    ],
    visibility = ["//visibility:public"],
    main = "cl-scripting/test/suite:cl-scripting/test",
    nowarn = ["style"],
)
