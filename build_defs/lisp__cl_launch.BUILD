# Description: cl-launch.sh -- shell wrapper for Common Lisp

licenses(["unencumbered"]) # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_binary", "lisp_library", "lisp_test")

lisp_library(
    name = "launcher",
    srcs = ["launcher.lisp"],
    deps = ["@lisp__asdf//:asdf"],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "dispatch",
    srcs = ["dispatch.lisp"],
    deps = ["@lisp__asdf//:uiop"],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "release_lib",
    srcs = ["release.lisp"],
    deps = [
        ":dispatch",
        "@lisp__asdf//:asdf",
        "@lisp__asdf//:uiop",
        "@lisp__cl_scripting//:cl_scripting",
        "@lisp__fare_utils//:fare_utils",
        "@lisp__inferior_shell//:inferior_shell",
        "@lisp__optima//:optima",
        "@lisp__optima//:ppcre",
    ],
    visibility = ["//visibility:private"],
)

lisp_binary(
    name = "release",
    deps = [":release_lib"],
    main = "uiop:restore-image",
    visibility = ["//visibility:public"],
)

sh_binary(
    name = "cl_launch",
    srcs = ["cl-launch.sh"]
)

genrule(
    name = "generate_files",
    outs = [
        "build.xcvb",
        "cl-launch.asd",
        "launcher.lisp",
        "wrapper.sh",
    ],
    tools = [":cl_launch"],
    cmd = "$(location :cl_launch) -I $(@D) -B install_path",
    visibility = ["//visibility:public"],
)

genrule(
    name = "test_runner",
    outs = ["test-runner.sh"],
    tools = [
        "cl-launch.sh",
        "cl-launch-tests.sh",
    ],
    cmd = "echo > $@ $(location :cl-launch.sh) -l sbcl -B tests",
    visibility = ["//visibility:private"],
)

sh_test(
    name = "cl_launch_test",
    srcs = [":test_runner"],
    data = [
        "cl-launch.sh",
        "cl-launch-tests.sh",
    ],
    size = "small",
    visibility = ["//visibility:public"],
)
