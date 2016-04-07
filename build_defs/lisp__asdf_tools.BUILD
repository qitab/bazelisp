# Description:
#   Tools and tests for ASDF
#

load("@lisp__bazel//:bazel/rules.bzl", "lisp_binary", "lisp_library", "lisp_test")

lisp_library(
    name = "tools_lib",
    srcs = [
	# Serially.
        "@lisp__asdf//:tools/package.lisp",
        "@lisp__asdf//:tools/main.lisp",
        "@lisp__asdf//:tools/pathnames.lisp",
        "@lisp__asdf//:tools/version.lisp",
        "@lisp__asdf//:tools/test-environment.lisp",
        "@lisp__asdf//:tools/build.lisp",
        "@lisp__asdf//:tools/git.lisp",
        "@lisp__asdf//:tools/test-basic.lisp",
        "@lisp__asdf//:tools/test-scripts.lisp",
        "@lisp__asdf//:tools/test-upgrade.lisp",
        "@lisp__asdf//:tools/test-all.lisp",
        "@lisp__asdf//:tools/installation.lisp",
        "@lisp__asdf//:tools/release.lisp",
    ],
    deps = [
        "@lisp__asdf//:asdf",
        "@lisp__asdf//:uiop",
        "@lisp__cl_ppcre//:cl_ppcre",
        "@lisp__cl_scripting//:cl_scripting",
        "@lisp__inferior_shell//:inferior_shell",
        "@lisp__lisp_invocation//:lisp_invocation_all",
        "@lisp__optima//:ppcre",
    ],
    visibility = ["//visibility:private"],
)

lisp_binary(
    name = "asdf_tools",
    deps = [":tools_lib"],
    main = "asdf-tools::entry-point",
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "asdf_test",
    deps = [":tools_lib"],
    data = [
        "@lisp__asdf//:all-files"
    ],
    main = "asdf-tools::test",
    visibility = ["//visibility:public"],
)
