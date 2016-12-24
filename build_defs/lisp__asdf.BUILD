# Description:
#   ASDF, Another System Definition Facility
#   is the de facto standard Common Lisp build tool
#
#   It also contains UIOP, Utilities for Implementation and OS Portability,
#   the portability layer of ASDF (see in uiop/), that provides
#   provides abstraction of pathnames, filesystem access, compilation, etc.
#
#   Note: tests and release tools were moved to pseudo-repository @lisp__asdf_tools
#   so that one may use asdf without any of the dependencies required for test and release.
#
licenses(["unencumbered"])  # MIT

exports_files(glob(["**"], exclude=[".git/**"]))

load("@lisp__bazel//:bazel/rules.bzl", "lisp_binary", "lisp_library", "lisp_test")

lisp_library(
    name = "uiop",
    srcs = [
	# Serially.
        "uiop/package.lisp",
        "uiop/common-lisp.lisp",
        "uiop/utility.lisp",
        "uiop/os.lisp",
        "uiop/pathname.lisp",
        "uiop/filesystem.lisp",
        "uiop/stream.lisp",
        "uiop/image.lisp",
        "uiop/run-program.lisp",
        "uiop/lisp-build.lisp",
        "uiop/configuration.lisp",
        "uiop/backward-driver.lisp",
        "uiop/driver.lisp",
    ],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "asdf",
    srcs = [
	# Serially.
        "header.lisp",
        "upgrade.lisp",
        "component.lisp",
        "system.lisp",
        "cache.lisp",
        "find-system.lisp",
        "find-component.lisp",
        "operation.lisp",
        "action.lisp",
        "lisp-action.lisp",
        "plan.lisp",
        "operate.lisp",
        "output-translations.lisp",
        "source-registry.lisp",
        "parse-defsystem.lisp",
        "bundle.lisp",
        "concatenate-source.lisp",
        "package-inferred-system.lisp",
        "backward-internals.lisp",
        "backward-interface.lisp",
        "interface.lisp",
        "user.lisp",
        "footer.lisp",
	":bazel-init.lisp",
    ],
    deps = [":uiop"],
    nowarn = ["type-style"],
    visibility = ["//visibility:public"],
)

genrule(
    name = "bazel_init",
    outs = ["bazel-init.lisp"],
    cmd = ("echo > $@ " +
        "\"(asdf:initialize-source-registry " +
	"'(:source-registry :ignore-inherited-configuration))\" " +
	"'(asdf:disable-output-translations)'"),
)

filegroup(
    name = "all-files",
    srcs = glob(["**"], exclude=[".git/**"]),
    visibility = ["//visibility:public"],
)
