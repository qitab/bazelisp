# Description: fare-utils, general-purpose utilities for Common Lisp.

licenses(["unencumbered"]) # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "fare_utils",
    srcs = [
	# Serially.
        "package.lisp",
        "base/utils.lisp",
        "base/character-classes.lisp",
        "base/strings.lisp",
        "base/symbols.lisp",
        "base/macros.lisp",
        "base/lists.lisp",
        "base/packages.lisp",
        "base/objects.lisp",
        "base/streams.lisp",
        "base/hash-tables.lisp",
        "base/more-strings.lisp",
        "base/parse-cl-syntax.lisp",
        "filesystem/pathnames.lisp",
        "filesystem/files.lisp",
        "filesystem/atomic.lisp",
        "stateful/package.lisp",
        "stateful/container.lisp",
	"stateful/dllist.lisp",
    ],
    deps = [
        "@lisp__asdf//:uiop",
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "fare_utils_test",
    srcs = [
        "test/package.lisp",
	"test/strings.lisp",
    ],
    deps = [
        ":fare_utils",
        "@lisp__hu_dwim_stefil//:hu_dwim_stefil",
    ],
    main = "fare-utils-test::test-suite",
    size = "small",
    visibility = ["//visibility:public"],
)
