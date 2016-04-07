# Description: Flexible bivalent streams for Common Lisp

licenses(["notice"]) # Simplified BSD

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "flexi_streams",
    srcs = [
	# Serially.
        "packages.lisp",
        "mapping.lisp",
        "ascii.lisp",
        "koi8-r.lisp",
        "iso-8859.lisp",
        "code-pages.lisp",
        "specials.lisp",
        "util.lisp",
        "conditions.lisp",
        "external-format.lisp",
        "length.lisp",
        "encode.lisp",
        "decode.lisp",
        "in-memory.lisp",
        "stream.lisp",
        "output.lisp",
        "input.lisp",
        "io.lisp",
        "strings.lisp",
    ],
    nowarn = [
        "implicit-generic",
        "changed-ftype-proclamation",
    ],
    deps = ["@lisp__trivial_gray_streams//:trivial_gray_streams"],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "flexi_streams_test",
    srcs = [
	# Serially.
	"test/packages.lisp",
	"test/test.lisp",
    ],
    deps = [":flexi_streams"],
    data = [
        "test/kafka_utf8_lf.txt",
    ],
    main = "flexi-streams-test::run-all-tests",
    visibility = ["//visibility:public"],
)
