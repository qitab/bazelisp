# Description: Common Lisp Portable Perl-Compatible Regular Expression library.

licenses(["unencumbered"]) # BSD

load("@lisp__bazel//:bazel/rules.bzl", "lisp_binary", "lisp_library", "lisp_test")

lisp_library(
    name = "base",
    srcs = [
	# Serially.
        "packages.lisp",
        "specials.lisp",
        "util.lisp",
    ],
    deps = [
	"@lisp__cl_ppcre//:cl_ppcre"
    ],
    nowarn = [
        "optional-and-key",
    ],
    visibility = ["//visibility:private"],
)

lisp_binary(
    name = "builder",
    srcs = [
	# Serially.
	"build/util.lisp",
	"build/char-info.lisp",
	"build/read.lisp",
	"build/dump.lisp",
    ],
    deps = [
        ":base",
	"@lisp__flexi_streams//:flexi_streams"
    ],
    main = "cl-unicode::create-source-files",
    nowarn = [
        "optional-and-key",
    ],
    visibility = ["//visibility:private"],
)

genrule(
    name = "generate_files",
    outs = [
      "lists.lisp",
      "hash-tables.lisp",
      "methods.lisp",
      "test/derived-properties",
    ],
    tools = ([":builder"] + glob(["build/data/*.txt"])),
    cmd = ("mkdir external/lisp__cl_unicode/test && " +
        "$(location :builder) && " +
	"cp -a --parent external/lisp__cl_unicode/* $(@D)/../.. ;"),
)

lisp_library(
    name = "cl_unicode",
    srcs = [
	# Serially.
	"conditions.lisp",
	"lists.lisp",
	"hash-tables.lisp",
	"api.lisp",
	"methods.lisp",
	"test-functions.lisp",
	"derived.lisp",
	"alias.lisp",
    ],
    deps = [":base"],
    nowarn = [
        "optional-and-key",
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "cl_unicode_test",
    srcs = [
        # Serially.
        "test/packages.lisp",
	"test/tests.lisp",
    ],
    main = "cl-unicode-test::run-all-tests",
    deps = [":cl_unicode"],
    data = [
        ":test/derived-properties",
	"test/properties",
	"test/simple"
    ],
    size = "small",
    visibility = ["//visibility:public"],
)
