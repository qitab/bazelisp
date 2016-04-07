# Description: Common Lisp Portable Perl-Compatible Regular Expression library.

licenses(["unencumbered"])  # BSD

exports_files(["LICENSE"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "cl_ppcre",
    srcs = [
	# Serially.
        "packages.lisp",
        "specials.lisp",
        "util.lisp",
        "errors.lisp",
        "charset.lisp",
        "charmap.lisp",
        "chartest.lisp",
        "lexer.lisp",
        "parser.lisp",
        "regex-class.lisp",
        "regex-class-util.lisp",
        "convert.lisp",
        "optimize.lisp",
        "closures.lisp",
        "repetition-closures.lisp",
        "scanner.lisp",
        "api.lisp",
    ],
    nowarn = [
        "sb-c:inlining-dependency-failure",
        "optional-and-key",
    ],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "cl_ppcre_test_lib",
    srcs = [
	# Serially.
	"test/packages.lisp",
	"test/tests.lisp",
	"test/perl-tests.lisp",
    ],
    deps = [
        ":cl_ppcre",
	"@lisp__flexi_streams//:flexi_streams"
    ],
    visibility = ["//visibility:private"],
)

lisp_test(
    name = "cl_ppcre_test",
    main = "cl-ppcre-test::run-all-tests",
    deps = [":cl_ppcre_test_lib"],
    data = [
        "test/perltestdata",
        "test/perltestinput",
        "test/simple",
    ],
    size = "small",
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "cl_ppcre_unicode",
    srcs = [
	# Serially.
        "cl-ppcre-unicode/packages.lisp",
        "cl-ppcre-unicode/resolver.lisp",
    ],
    deps = [
        ":cl_ppcre",
	"@lisp__cl_unicode//:cl_unicode"
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "cl_ppcre_unicode_test",
    srcs = ["test/unicode-tests.lisp"],
    deps = [
        ":cl_ppcre_test_lib",
	":cl_ppcre_unicode",
    ],
    #main = "(lambda () (cl-ppcre-test::run-all-test :more-tests 'cl-ppcre-test::unicode-test))",
    main = "cl-ppcre-test::unicode-test",
    data = [
        "test/unicodetestdata",
    ],
    size = "small",
    visibility = ["//visibility:public"],
)
