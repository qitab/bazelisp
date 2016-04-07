# Description: fare-quasiquote, portable and matchable implementation of backquote for Common Lisp.

licenses(["unencumbered"]) # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "quasiquote",
    srcs = [
	# Serially.
        "packages.lisp",
        "quasiquote.lisp",
        "pp-quasiquote.lisp",
    ],
    deps = [
        "@lisp__asdf//:uiop",
        "@lisp__fare_utils//:fare_utils",
    ],
    visibility = ["//visibility:public"],
)

genrule(
    name = "nop",
    outs = ["nop.lisp"],
    cmd = ": > $@",
)

lisp_library(
    name = "fare_quasiquote",
    srcs = ["nop.lisp"], # BUG: lisp_library fails when there are no srcs(!)
    deps = [
        ":quasiquote",
        ":optima",
	":readtable",
    ],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "optima",
    srcs = ["fare-quasiquote-optima.lisp"],
    deps = [
        ":quasiquote",
        "@lisp__optima//:optima",
    ],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "readtable",
    srcs = ["quasiquote-readtable.lisp"],
    deps = [
        ":quasiquote",
        "@lisp__named_readtables//:named_readtables",
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "fare_quasiquote_test",
    srcs = ["quasiquote-test.lisp"],
    deps = [
        ":fare_quasiquote",
        "@lisp__hu_dwim_stefil//:hu_dwim_stefil",
    ],
    main = "fare-quasiquote/test::fare-quasiquote-test",
    size = "small",
    visibility = ["//visibility:public"],
)
