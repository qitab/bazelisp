# Description:
#  Chipz is a library for decompressing DEFLATE and BZIP2 data.
#  Chipz is the reading complement to Salza.

licenses(["notice"])  # New BSD

exports_files(["LICENSE"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "chipz",
    srcs = [
        # Order is important here.
	"chipz.asd.lisp", # yuck: the code depends on a feature defined here.
        "package.lisp",
        "constants.lisp",
        "types-and-tables.lisp",
        "crc32.lisp",
        "adler32.lisp",
        "conditions.lisp",
        "dstate.lisp",
        "inflate-state.lisp",
        "gzip.lisp",
        "zlib.lisp",
        "inflate.lisp",
        "bzip2.lisp",
        "decompress.lisp",
        "stream.lisp",
        #"stream-fallback.lisp",
    ],
    visibility = ["//visibility:public"],
)

genrule(
    name = "chips.asd.lisp.generate",
    outs = ["chipz.asd.lisp"],
    cmd = ("echo > $@ '(in-package :cl) " +
        "(defpackage :chipz-system (:use :cl) (:export #:gray-streams)) " +
	"(pushnew (quote chipz-system:gray-streams) cl:*features*)'"),
    visibility = ["//visibility:private"],
)
