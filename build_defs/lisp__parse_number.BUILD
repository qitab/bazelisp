# Description: PARSE-NUMBER is a library of functions which accept an arbitrary
# string and attempt to parse it, if possible, into one of the standard
# Common Lisp number types without using the reader, or else signal an
# error of type INVALID-NUMBER

licenses(["notice"]) # BSD 3-Clause

package(default_visibility = ["//visibility:public"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "parse_number",
    srcs = ["parse-number.lisp"],
)

lisp_test(
    name = "parse_number_test",
    srcs = ["tests.lisp"],
    main = "org.mapcar.parse-number-tests::run-tests",
    deps = [":parse_number"],
)
