# Description:
#   This is a portable Universal Resource Identifier library for
#   Common Lisp programs.  It parses URIs according to the RFC 2396
#   specification.  It's based on Franz, Inc.'s opensource URI package
#   and has been ported to work on other CL implementations.

licenses(["restricted"])  # LLGPL (Lisp Lesser General Public Licence)

exports_files(["LICENSE"])

package(default_visibility = ["//visibility:public"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "puri",
    srcs = ["src.lisp"],
    nowarn = ["implicit-generic"],
)

lisp_test(
    name = "puri-tests",
    srcs = ["tests.lisp"],
    main = "puri-tests::do-tests",
    deps = [
        ":puri",
        "@lisp__ptester//:ptester",
    ],
)
