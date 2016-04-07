# Description: A LALR(1) parser generator for Common Lisp

licenses(["notice"])  # MIT

package(default_visibility = ["//visibility:public"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "cl_yacc",
    srcs = ["yacc.lisp"],
    nowarn = ["sb-c:inlining-dependency-failure"],
)

lisp_test(
    name = "cl_yacc_test",
    srcs = ["yacc-tests.lisp"],
    main = "yacc-tests:tests",
    deps = [":cl_yacc"],
)
