# Description:
# Plexippus XPath project aims to become a full implementation of XML Path Language
# (XPath) Version 1.0. As of now, its API isn't stabilized yet and there isn't
# any documentation except the source.

licenses(["notice"])  # X11-style

exports_files(["LICENSE"])

package(default_visibility = ["//visibility:public"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "xpath",
    srcs = [
        # Serial
        "package.lisp",
        "utils.lisp",
        "pipes.lisp",
        "protocol.lisp",
        "xnum-ieee.lisp",
        "types.lisp",
        "extensions.lisp",
        "environment.lisp",
        "axes.lisp",
        "node-tests.lisp",
        "xpath.lisp",
        "functions.lisp",
        "lexer.lisp",
        "api.lisp",
        "parser.lisp",
        "plx.lisp",
        "xmls-compat.lisp",
        "patterns.lisp",
        "profile.lisp",
    ],
    deps = [
        "@lisp__cl_ppcre//:cl_ppcre",
        "@lisp__cl_yacc//:cl_yacc",
        "@lisp__cxml//:cxml",
        "@lisp__parse_number//:parse_number",
    ],
)

lisp_test(
    name = "xpath-test",
    srcs = [
        # Serial
        "test.lisp",
        "xpath-test.lisp",
        #"xnum-ieee-test.lisp",
    ],
    main = "xpath::run-all-tests",
    deps = [":xpath"],
)
