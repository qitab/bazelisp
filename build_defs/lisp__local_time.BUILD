# Description: A library for manipulating dates and times, based on a paper by Erik Naggum

licenses(["notice"]) # MIT

exports_files(["COPYING"])

package(default_visibility = ["//visibility:public"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "local_time",
    srcs = [
        # Serially.
        "src/package.lisp",
        "src/local-time.lisp",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@lisp__cl_fad//:cl_fad",
    ],
    nowarn = [
        "sb-c:compiler-macro-application-missed-warning",
    ],
)

#lisp_test(
#    name = "local_time_test",
#    main = "local-time.test::test",
#    srcs = [
#        "test/package.lisp",
#        "test/simple.lisp",
#        "test/comparison.lisp",
#        "test/formatting.lisp",
#        "test/parsing.lisp",
#        "test/timezone.lisp",
#    ],
#    deps = [
#        ":local_time",
#        "@lisp__stefil//:stefil",
#    ],
#)
