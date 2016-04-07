# Description: Portable pathname library for Common Lisp
# NB: Obsoleted by UIOP.

licenses(["notice"]) # BSD

exports_files(["LICENSE"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "cl_fad",
    srcs = [
        # Serially.
        "packages.lisp",
        "fad.lisp",
        "path.lisp",
        "temporary-files.lisp",
    ],
    deps = [
        # "@lisp__sbcl//:sb-posix",
        "@lisp__alexandria//:alexandria",
        "@lisp__bordeaux_threads//:bordeaux_threads",
    ],
    visibility = ["//visibility:public"],
)

#lisp_test(
#    name = "cl_fad_test",
#    srcs = [
#        "packages.test.lisp",
#        "fad.test.lisp",
#        "temporary-files.test.lisp",
#    ],
#    deps = [
#        ":cl_fad",
#        "@lisp__cl_ppcre//:cl_ppcre",
#        "@lisp__unit_test//:unit_test",
#    ],
#    visibility = ["//visibility:public"],
#)
