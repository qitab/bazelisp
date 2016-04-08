# Description: universally useful Lisp code that lives in package com.google.base.

licenses(["notice"]) # New BSD

exports_files(["COPYING"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "com_google_base",
    srcs = [
        # Order is important here.
        "package.lisp",
        "optimize.lisp",
        "syntax.lisp",
        "error.lisp",
        "type.lisp",
        "octet.lisp", # TODO: make it fast and unsafe
        "sequence.lisp",
    ],
    visibility = ["//visibility:public"],
)

lisp_test(
    name = "com_google_base_test",
    size = "small",
    srcs = ["base_test.lisp"],
    main = "com.google.base-test:test-base",
    deps = [
        ":com_google_base",
        "@lisp__hu_dwim_stefil//:hu_dwim_stefil",
    ],
)
