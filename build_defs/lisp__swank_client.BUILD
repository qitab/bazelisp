# Description: swank client protocol

licenses(["restricted"])  # GPLv2

exports_files(["LICENSE"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

# WARNING: Testing showed that the SWANK library is not very reliable.
lisp_library(
    name = "swank_client",
    srcs = [
        # Order is important here.
        "package.lisp",
        "swank-client.lisp",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@lisp__com_google_base//:com_google_base",
        "@lisp__bordeaux_threads//:bordeaux_threads",
        "@lisp__slime//:swank",
        "@lisp__usocket//:usocket",
    ],
)

#lisp_test(
#    name = "swank-client-test",
#    timeout = "short",
#    srcs = ["swank-client-test.lisp"],
#    flaky = 1,
#    deps = [
#        ":swank_client",
#        "@lisp__google_net_util//:port-picker",
#        "@lisp__google_test",
#    ],
#)
