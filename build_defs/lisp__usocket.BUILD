# Description: usocket socket I/O library

licenses(["notice"])  # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

usocket_version="0.6.4.1"

genrule(
    name = "setup",
    outs = ["setup.lisp"],
    cmd = ("echo > $@ '(asdf:register-preloaded-system %r :version %r)'" %
        ("usocket", usocket_version)),
)



lisp_library(
    name = "usocket",
    srcs = [
        # Order is important here.
	":setup.lisp",
        "package.lisp",
        "vendor/split-sequence.lisp",
        "vendor/spawn-thread.lisp",
        "usocket.lisp",
        "condition.lisp",
        "backend/sbcl.lisp",
        "option.lisp",
        "server.lisp",
    ],
    deps = [
        "@lisp__asdf//:asdf",
    ],
    nowarn = ["style-warning"],
    visibility = ["//visibility:public"],
)
