# Description: Common Lisp Protocol Buffers library

licenses(["notice"])  # MIT

exports_files(["LICENSE"])

package(default_visibility = ["//visibility:public"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "cl_protobufs",
    srcs = [
        "pkgdcl.lisp",
        # models
        "utilities.lisp",
        "model-classes.lisp",
        "conditions.lisp",
        # parsing
        "printer.lisp",
        "parser.lisp",
        # schema
        "define-proto.lisp",
        "upgradable.lisp",
        "clos-transform.lisp",
        # serialization
        "text-format.lisp",
        "wire-format.lisp",
        "serialize.lisp",
        # misc
        "api.lisp",
	"asdf-support.lisp",
        "examples.lisp",
    ],
    nowarn = [
        "implicit-generic",
	"optional-and-key",
        "undefined-function-warning",
	"sb-c:compiler-macro-application-missed-warning",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@lisp__asdf//:asdf",
        "@lisp__babel//:babel",
        "@lisp__closer_mop//:closer_mop",
        "@lisp__trivial_garbage//:trivial_garbage",
    ],
)

#proto_library(
#    name = "proto2_descriptor_extensions",
#    srcs = ["proto2-descriptor-extensions.proto"],
#    has_services = 0,
#    cc_api_version = 2,
#    go_api_version = 2,
#    java_api_version = 2,
#    visibility = ["//visibility:public"],
#    deps = ["@c__proto2//proto:descriptor"],
#)

# Tests belong into the ./tests directory.
