# Description: JSON I/O library for Common Lisp

licenses(["notice"])  # MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

package(default_visibility = ["//visibility:public"])

lisp_library(
    name = "cl_json",
    srcs = [
        # Serial
        "src/package.lisp",
        "src/common.lisp",
        "src/objects.lisp",
        "src/camel-case.lisp",
        "src/decoder.lisp",
        "src/encoder.lisp",
        "src/utils.lisp",
        "src/json-rpc.lisp",
    ],
    features = [
        "cl-json",
        "cl-json-clos",
    ],
)

#lisp_test(
#    name = "cl_json_test",
#    srcs = [
#        # Serial
#        "t/package.lisp",
#        "t/testdecoder.lisp",
#        "t/testencoder.lisp",
#        "t/testmisc.lisp",
#    ],
#    main = "(it.bese.fiveam:run! json-test::json)",
#    deps = [
#        ":cl-json",
#        "@lisp__fiveam//:fiveam",
#    ],
#)
