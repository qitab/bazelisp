# Description: Closure Common -- a Common Lisp XML parser

licenses(["restricted"])  # LLGPL (Lisp Lesser General Public Licence)

exports_files(["LICENSE"])

package(default_visibility = ["//visibility:public"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "closure_common",
    srcs = [
        # Serial
	"system.lisp",
        "package.lisp",
        "definline.lisp",
        "characters.lisp",
        "syntax.lisp",
        "encodings.lisp",
        "encodings-data.lisp",
        "xstream.lisp",
        "ystream.lisp",
        "hax.lisp",
    ],
    nowarn = [
        "implicit-generic",
        "changed-ftype-proclamation",
    ],
    deps = [
        "@lisp__asdf//:asdf",
        "@lisp__babel//:babel",
        "@lisp__trivial_gray_streams//:trivial_gray_streams",
    ],
)

genrule(
    name = "system",
    outs = ["system.lisp"],
    tools = ["closure-common.asd"],
    cmd = "cat $(location :closure-common.asd) > $@",
)

