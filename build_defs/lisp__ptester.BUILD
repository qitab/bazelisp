# Description: PTester is a portable version of Franz's tester library.

licenses(["restricted"]) # LLGPL (Lisp Lesser General Public Licence)

package(default_visibility = ["//visibility:public"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "ptester",
    srcs = ["src.lisp"],
    nowarn = ["style"],
)
