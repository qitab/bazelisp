# Description: Trivial workout timer in Common Lisp.

licenses(["notice"])  # MIT

exports_files(["LICENSE"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_binary")

lisp_library(
    name = "workout_timer",
    srcs = ["timer.lisp"],
    visibility = ["//visibility:public"],
    deps = [
        "@lisp__asdf//:uiop",
        "@lisp__command_line_arguments//:command_line_arguments",
        "@lisp__local_time//:local_time",
        "@lisp__mixalot//:mixalot",
        "@lisp__mixalot//:vorbis",
        "@lisp__mixalot//:vorbisfile",
    ],
)
lisp_binary(
    name = "wt",
    deps = [":workout-timer"],
    main = "workout-timer:entry-point",
)
