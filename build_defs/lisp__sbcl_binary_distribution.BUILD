# Description: SBCL 1.x binary distribution

exports_files(["bin/sbcl"] + glob(["lib/sbcl/**"]))

filegroup(
    name = "sbcl",
    srcs = ["bin/sbcl"],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "sbcl-lib",
    srcs = glob(["lib/sbcl/**"]),
    visibility = ["//visibility:public"],
)

load("@lisp__bazel//:bazel/rules.bzl", "ld_use_symbol")

cc_library(
    name = "libsbcl",
    srcs = ["lib/sbcl/libsbcl.a"],
    visibility = ["//visibility:public"],
    linkopts = [
        ld_use_symbol("uid_username"), # wrap.o
        ld_use_symbol("lseek_largefile"),  # largefile.o
        ld_use_symbol("get_timezone"),  # time.o
        ld_use_symbol("spawn"),  # run-program.o
        "-ldl",
        "-lpthread",
        "-lm",
    ],
    linkstatic = 1,
    deps = [
        "@c__zlib//:zlib",
    ],
)

genrule(
    name = "libsbcl-exported-symbols",
    srcs = ["lib/sbcl/libsbcl-exported-symbols.lds"],
    outs = ["libsbcl-exported-symbols.lds"],
    cmd = "cat $< > $@",
    visibility = ["//visibility:public"],
)
