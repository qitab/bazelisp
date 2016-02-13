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

cc_library(
    name = "libsbcl",
    srcs = ["lib/sbcl/libsbcl.a"],
    visibility = ["//visibility:public"],
)

genrule(
    name = "libsbcl-exported-symbols",
    srcs = ["lib/sbcl/libsbcl-exported-symbols.lds"],
    outs = ["libsbcl-exported-symbols.lds"],
    cmd = "cat $< > $@",
    visibility = ["//visibility:public"],
)
