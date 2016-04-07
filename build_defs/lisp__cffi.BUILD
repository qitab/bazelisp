# Description:
#     Common Lisp CFFI library.

licenses(["notice"]) # MIT

exports_files(["LICENSE"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "cffi",
    srcs = [
        # Serial
        "src/cffi-sbcl.lisp",  # TODO(czak): Make the compiler configurable.
        "src/package.lisp",
        "src/utils.lisp",
        "src/libraries.lisp",
        "src/early-types.lisp",
        "src/types.lisp",
        "src/enum.lisp",
        "src/strings.lisp",
        "src/structures.lisp",
        "src/functions.lisp",
        "src/foreign-vars.lisp",
        "src/features.lisp",
    ],
    deps = [
        "@lisp__alexandria//:alexandria",
        "@lisp__asdf//:asdf",
        "@lisp__asdf//:uiop",
        "@lisp__babel//:babel",
        "@lisp__trivial_features//:trivial_features",
    ],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "c2ffi",
    srcs = [
        # Serial
        "src/c2ffi/package.lisp",
        "src/c2ffi/c2ffi.lisp",
        "src/c2ffi/asdf.lisp", # What should we do about ASDF support? Move it to its own target?
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":cffi",
        "@lisp__alexandria//:alexandria",
        "@lisp__asdf//:asdf",
        "@lisp__asdf//:uiop",
    ],
)

lisp_library(
    name = "c2ffi_generator",
    srcs = ["src/c2ffi/generator.lisp"],
    deps = [
        ":c2ffi",
        "@lisp__cl_ppcre//:cl_ppcre",
        "@lisp__cl_json//:cl_json",
    ],
    visibility = ["//visibility:public"],
)

#lisp_library(
#    name = "examples",
#    srcs = [
#        "examples/examples.lisp",
#        "examples/gethostname.lisp",
#        "examples/gettimeofday.lisp",
#    ],
#    deps = [
#        ":cffi",
#    ],
#    visibility = ["//visibility:public"],
#)

lisp_library(
    name = "grovel",
    srcs = [
        # Serial
        "grovel/package.lisp",
        "grovel/grovel.lisp",
        "grovel/asdf.lisp", # What to do with it?
    ],
    data = [
        "grovel/common.h",
    ],
    deps = [
        ":cffi",
        ":toolchain",
        "@lisp__alexandria//:alexandria",
    ],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "toolchain",
    srcs = [
        "toolchain/package.lisp",
        "toolchain/asdf-compat.lisp", # What to do with it?
        "toolchain/c-toolchain.lisp",
        "toolchain/static-link.lisp",
    ],
    deps = [
        ":cffi",
        "@lisp__asdf//:asdf",
    ],
    visibility = ["//visibility:public"],
)

## TODO(dlroxe) fix linking issue with @c__libffi//:libffi
#
#lisp_library(
#    name = "libffi",
#    srcs = [
#        "libffi/libffi.lisp",
#        "libffi/libffi-types.grovel.lisp", # to be generated via grovel-files (!)
#        "libffi/libffi-functions.lisp",
#        "libffi/type-descriptors",
#        "libffi/funcall",
#    ],
#    cdeps = [
#        "@c__libffi//:libffi",
#    ],
#    visibility = ["//visibility:public"],
#    deps = [
#        ":grovel",
#        "@lisp__trivial_features//:trivial_features",
#    ],
#)

lisp_library(
    name = "uffi",
    srcs = ["uffi-compat/uffi-compat.lisp"],
    deps = [
        ":cffi",
    ],
    visibility = ["//visibility:public"],
)

# TODO: convert cffi-tests
#
#lisp_test(
#    name = "cffi-tests",
#    srcs = [
#        "tests/package.lisp",
#        "tests/bindings.lisp",
#        "tests/funcall.lisp",
#        "tests/defcfun.lisp",
#        "tests/callbacks.lisp",
#        "tests/foreign-globals.lisp",
#        "tests/memory.lisp",
#        "tests/strings.lisp",
#        "tests/arrays.lisp",
#        "tests/struct.lisp",
#        "tests/union.lisp",
#        "tests/enum.lisp",
#        "tests/fsbv.lisp",
#        "tests/misc-types.lisp",
#        "tests/misc.lisp",
#        "tests/asdf.lisp",
#        "tests/grovel.lisp",
#    ],
#    deps = [
#        ":cffi",
#        ":libtest",
#        ":libtest2",
#        ":libfsbv",
#    ],
#    main = "cffi-tests::run-all-cffi-tests",
#    visibility = ["//visibility:public"],
#)

# TODO: convert cffi-tests/example
