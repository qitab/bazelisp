# Description:
# zlib is a general purpose data compression library.

# licenses(['unencumbered'])  # code authored by Google
licenses(["notice"])  # BSD/MIT-like license (for zlib)

exports_files(["LICENSE"])

# MSVC doesn't understand the assembler dialect in match.S, and mingw has
# trouble linking match.o.  Use preprocessor macros in match.S to avoid this
# problem.
#ZLIB_COPTS=[]
# On x86:
#ZLIB_COPTS=["-DASMV", "-DNO_UNDERLINE"]
# On x86_64:
ZLIB_COPTS=["-DUNALIGNED_OK", "-DEXPAND_INSERT_STRING"]

ZLIB_SOURCES = [
    "adler32.c",
    "compress.c",
    "crc32.c",
    "deflate.c",
    "gzclose.c",
    "gzlib.c",
    "gzread.c",
    "gzwrite.c",
    "infback.c",
    "inffast.c",
    "inflate.c",
    "inftrees.c",
    "trees.c",
    "uncompr.c",
    "zutil.c",
]

ZLIB_HEADERS = [
    "crc32.h",
    "deflate.h",
    "gzguts.h",
    "inffast.h",
    "inffixed.h",
    "inflate.h",
    "inftrees.h",
    "trees.h",
    "zconf.h",
    "zutil.h",
]

ZLIB_ASM = [
    # "contrib/amd64/amd64-match.S", # uncomment if you use -DASMV
    # "contrib/asm686/match.S", # uncomment if you use -DASMV
]

cc_library(
    name = "zlib",
    srcs = ZLIB_SOURCES + ZLIB_HEADERS + ZLIB_ASM,
    hdrs = ["zlib.h"],
    copts = [
        "-Wno-implicit-function-declaration",
        ] + ZLIB_COPTS,
    visibility = ["//visibility:public"],
)

# These targets should only be used by clients that have a need to depend upon
# the zlib headers from targets other than a cc_{library,binary,test} rule,
# such as a genrule that invokes a C compiler.
filegroup(
    name = "headers",
    srcs = glob(["*.h"]),
    visibility = ["//visibility:public"],
)
