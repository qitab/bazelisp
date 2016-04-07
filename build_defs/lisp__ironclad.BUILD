# Description: A cryptographic library for Common Lisp

licenses(["notice"]) # New BSD

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "common",
    srcs = [
        # serial
        "setup.lisp",
        "src/package.lisp",
        "src/conditions.lisp",
        "src/util.lisp",
        "src/macro-utils.lisp",
        "src/common.lisp",
    ],
    deps = ["@lisp__nibbles//:nibbles"],
)

lisp_library(
    name = "sbcl-opt",
    srcs = [
        "src/sbcl-opt/fndb.lisp",
        "src/sbcl-opt/x86oid-vm.lisp",
    ],
    deps = [":common"],
)

lisp_library(
    name = "digest",
    srcs = [
        "src/digests/digest.lisp",
        # All, else depends on digest.lisp
        "src/digests/adler32.lisp",
        "src/digests/crc24.lisp",
        "src/digests/crc32.lisp",
        "src/digests/md2.lisp",
        "src/digests/md4.lisp",
        "src/digests/md5.lisp",
        "src/digests/md5-lispworks-int32.lisp",
        "src/digests/ripemd-128.lisp",
        "src/digests/ripemd-160.lisp",
        "src/digests/sha1.lisp",
        "src/digests/sha256.lisp",
        "src/digests/sha512.lisp",
        "src/digests/tiger.lisp",
        "src/digests/tree-hash.lisp",
        "src/digests/whirlpool.lisp",
    ],
    nowarn = [
        "undefined-function",
        "compiler-macro-after-function-use",
        "conflicting-ftype-declaration",
    ],
    deps = [
        ":common",
        ":sbcl-opt",
    ],
)

lisp_library(
    name = "ciphers",
    srcs = [
        "src/ciphers/cipher.lisp",
        # All else depends on cipher.lisp.
        "src/ciphers/modes.lisp",
        "src/ciphers/make-cipher.lisp",
        "src/ciphers/null-cipher.lisp",
        "src/ciphers/aes.lisp",
        "src/ciphers/des.lisp",
        "src/ciphers/blowfish.lisp",
        "src/ciphers/twofish.lisp",
        "src/ciphers/idea.lisp",
        "src/ciphers/misty1.lisp",
        "src/ciphers/square.lisp",
        "src/ciphers/rc2.lisp",
        "src/ciphers/rc5.lisp",
        "src/ciphers/rc6.lisp",
        "src/ciphers/tea.lisp",
        "src/ciphers/xtea.lisp",
        "src/ciphers/cast5.lisp",
        "src/ciphers/arcfour.lisp",
    ],
    nowarn = [
        "inline-used-before-definition",
    ],
    deps = [":common"],
)

lisp_library(
    name = "public-key",
    srcs = [
        "src/public-key/public-key.lisp",
        # All else depends on public-key.lisp.
        "src/public-key/dsa.lisp",
        "src/public-key/rsa.lisp",
    ],
    deps = [":common"],
)

lisp_library(
    name = "ironclad",
    srcs = [
        # Order is important here.
        # "ironclad.asd",
        "src/padding.lisp",
        "src/kdf-common.lisp",
        "src/pkcs5.lisp",
        "src/scrypt.lisp",
        "src/password-hash.lisp",
        "src/octet-stream.lisp",
        "src/prng/prng.lisp",
        "src/prng/generator.lisp",
        "src/prng/fortuna.lisp",
        "src/math.lisp",
        "src/macs/cmac.lisp",
        "src/macs/hmac.lisp",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":ciphers",
        ":common",
        ":digest",
        ":public-key",
    ],
)
