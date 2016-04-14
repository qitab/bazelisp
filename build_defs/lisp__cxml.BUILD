# Description: Closure XML -- a Common Lisp XML parser

licenses(["restricted"])  # LLGPL (Lisp Lesser General Public Licence)

exports_files(["LICENSE"])

package(default_visibility = ["//visibility:public"])

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library", "lisp_test")

genrule(
    name = "setup",
    outs = ["setup.lisp"],
    tools = [":cxml.asd", "@lisp__bazel//:build_defs/lisp__cxml.setup.lisp", ":catalog.dtd"],
    cmd = (
        "( cat $(location :cxml.asd) $(location @lisp__bazel//:build_defs/lisp__cxml.setup.lisp) ; " +
        "  echo '(eval-when (:compile-toplevel :execute)' " +
	"    '(cxml-setup:setup \"$(location :catalog.dtd)\" \"$(@D)\"))' ) > $@" ),
)

lisp_library(
    name = "xml",
    srcs = [
        # Serial
        ":setup.lisp",
        "xml/package.lisp",
        "xml/util.lisp",
        "xml/sax-handler.lisp",
        "xml/xml-name-rune-p.lisp",
        "xml/split-sequence.lisp",
        "xml/xml-parse.lisp",
        "xml/unparse.lisp",
        "xml/xmls-compat.lisp",
        "xml/recoder.lisp",
        "xml/sax-proxy.lisp",
        "xml/xmlns-normalizer.lisp",
        "xml/space-normalizer.lisp",
        "xml/catalog.lisp",
        "xml/atdoc-configuration.lisp",
    ],
    ## TODO: make catalog.dtd appear at the right place, or
    ## somehow teach xml/catalog.lisp about where it is.
    compile_data = ["catalog.dtd"],
    nowarn = [
        "implicit-generic",
        "sb-c:inlining-dependency-failure",
    ],
    deps = [
        "@lisp__closure_common//:closure_common",
        "@lisp__puri//:puri",
        "@lisp__trivial_gray_streams//:trivial_gray_streams",
    ],
)

lisp_library(
    name = "dom",
    srcs = [
        # Serial
        "dom/package.lisp",
        "dom/dom-impl.lisp",
        "dom/dom-builder.lisp",
        "dom/dom-sax.lisp",
    ],
    nowarn = [
        "implicit-generic",
        "sb-c:inlining-dependency-failure",
    ],
    deps = [":xml"],
)

lisp_library(
    name = "klacks",
    srcs = [
        # Serial
        "klacks/package.lisp",
        "klacks/klacks.lisp",
        "klacks/klacks-impl.lisp",
        "klacks/tap-source.lisp",
    ],
    nowarn = ["implicit-generic"],
    deps = [":xml"],
)

lisp_library(
    name = "cxml",
    deps = [
        ":dom",
        ":klacks",
    ],
)
