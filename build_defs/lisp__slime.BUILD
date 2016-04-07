# Description: Slime debugger

licenses(["restricted"])  # GPL, GPLv2, LLGPL, public domain, MIT

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "swank",
    srcs = [
        # Order is important here.  These are the sources for SBCL only.
        "swank-loader.lisp",
        "packages.lisp",
        "swank/backend.lisp",
        "swank/source-path-parser.lisp",
        "swank/source-file-cache.lisp",
        "swank/sbcl.lisp",
        "swank/gray.lisp",
        "swank/match.lisp",
        "swank/rpc.lisp",
        "swank.lisp",
        "google-init.lisp",
    ],
    nowarn = [
        "style-warning",
        "simple-warning",
    ],
    visibility = ["//visibility:public"],
)

lisp_library(
    name = "slime",
    srcs = [
        # Order is important here.
        "contrib/swank-util.lisp",
        "contrib/swank-repl.lisp",
        "contrib/swank-c-p-c.lisp",
        "contrib/swank-arglists.lisp",
        "contrib/swank-fuzzy.lisp",
        "contrib/swank-fancy-inspector.lisp",
        "contrib/swank-presentations.lisp",
        "contrib/swank-presentation-streams.lisp",
        "contrib/swank-asdf.lisp",
        "contrib/swank-package-fu.lisp",
        "contrib/swank-hyperdoc.lisp",
        "contrib/swank-sbcl-exts.lisp",
        "contrib/swank-mrepl.lisp",
        "contrib/swank-trace-dialog.lisp",
        "contrib/swank-macrostep.lisp",
        # The following files are generally loaded by Slime because of a client
        # request.  Load them now to avoid problems when a client connects to a
        # Swank server that's running on Borg.
        "contrib/swank-sprof.lisp",
        "contrib/swank-indentation.lisp",
    ],
    nowarn = ["style-warning"],
    visibility = ["//visibility:public"],
    deps = [
        ":swank",
        "@lisp__asdf//:asdf",
    ],
)

# load("//devtools/editors/emacs:emacs.bzl", "el_library", "el_build_test")
# 
# el_library(
#     name = "emacs",
#     srcs = glob([
#         "contrib/*.el",
#         "lib/macrostep.el",
#     ]) + ["slime.el"],
#     build_flavors = [
#         "google-emacs",
#         "emacs24",
#         "emacs-snapshot",
#     ],
#     data = glob([
#         "*.lisp",
#         "contrib/*.lisp",
#         "lib/*.lisp",
#         "swank/*.lisp",
#     ]) + ["ChangeLog"],
#     visibility = [
#         "//devtools/editors/emacs/infrastructure/debian:__pkg__",
#         "//travel/tools/emacs:__pkg__",
#     ],
#     warnings_as_errors = False,
#     deps = [
#         ":autoloads",
#         ":hyperspec",
#     ],
# )

# el_build_test(
#     name = "emacs_test",
#     deps = [":emacs"],
# )

# TODO(phst): Try to get the unit test working, then uncomment the rule below.
# ert_test(
#     name = "emacs_test",
#     srcs = glob(["contrib/test/*-tests.el"]) + ["slime-tests.el"],
#     deps = [":emacs"],
# )

# el_library(
#     name = "autoloads",
#     srcs = ["slime-autoloads.el"],
#     build_flavors = [],
#     supported_flavors = [
#         "google-emacs",
#         "emacs24",
#         "emacs-snapshot",
#     ],
#     visibility = ["//devtools/editors/emacs/infrastructure/debian:__pkg__"],
# )

# el_library(
#     name = "hyperspec",
#     srcs = ["lib/hyperspec.el"],
#     build_flavors = [
#         "google-emacs",
#         "emacs24",
#         "emacs-snapshot",
#     ],
#     visibility = ["//devtools/editors/emacs/infrastructure/debian:__pkg__"],
#     warnings_as_errors = False,
# )

# el_build_test(
#     name = "hyperspec_test",
#     deps = [":hyperspec"],
# )
