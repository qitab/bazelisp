load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
    "new_git_repository",
)

git_repository(
    name = "bazel_skylib",
    remote = "https://github.com/bazelbuild/bazel-skylib.git",
    # tag = "1.0.3",
    commit = "2ec2e6d715e993d96ad6222770805b5bd25399ae",
    shallow_since = "1598536904 -0400",
)

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")
bazel_skylib_workspace()

git_repository(
    name = "io_bazel_stardoc",
    remote = "https://github.com/bazelbuild/stardoc.git",
    # tag = "0.4.0",
    commit = "4378e9b6bb2831de7143580594782f538f461180",
    shallow_since = "1570829166 -0400",
)

load("@io_bazel_stardoc//:setup.bzl", "stardoc_repositories")
stardoc_repositories()

git_repository(
    name = "rules_cc",
    remote = "https://github.com/bazelbuild/rules_cc.git",
    commit = "b1c40e1de81913a3c40e5948f78719c28152486d",
    shallow_since = "1605101351 -0800",
)

# Installed SBCL in /usr/bin/sbcl and /usr/lib/sbcl/*. See BUILD.local_sbcl for
# specific files referenced.
new_local_repository(
    name = "local_sbcl",
    path = "/usr/",
    build_file = "@//:BUILD.local_sbcl",
)

# For tools-to-build/{corefile.lisp,editcore.lisp}.
new_git_repository(
    name = "sbcl",
    remote = "https://github.com/sbcl/sbcl.git",
    # May need to be adjusted to match the installed SBCL version.
    # tag = "sbcl-2.0.6",
    commit = "1104c7fd508ec97d97c8f3f82f97e03ac55d7c62",
    shallow_since = "1593435415 +0100",
    build_file = "@//:BUILD.sbcl",
)
