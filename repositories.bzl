# Copyright 2020-2021 Google LLC
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE file or at https://opensource.org/licenses/MIT.
"""Repository functions for setting up Bazelisp dependencies."""

load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
    "new_git_repository",
)

def _include_if_not_defined(repo_rule, name, **kwargs):
    if not native.existing_rule(name):
        repo_rule(name = name, **kwargs)

def bazelisp_repositories():
    """Setup workspace for bazelisp dependencies, if not done already."""
    _include_if_not_defined(
        git_repository,
        name = "bazel_skylib",
        remote = "https://github.com/bazelbuild/bazel-skylib.git",
        # tag = "1.0.3",
        commit = "2ec2e6d715e993d96ad6222770805b5bd25399ae",
        shallow_since = "1598536904 -0400",
    )

    _include_if_not_defined(
        git_repository,
        name = "io_bazel_stardoc",
        remote = "https://github.com/bazelbuild/stardoc.git",
        # tag = "0.4.0",
        commit = "4378e9b6bb2831de7143580594782f538f461180",
        shallow_since = "1570829166 -0400",
    )

    _include_if_not_defined(
        git_repository,
        name = "rules_cc",
        remote = "https://github.com/bazelbuild/rules_cc.git",
        commit = "b1c40e1de81913a3c40e5948f78719c28152486d",
        shallow_since = "1605101351 -0800",
    )

    # Installed SBCL in /usr/bin/sbcl and /usr/lib/sbcl/*. See BUILD.local_sbcl
    # for specific files referenced.
    _include_if_not_defined(
        native.new_local_repository,
        name = "local_sbcl",
        path = "/usr/",
        build_file = "//:BUILD.local_sbcl",
    )

    # For tools-to-build/{corefile.lisp,editcore.lisp}.
    _include_if_not_defined(
        new_git_repository,
        name = "sbcl",
        remote = "https://github.com/sbcl/sbcl.git",
        # May need to be adjusted to match the installed SBCL version.
        # tag = "sbcl-2.0.11",
        commit = "58d68fe6b355fbca8a539a129b1b51e0340ccdec",
        shallow_since = "1606649057 +0000",
        build_file = "//:BUILD.sbcl",
    )
