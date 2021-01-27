# Copyright 2020-2021 Google LLC
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE file or at https://opensource.org/licenses/MIT.
"""Repository functions for setting up Bazelisp transitive dependencies."""

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")
load("@io_bazel_stardoc//:setup.bzl", "stardoc_repositories")

def bazelisp_setup():
    bazel_skylib_workspace()
    stardoc_repositories()
