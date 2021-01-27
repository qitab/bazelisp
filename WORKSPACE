# Copyright 2020-2021 Google LLC
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE file or at https://opensource.org/licenses/MIT.

# The WORKSPACE file for a project which uses these rules should:
#
# 1. Invoke a workspace rule to setup the bazelisp repository:
#
# local_repository(
#     name = "bazelisp",
#     path = "/path/to/this/project",
# )
#
# 2. Call the setup function below to set up bazelisp's dependencies:
#
# load("@bazelisp//:repositories.bzl", "bazelisp_repositories")
# bazelisp_repositories()
# load("@bazelisp//:setup.bzl", "bazelisp_setup")
# bazelisp_setup()
#
# Or to structure your project so that it can be imported by other projects,
# follow the same pattern as this project: repositories.bzl and setup.bzl
# are split into separate files because external repositories need to be setup
# before a Starlark file can be loaded which loads from those repositories,
# and load can only be called from the top level in a Starlark file.

load(":repositories.bzl", "bazelisp_repositories")
bazelisp_repositories()
load(":setup.bzl", "bazelisp_setup")
bazelisp_setup()
