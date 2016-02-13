Common Lisp support for Bazel
=============================

This repository provides support for compiling Common Lisp code using [bazel](http://bazel.io).

Support is currently experimental, and currently requires you to manually setup your `WORKSPACE`.


Setting up your WORKSPACE
-------------------------

Copy [lisp.WORKSPACE.bzl](lisp.WORKSPACE.bzl) into the top of your bazel installation, and
edit the `WORKSPACE` for your bazel installation, to include the following:

    load("/lisp.WORKSPACE", "lisp_repositories")
    lisp_repository("/full/path/to/where/you/checked/out/bazelisp")

Also, edit your copy of `lisp.WORKSPACE.bzl` to reflect the versions you want to use,
and particularly to reflect where to find an installed binary of SBCL with which to
bootstrap the rest. For install, if some SBCL is installed in `/usr/local/bin/sbcl`,
with the support files in `/usr/local/lib/sbcl/`, then use:

    native.new_local_repository(
        name = "lisp__sbcl_binary_distribution",
        path = "/usr/local",
        build_file = base_dir + "/build_defs/lisp__sbcl_binary_distribution.BUILD"
    )

You may also add more libraries that you use. If they include their own `BUILD` file
and `WORKSPACE` file, then use `native.local_repository()`, `native.http_archive`,
`native.git_repository`, etc. If they do not include their own `BUILD` file, then
create one in [build_defs/](build_defs/) and use the variants `native.new_local_repository()`,
`native.new_http_archive`, `native.new_git_repository`, etc. Note that the naming convention
for the workspace is `lisp__foo_bar` where `foo_bar` is the name of your package
with any dash or period replaced by an underscore.

Please send us updates to such definitions, e.g. via pull requests.
