Common Lisp support for Bazel
=============================

This repository provides support for compiling Common Lisp code using [bazel](http://bazel.io).

Bazel provides safe and fast, hermetic and deterministic, incremental build for software written
in a mix of language notably including C++, Java, Go, Python, Javascript, and now Common Lisp.

Our Lisp support will use SBCL to build standalone Lisp executables that statically link
all C/C++ libraries except the most basic ones: libc, libm, etc.
This makes it easier to deploy or debug production binaries without having to worry
about subtle installation issues or discrepancies between installations.

ASDF cannot do any of the above. However Bazel has some restrictions for Commom Lisp.
Notably, Bazel isn't lightweight at all, and for now doesn't allow
building then loading code in the current image.

Currently, only SBCL is supported, and only on x86-64 for now.
The only operating system tested was Linux, though
it might work on MacOS X and possibly on Windows.

See our [article for ELS 2016](doc/els2016.pdf) for an explanation of what is Bazel,
what our Lisp support does and how it works.

Support is currently experimental, and currently requires you to manually setup your `WORKSPACE`.


Setting up your WORKSPACE
-------------------------

Copy or symlink [lisp.WORKSPACE.bzl](lisp.WORKSPACE.bzl) into the top of your bazel installation,
and edit the `WORKSPACE` for your bazel installation, to include the following:

    load("/lisp.WORKSPACE", "lisp_repositories")
    lisp_repositories("/full/path/to/where/you/checked/out/bazelisp")

Also, edit your copy of `lisp.WORKSPACE.bzl` to reflect the versions you want to use,
and particularly to reflect where to find an installed binary of SBCL with which to
bootstrap the rest. For example, if some SBCL is installed in `/usr/local/bin/sbcl`,
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


Using bazel to build Lisp code
------------------------------

Once your `WORKSPACE` is setup, you can build Lisp code with a command like:

   ( cd /path/to/bazel ; JAVA_HOME=/path/to/jdk8 ./output/bazel build @lisp__hello//:hello )

Of course, you can and probably should create a small shell script or shell function
that will properly invoke bazel for you.


Building more Lisp code with Bazel
----------------------------------

You'll have to edit the `lisp.WORKSPACE.bzl` file and/or your `WORKSPACE` file,
and to add a `BUILD` file to each package you build, either together with the source code,
or in the [build_defs/](build_defs/) directory.
For now, you'll have to see existing `BUILD` files as examples,
because this documentation is otherwise lacking.
