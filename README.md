Common Lisp support for Bazel
=============================

This repository provides support for compiling Common Lisp code using
[bazel](http://bazel.io).

Bazel provides safe and fast, hermetic and deterministic, incremental
build for software written in a mix of languages notably including C++,
Java, Go, Python, Javascript --- and now Common Lisp.

Our Lisp support will use SBCL to build standalone Common Lisp
executables that statically link all C/C++ libraries except the most
basic ones: `libc`, `libm`, etc. This makes it easier to deploy or debug
production binaries without having to worry about subtle installation
issues or discrepancies between installations.

See our [article for ELS 2016](doc/els2016.pdf) for an explanation of
what is Bazel, what our Lisp support does and how it works.

Support is currently experimental, and currently requires you to
manually setup your `WORKSPACE`. See according section below.


Limitations as compared to ASDF
-------------------------------

[ASDF](http://common-lisp.net/project/asdf/) cannot do any of the
above. However there are many limitations to our support for building
Common Lisp software with Bazel, where it can not yet do what ASDF
can.  Notably, on the one hand Bazel isn't lightweight at all, and on
the other hand, it doesn't allow building then loading code in the
current image. Many systems use ASDF extensions or wrappers with no
current equivalent for Bazel, for which you may have to write
corresponding Bazel extensions or source code tweaks.

Currently, only SBCL is supported, and only on x86-64 for now. The
only operating systems tested were Linux and MacOS X, though with
minimal modifications it might work other systems including Windows.
Supporting other implementations and/or other architectures is not
conceptually difficult, but will require work. Making it as fast as
SBCL will be hard, for SBCL now has a "fasteval" interpreter that was
especially optimized to accelerate building with Bazel.


Setting up your WORKSPACE
-------------------------

Copy or symlink [lisp.WORKSPACE.bzl](lisp.WORKSPACE.bzl) into the
toplevel directory of your Bazel workspace, and edit your toplevel
`WORKSPACE` file to include the following:

    load("/lisp.WORKSPACE", "lisp_repositories")
    lisp_repositories(bazelisp="/path/to/bazelisp", sbcl="/path/to/base/of/sbcl")

The path to `bazelisp` is the full Unix path to the directory
containing this `README.md` file.

The path to the base of `sbcl` is the `--prefix` argument used when
building SBCL. This would be `/usr` when using sbcl from a debian or
RPM package for SBCL, or `/usr/local` by default when building SBCL
from source. The SBCL executable is `bin/sbcl` under this base
directory whereas the SBCL support files are in `lib/sbcl/` under that
base directory. For instance, I (Faré) personally use `stow` to manage
local software under either `/usr/local` or `~/local`, and my SBCL
path is `/home/fare/local/stow/sbcl`. YMMV.

By default, this SBCL installation is used only to bootstrap the
hermetic version of SBCL from the `@lisp__sbcl//` external repository.
But if you used the `@lisp__sbcl//...` targets to precompile a SBCL
installation that has all the patches expected by Bazel, and you
checked it into your source control with enough hermeticity, then you
can shorten your Common Lisp builds by using that directly rather than
rebuilding SBCL in every new checkout (assuming you don't have a
shared distributed cache). To that effect, edit `bazel/rules.bzl` and
change the value of the variable `SBCL_PACKAGE` to be
`"@lisp__sbcl_binary_distribution//:"` instead of `"@lisp__sbcl//:"`.


Using bazel to build Lisp code
------------------------------

Once your `WORKSPACE` is setup, you can build Lisp code with a command
like:

    ( cd /path/to/bazel ; JAVA_HOME=/path/to/jdk8 ./output/bazel build @lisp__hello//:hello )

Of course, you can and probably should create a small shell script or
shell function that will properly invoke `bazel` for you from the
proper workspace.


Building additional or updated Lisp systems
-------------------------------------------

If you want to build additional or updated Lisp systems, edit the
`lisp.WORKSPACE.bzl` file and modify or add the `BUILD` file for each
corresponding repository that you want to build. `BUILD` files and
ancillary support files are located in the [build_defs/](build_defs/)
directory. For now, see our existing `BUILD` files as examples,
because our documentation is otherwise lacking.

For repositories that include their own `BUILD` and `WORKSPACE` files,
use `native.local_repository()`, `native.http_archive()`, and
`native.git_repository()`, etc. If they do not include their own
`BUILD` file, then create or edit one in [build_defs/](build_defs/)
and use the variants `native.new_local_repository()`,
`native.new_http_archive()`, and `native.new_git_repository()`,
etc. Note that the naming convention for the workspace is
`lisp__foo_bar` where `foo_bar` is the name of your package with any
dash or period replaced by an underscore. The convention is that build
file is named `lisp__foo_bar.BUILD` and any ancillary file has a name
starting with the prefix `lisp__foo_bar.` to identify what repository
uses it.

Please send us updates via pull requests.
