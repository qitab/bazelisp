#lang scribble/sigplan
@; @nocopyright @preprint
@;-*- Scheme -*-
@; To be built with:
@;   scribble --pdf els2016.scrbl

@(require scribble/base
          scriblib/autobib scriblib/footnote
          scribble/decode scribble/core scribble/manual-struct scribble/decode-struct
          scribble/html-properties scribble/tag
          (only-in scribble/core style)
          "bibliography.scrbl" "utils.rkt")

@;authorinfo["Douglas Katzman" "Google" "dougk@google.com"]
@authorinfo["James Y. Knight" "Google" "jyknight@google.com"]
@authorinfo["François-René Rideau" "Google" "tunes@google.com"]
@authorinfo["Andrzej Walczak" "Google" "czak@google.com"]

@conferenceinfo["ELS 2016" "May 9--10, Kraków, Poland"]
@copyrightyear{2016}

@title{Building Common Lisp programs using Bazel
              @(linebreak) @smaller{or Correct, Fast, Deterministic Builds for Lisp}}

@abstract{
  We will demonstrate how to build Common Lisp programs using Bazel,
  Google's hermetic and reproducible build system.
  Unlike the state of the art so far for building Lisp programs,
  Bazel ensures that incremental builds are always both fast and correct.
  With Bazel, one can statically link C libraries into one's SBCL runtime,
  making the executable file self-contained.
}

@category["D.2.3" "Software Engineering"]{Coding Tools and Techniques}

@keywords{
Build System,
Common Lisp,
Bazel,
Determinism,
Reproducibility,
Hermeticity
}

@section{Introduction}

Common Lisp, a general-purpose programming language,
is used at Google for the well-known low-fare search engine QPX @~cite[Inside-Orbitz].
Google now builds its Lisp code incrementally,
using Bazel, its recently open-sourced scalable build system.

Bazel is designed to build software in a reproducible and hermetic way.
Hermeticity means all build dependencies, including build tools such as compilers,
are kept under source control.
Bazel can thus assess what was or wasn't modified,
and either rebuild or reuse cached artifacts.
Reproducibility means building the same target multiple times from the same source code
produces the same output.
Reproducibility facilitates testing and debugging production code.
Bazel further enforces determinism by executing each build action in a container
wherein only declared inputs may be read, and any non-declared output is discarded.
Bazel can thus parallelize build actions, either locally or to remote workers.
Bazel supports writing software in a mix of languages notably including
C++, Java, Python, Go and Javascript.
Compilers and other build tools must be tuned to remove sources of non-determinism
such as timestamps, PRNG seeds, etc.

While mainly written in Java, Bazel is extensible using @italic{Skylark},
a subset of Python with strict limits on side-effects.
We used Skylark to add support for building software written in Common Lisp.

@section{Previous Work}

The state of the art so far for building large Common Lisp applications
is ASDF @~cite[ASDF3-2014].
A descendent of the original Lisp DEFSYSTEM from the 1970s,
ASDF builds all the code in the current Lisp image;
incremental builds may therefore be affected
by all kinds of potential side-effects in the current image;
and to guarantee a deterministic build one has to build from scratch.
ASDF also lacks good support for multi-language software.
An attempt to build Lisp code deterministically, XCVB @~cite[XCVB-2009],
failed for social and technical reasons, though it had a working prototype.

Meanwhile, QPX was built using an ad-hoc script loading hundreds of source files
before compiling them and reloading the resulting FASLs.
The multi-stage build was necessary because of circular dependencies between the files;
the dependencies formed a big "hairball"
which any replacement build solution had to handle.

@section{Building Lisp Software with Bazel}

The input to Bazel is a set of @(BUILD) files with Python-like syntax,
wherein users declaratively specify their software @emph{targets}
using @emph{rules}, and @emph{macros} that expand into one or more rules.

@; ------------- lisp_library ---------------------

Our first Lisp support function, @(lisp_library),
declares an intermediate target which can be referenced from other Lisp rules.
With SBCL (Steel Bank Common Lisp),
@(lisp_library) creates a FASt Load (FASL) archive
by concatenating the FASL files produced by compiling each of its Lisp sources.

Bazel users specify @emph{attributes} when defining rules.
For Lisp rules, these include the Lisp sources @(srcs),
Lisp libraries @(deps), C sources @(csrcs) and C libraries @(cdeps),
auxiliary @(data) available at runtime to all executable targets depending on the library,
and auxiliary @(compile_data) available at compile-time.

Lisp rules have additional build options.
The @(order) attribute notably specifies a build strategy;
the default @tt{"serial"} order loads each source file in sequence
before compiling the next file.
The @tt{"parallel"} order compiles all files in parallel without loading other ones.
The @tt{"multipass"} order first loads all source files,
then compiles each one separately in parallel,
which is useful to compile a "hairball" aggregate.

@verbatim[#:indent 3]|{
load("@lisp__bazel//:bazel/rules.bzl",
     "lisp_library")
lisp_library(
    name = "alexandria",
    srcs = ["package.lisp",
	# ...
        "io.lisp"],
    visibility = ["//visibility:public"])
}|

The above example is from the @(BUILD) file for the "alexandria" library of general utilities.
First, Bazel loads the definition of @(lisp_library)
from its conventional @italic{build label} using the @tt{lisp__bazel} "external repository".
The @(visibility) attribute indicates which @(BUILD) packages
are allowed to reference the rule's target --- in this case, it is visible to all packages.

The following command builds @file{alexandria.fasl} and makes it available at a well defined path:
@verbatim[#:indent 5]{bazel build :alexandria}

@; ------------- lisp_binary ---------------------

Our second Lisp support function, @(lisp_binary),
statically links an executable including both Lisp runtime and Lisp core image.
If Lisp or C sources are specified, they will be compiled into corresponding
Lisp and C components before being statically linked into the final binary.
Our third Lisp support function, @(lisp_test),
is a variation on the @(lisp_binary) rule meant to be invoked with the @tt{bazel test} command.

@verbatim[#:indent 3]|{
load("@lisp__bazel//:bazel/rules.bzl",
     "lisp_binary")
lisp_binary(
  name = "myapp",
  srcs = ["myapp.lisp"],
  main = "myapp:main",
  deps = [
    "@lisp__alexandria//:alexandria"])
}|
@; Insert a line above.
This @(BUILD) file contains a @(lisp_binary)
target which references the "alexandria" @(BUILD) target seen before.
At startup, Lisp function @cl{myapp:main} will be called with no arguments.
The program is compiled and executed using:
@verbatim[#:indent 5]{bazel run :myapp}

@; ------------- C++ dependencies ---------------------

A @(lisp_binary) can directly or transitively depend on C or C++ libraries.
Static linking of the libraries makes it more reliable to deploy such a binary
on multiple hosts in the cloud, without the opportunity to get library dependencies wrong;
in particular, it helps to minimize discrepancies between test and production environments.
C and C++ dependencies can be specified via the @(cdeps) rule attribute,
which can refer to any @(cc_library) built with Bazel.
The @(csrcs) and @(copts) rule attributes allow to directly specify C source files
for which an internal target will be generated.

@; ------------- Speed ---------------------

Thanks to these build rules, the duration of the incremental QPX build
went from about 15 minutes to about 90 seconds, with qualitative effects on developer experience.
However, this is for a large project, using a computing cloud for compilation.
The open source version of Bazel currently lacks the ability to distribute builds,
though it can already take advantage of multiple cores on a single machine.
The typical Lisp user will therefore not experience a similar speedup
when using the Bazel lisp rules;
but he may enjoy the incremental reproducibility.

@section{Inside the Lisp rules}

Lisp support was implemented using Bazel's extension language @italic{Skylark}.
@(lisp_binary), @(lisp_library), and @(lisp_test) are implemented as Skylark @italic{macros}
invoking several Skylark @italic{rules}.
A Skylark @italic{rule} consists of an implementation function
and a list of attribute specifications that notably define
type-checked inputs and outputs for the rule's target.
A Skylark @italic{macro} is a function that
can create a (directed acyclic) graph involving multiple rules.
The Lisp support uses macros to establish a separate graph
for each of the Lisp and C parts of the build,
then connecting these two graphs at the final binary targets.
Thus, the @(lisp_library) macro calls the @(_lisp_library) rule to create
Lisp related actions, and also calls the @(make_cdeps_library) macro
to create the C related targets using Skylark's @(native.cc_library).

The rules compile C sources and Lisp sources in parallel to each other,
and the resulting compilation outputs are combined together in the last step.
This improves the build parallelism and reduces the latency.
In order to facilitate linking, all C symbols referred to at Lisp-compilation time
are dumped into a linker script file.
The final linking step uses that @file{.lds} file to
include from C libraries only those object files containing referred symbols,
and to statically detect any missing or misspelled C symbol.
The @(lisp_binary) and the @(lisp_test) macros then create the executable
by combining the linked SBCL/C runtime with
a core image dumped after loading all FASLs.

The @(_lisp_library) rule implementation
computes the transitive dependencies from referenced targets,
compiles the sources using Skylark's @(ctx.action),
and returns a Lisp @italic{provider} structure to be used by other Lisp rules.
Each of the Lisp sources are compiled in a separate process,
possibly running on different machines.
This increases build parallelism and reduces the latency
when contrasted with waiting for each dependency to be compiled first
before its compilation output is loaded.
The compilation effects of one source are not seen when compiling other Lisp sources.

The Lisp provider structure contains transitive information about:
FASL files from each @(lisp_library) target;
all Lisp sources and reader features declared;
deferred warnings from each compilation;
the runtime and compilation data for each library.
The Lisp text @emph{sources} of the dependencies
are loaded before compiling an intermediate target.
The FASL files are only used when linking the final binary target.
The deferred compilation warnings --- mostly for undefined functions ---
are checked only after all FASL files have been loaded into the final target.

@section{Requirements}

The current version of the Lisp support for Bazel
has only been made to work with SBCL on Linux on the x86-64 architecture.
It should be relatively easy to get it working
on any platform that is supported by both SBCL and Bazel.
However, porting to a different Lisp implementation, while possible,
will require non-trivial work, especially with respect to linking C libraries into an executable,
or reproducing the low latency achieved with SBCL's @tt{fasteval} interpreter@~cite[FASTEVAL].

Bazel itself is an application written in Java.
It takes seconds to start for the first time;
then it becomes a server that can start an incremental build instantly
but consumes gigabytes of memory.
It isn't a lightweight solution for programming Lisp
@hyperlink["https://en.wikipedia.org/wiki/Programming_in_the_large_and_programming_in_the_small"]{@italic{in the small}};
but it is a robust solution for building software
@hyperlink["https://en.wikipedia.org/wiki/Programming_in_the_large_and_programming_in_the_small"]{@italic{in the large}}@;
@~cite[in-the-large-small].

@section{Conclusion and Future Work}

We have demonstrated simultaneously
how Common Lisp applications can be built in a fast and robust way,
and how Bazel can be extended to reasonably support a new language unforeseen by its authors.
Our code can be found at:

@hyperlink["http://github.com/qitab/bazelisp"]{@tt{http://github.com/qitab/bazelisp}}

In the future, we may want to add Lisp-side support for interactively controlling Bazel:
we would like to be able to build code, and load the result code into the current image,
without reloading unmodified fasls and object files.

@(generate-bib)
