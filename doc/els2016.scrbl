#lang scribble/sigplan
@; @nocopyright @preprint
@;-*- Scheme -*-

@(require scribble/base
          scriblib/autobib scriblib/footnote
          scribble/decode scribble/core scribble/manual-struct scribble/decode-struct
          scribble/html-properties scribble/tag
          (only-in scribble/core style)
          "bibliography.scrbl" "utils.rkt")

@authorinfo["Douglas Katzman" "Google" "dougk@google.com"]
@authorinfo["James Y. Knight" "Google" "jyknight@google.com"]
@authorinfo["François-René Rideau" "Google" "tunes@google.com"]
@authorinfo["Andrzej Walczak" "Google" "czak@google.com"]

@conferenceinfo["ELS 2016" "May 9--10, Kraków, Poland"]
@copyrightyear{2016}

@title{Building Common Lisp programs using Bazel
              @(linebreak) @smaller{or Correct, Fast, Deterministic Builds for Lisp}}

@keywords{
Build systems,
Lisp,
Common Lisp,
Bazel,
Deterministic,
Hermetic
}

@abstract{
  We will demonstrate how to build Common Lisp programs using Bazel,
  Google's hermetic and deterministic build system.
  Unlike the previous state of the art for building Lisp programs,
  Bazel ensures that incremental builds are always both fast and correct.
  With Bazel, you can also statically link C libraries into your SBCL runtime,
  allowing for self-contained deployment of your program as a single executable file.
  Currently, only SBCL is supported, and has only been tested on Linux.
  A faster evaluator for SBCL also reduces latency due to loading dependencies
  while maintaining compilation in isolated processes.
}

@section{Introduction}

Common Lisp is a universal programming language,
notably used at Google for its well-known server application QPX @~cite[Inside-Orbitz].
Google updated its Lisp code base so it can be built incrementally
using its internal build system, recently open-sourced as Bazel @~cite[Bazel].

Bazel is designed to build your software deterministically —
assuming you maintain hermeticity, as Google does.
Hermeticity in this context means that the build should only depend but
on programs and files that are checked into source control;
thus, Bazel can see when they are modified, and rebuild;
or it can see that they haven't been modified, and reuse cached build artifacts.
Bazel helps enforce determinism by executing each build action in a container
whereby only the action may only read from declared inputs,
and must produce all declared outputs, after which
all other temporary files are discarded.
Thanks to this isolation, build actions can be easily parallelized,
either locally on a multiprocessor, or to a distributed farm of build workers.

While mainly written in Java, Bazel is also extensible using Skylark,
a programming language that is essentially a subset of Python.
Using Skylark, three new rules were written to support building software written in Common Lisp:
@(lisp_library) for libraries (e.g. alexandria, or cl-ppcre),
@(lisp_binary) for executable binaries (e.g. the QPX server, or your favorite application),
and @(lisp_test) for running unit tests or integration tests.


@section{Previous Work}

The state of the art for building large Common Lisp applications up until then was ASDF.
An evolution of the original Lisp DEFSYSTEM from the 1970s,
ASDF builds all the code in the current Lisp image.
Unhappily, building in the current Lisp image means that
incremental builds may be affected by all kinds of side-effects.
Therefore, the only completely reliable way to get a deterministic build is to build from scratch.
Modifying ASDF to build deterministically in isolated processes,
while conceptually imaginable, wasn't possible because
no one fully understood the dependency model of ASDF
— at least, until it was eventually fixed in 2013 @~cite[ASDF3-2014];
such a modification is now possible but the work remains to be done.

A previous attempt to build Lisp code deterministically, XCVB @~cite[XCVB-2009],
was a failure, for social as well as technical reasons,
including a lack of a sufficiently smooth transition from previous build systems,
and lack of managerial support, until resources were diverted towards ASDF and Bazel.

Meanwhile, QPX itself was built using an ad-hoc script that had to load all of hundreds of files
before to compile them and reload them:
this multi-stage build was made necessary because
there were circular dependencies between those files;
from the point of view of dependencies they formed a big "hairball".
An effort was made to chip at the "hairball" by moving files into
a "prefix" of files without circular dependencies that the "hairball" depends on,
and a "suffix" of files without circular dependencies that depend on the "hairball";
but this cleanup isn't backed by a strong will, and requires splitting files in two (or more),
and otherwise reorganizing the code, which in turns necessitates design effort
requiring the involvement of senior engineers for whom this is not a high priority.
Therefore, the replacement for that build script had to be able to handle that "hairball".


@section{Features}

The features that we will demonstrate include:
@itemlist[
@item{
  Building a simple "hello, world" application in Common Lisp, using @(lisp_binary).
}
@item{
  Building a simple library using @(lisp_library).
}
@item{
  Running simple tests using @(lisp_test).
}
@item{
  Including C libraries in a Lisp binary by depending on a @(cc_library).
}
@item{
  More complex dependency graphs using multiple @(lisp_library) rules.
}
@item{
  Handling dependency hairballs using the @tt{"multipass"} feature.
}
@item{
  Low-latency in loading dependencies using the new @tt{:sb-fasteval} feature of SBCL.
}]

@section{Discussion}

TO BE WRITTEN

Increased parallelism with reduced latency by loading dependencies in a fast interpreter,
rather than waiting for them to be compiled first.
This works fine as long as, with some restrictions:
(a) Some code may use @cl{eval-when} incorrectly and fail to support loading without compiling;
it will have to be fixed.
(b) Some code may do heavy computations at macro-expansion time and run somewhat slowly
because it's not compiled; explicitly calling @cl{compile} may speed up that code.

@section{Inside the Lisp rules}

TO BE WRITTEN

@section{Speed}

TO BE WRITTEN

Comparison between building with ASDF vs building with Bazel:
Incremental build vs build all Lisp code "from scratch"
vs build all code "really from scratch" (including SBCL).
Consider speedup from using a multiprocessor (vanilla ASDF can't take advantage of them).

@section{Requirements}

The current version of these Common Lisp rules for Bazel only work with SBCL.
Porting to a different Lisp implementation, while possible,
may require non-trivial work, especially with respect to linking C libraries into an executable,
or to reproducing the low latency that was achieved with SBCL.

These Common Lisp rules have only been tested on Linux on the x86-64 architecture;
but they should be relatively easy to get to work on a different operating system and architecture,
as long as they are supported by both SBCL and Bazel:
for instance on MacOS X, and possibly, on Windows or on mobile devices.

Bazel itself is an application written in Java that takes many seconds to start the first time;
then it becomes a server that eats several gigabytes of memory, but can start your build instantly.
It isn't a lightweight solution for programming Lisp in the small;
but it is a robust solution for building quality software in Lisp in an industrial setting.


@section{Conclusion and Future Work}

We have demonstrated simultaneously
how Common Lisp applications can be built in a fast and robust way,
and how Bazel can be extended to reasonably support a new language unforeseen by its authors.

In the future, we may want to add Lisp-side support for interactively controlling Bazel:
we would like to be able to build code, and load the result code into the current image,
without reloading unmodified fasls and object files;
we would also like to be able to run tests with Bazel and interactively debug those that fail.

@(generate-bib)
