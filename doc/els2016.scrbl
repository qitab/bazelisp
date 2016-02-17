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

Common Lisp is a universal programming language, @; Orbitz is gone :(
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
@item{ @; Note that :sb-fasteval is already standard in SBCL.
  Low-latency in loading dependencies using the new @tt{:sb-fasteval} feature of SBCL.
}]

@section{Discussion}

@; ------------- lisp_binary ---------------------

A @italic{lisp_binary} rule is used to link statically an executable with embedded
Lisp runtime and Lisp core image. The inputs to the binary are Lisp sources @italic{srcs},
Lisp libraries @italic{deps}, C++ sources @italic{csrcs} and libraries @italic{cdeps},
and auxiliary compile or runtime data.
If Lisp or C++ sources are specified, those will be compiled to the corresponding
Lisp/C++ library components before being linked statically into the final binary.
Further discussion about compilation related options is to be found with
the @(lisp_library) rule below.

The produced executable binary can be run as any program. For this purpose the
@italic{main} rule attribute specifies the symbol of the entry point,
which is the @italic{cl-user::main} function by default.

An example "hello, world" application is simply declared as follows:

@verbatim|{

;-> hello.lisp

(defun main ()
  (format t "Hello, world!~%"))

;-> BUILD

load("@lisp__bazel//:bazel/rules.bzl", "lisp_binary")

lisp_binary(
    name = "hello",
    srcs = ["hello.lisp"]
)}|

The above example contains the @italic{cl-user::main} function that is called
at image startup from the @italic{lisp_binary} wrapper specific to the Lisp implementation.
The @italic{main} function does not receive any arguments, as access to command line
arguments is not unified between Lisp implementations.
The @italic{BUILD} file contains the system description. First, Bazel needs to
load the corresponding definition of the @italic{lisp_binary} rule. It does find the
definitions by refering to the external repository label @italic{lisp__bazel} which
needs to be defined in the corresponding @italic{WORKSPACE} files.
Then it applies the @italic{lisp_binary} rule to the source file giving the target
"hello" as a name. The program is compiled, linked, and executed using the following command:

@verbatim{> bazel run :hello}

@; ------------- lisp_library ---------------------

A @italic{lisp_library} is usefull to declare an intermediate target which can be
referenced from other Lisp BUILD rules. For SBCL the @italic{lisp_library} creates
a fast load (FASL) archive, which is possibly a concatenation of FASL files produced
from compilation of each of its Lisp sources (@italic{srcs}).

The attributes of the @italic{lisp_library} rule are Lisp sources @italic{srcs},
Lisp libraries @italic{deps}, C++ sources @italic{csrcs} and libraries @italic{cdeps},
and auxiliary runtime @italic{data} or compile data @italic{compile_data}.
The runtime data will be available to all executable targets depending on this library.
The compile data is available at compile time.

The rule also accepts other Lisp build options. @italic{order} specifies the order
in which the files are loaded and compiled. The default "serial" order will
load each of the sources specified in sequence to compile the next Lisp source.
So each successive Lisp source can depend on data and info from previous Lisp sources.
The "parallel" compilation assures that each of the sources will be compiled without
the compilation loading other sources. And finally "multipass" order will load
all sources first before compiling each one separately. The last option is usefull
for compiling "hairball" kind of Lisp aggregates.

The Lisp compilation can be modified by specifying reader features using the @italic{features}
attribute. The features are set before loading any dependencies or compiling any sources for
the target. The features also propagate transitively to any targets that depend on the one
library which specified the reader features.

By default the Lisp compilation is strict and any warnings will fail the compilation.
In order to compile some "hairy" sources the @italic{nowarn} attribute can be usefull.
It accepts names of condition types or names of condition handlers.
There is a set of predefined warning types found in the @italic{bazel.warning} package.

@verbatim|{
;-> alexandria/BUILD

load("@lisp__bazel//:bazel/rules.bzl", "lisp_library")

lisp_library(
    name = "package",
    srcs = ["package.lisp"],
)

lisp_library(
    name = "alexandria",
    srcs = [
        "binding.lisp",
        "conditions.lisp",
	# ... some omitted
        "features.lisp",
        "io.lisp",
    ],
    deps = [":package"],
    order = "parallel",
    visibility = ["//visibility:public"],
)}|

The above example of a Lisp library is for "alexandria".
The library is compiled in the "parallel" @italic{order} because the sources just
depend on the @italic{package.lisp} file but not on each other. In practice
there is no much gain from compiling in "parallel" as opposed to the "serial" order -
the main advantage being the enforced independence of the sources through library evolution.
The @italic{deps} attribute specifies other Lisp library targets the "alexandria" library
depends on. The @italic{visibility} attribute allows to restrict the availability of that's rule target
to other BUILD packages. In this example, there is no restriction and the target is "public".

To build the library one needs to invoke the following Bazel command which will produce
@italic{alexandria.fasl} file.

@verbatim{> bazel build :alexandria}

The FASL file can be located in the @italic{blaze-genfiles} folder and contains the compiled
Lisp sources except for the "package.lisp" file, which are to be found in and loaded up-front
from the corresponding @italic{package.fasl} file.

@; ------------- lisp_test ---------------------

The last, but not least, BUILD rule to be introduced here is the @italic{lisp_test} rule.
The test rule is a variation on the @italic{lisp_binary} rule. It also produces
an executable program with a main entry point that can be executed on the command line.
The special purpose of the test rule is to run tests when invoked
with the @italic{bazel test} command.

@verbatim|{

;-> foo/test.lisp
(defun main ()
  (assert (= 720 (alexandria:factorial 6))))

;-> foo/BUILD

load("@lisp__bazel//:bazel/rules.bzl", "lisp_test")

lisp_test(
  name = "test",
  srcs = ["test.lisp"],
  deps = ["@lisp__alexandria//:alexandria"],
)}|

The above example contains a @italic{foo/test.lisp} file with a @italic{cl-user::main}
referencing the @italic{alexandria:factorial} function and an assertion.
The corresponding BUILD file has a @italic{foo:test} target defined which depends on
the above defined "alexandria" BUILD target and library.

The corresponding test can be run using the following Bazel command line:
@verbatim{> bazel test foo:test}


@; ------------- C++ dependencies ---------------------

TO BE WRITTEN

@; ------------- Etc ---------------------

Increased parallelism with reduced latency by loading dependencies in a fast interpreter,
rather than waiting for them to be compiled first.
This works fine with some restrictions:
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
or reproducing the low latency that was achieved with SBCL fasteval interpreter.

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
