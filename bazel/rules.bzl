# -*- mode: Python; -*-

"""Definition of the Lisp build rules.

These rules are used by projects that contain Lisp sources.
They are required to build Lisp binaries or libraries, or run Lisp tests.

The three rules defined here are:
 lisp_binary - for Lisp binaries,
 lisp_library - for Lisp libraries,
 lisp_test - a test binary run with blaze/bazel test.

Examples:
  load("//<path-to>/bazel/rules", "lisp_binary", "lisp_library", "lisp_test")

  lisp_library(
    name = "foo",
    srcs = ["foo.lisp"],
    deps = ["//lisp/log"],
  )

 lisp_binary(
    name = "bar",
    srcs = ["main.lisp"],
    deps = [":foo"],
 )

 lisp_test(
    name = "foo-test",
    srcs = ["foo-test.lisp"],
    deps = [
       ":foo",
       "//lisp/test",
    ],
 )

The compilation mode (--compilation_mode (fastbuild|opt|dbg)) can be
overridden for any target by defining LISP_COMPILATION_MODE.  E.g.
blaze (build|run|test) -c opt --define LISP_COMPILATION_MODE=dbg //target
"""
# TODO(czak): Need to provide a proper path here. See also the example above.
load(
    "@lisp__bazel//:bazel/rule-guts.bzl",
    "lisp_binary_guts",
    "lisp_library_guts",
    "lisp_test_guts",
    "BAZEL_LISP"
    )

def lisp_binary(name,
                srcs = [],
                deps = [],
                data = [],
                compile_data = [],
                features = [],
                order = "serial",
                nowarn = [],
                args = [],
                main = "main",
                image = BAZEL_LISP,
                save_runtime_options = True,
                precompile_generics = True,
                compressed = False,
                visibility = None,
                testonly = 0,
                csrcs = [],
                cdeps = [],
                copts = [],
                test = False,
                flaky = False,
                size = "medium",
                timeout = None,
                tags = [],
                stamp = -1,
                # TODO(czak): Need to provide proper path here.
                malloc = None, #"@c__tcmalloc//:tcmalloc_or_debug",
                verbose = None,
                **kwargs):
  """Bazel rule to create a binary executable from Common Lisp source files.

  The image file contains necessary SBCL contribs to be called into a
  development environment like emacs/swank for a debug session.
  The last point is given only in the fastbuild and dbg builds.
  The binary called by "name" is branded to the functionality
  expressed by the "main" function (default "cl-user::main").
  The binary created with "main" (not None) does not process the Lisp top-level
  command line options. Lisp debugger and interactive functions are disabled.
  This can be overridden using the LISP_MAIN environment variable at runtime.
  E.g LISP_MAIN="bazel:main" is used to transform any Lisp binary into
  the build/compile image used to compile and build other Lisp code.
  LISP_MAIN=T will cause a binary to invoke the interactive top-level REPL with
  an enabled Lisp debugger. Specifying main="nil" has the same effect.

  Outputs: <name>

  Args:
    name: the rule name. Also name of the executable to create.
    srcs: is a list of source labels.
    deps: is a list of dependency labels.
    data: runtime data available to the binary in the runfile directory.
    compile_data: data used at compilation time.
    features: a list of Common Lisp features applied while building target.
    order: takes values:
        "serial" - each source is compiled in an image with
          previous dependencies loaded (default).
        "multipass" - each source is compiled in an image with
          all dependencies loaded.
        "parallel" - each source is compiled independent from others
    nowarn: a list of suppressed Lisp warning types or warning handlers.
    args: default arguments passed to the binary or test on execution.
    main: specifies the entry point function (default: main).
    image: the base image used to compile the target (default BAZEL_LISP).
    save_runtime_options: save runtime options and prevent those being
        interpreted by the target binary with a main entry point (default True).
        Setting this to False allows SBCL to process following flags:
         --help, --version, --core, --dynamic-space-size, --control-stack-size.
    precompile_generics: precompile generic functions if True (as by default).
    compressed: if the generated core should be compressed.
    visibility: list of labels controlling which other rules can use this one.
    testonly: If 1, only test targets can use this rule.
    csrcs: a list of C/C++ source labels.
    cdeps: this will link the cc dependencies into the image.
    copts: a list of string values of options to pass to cc_library.
    test: indicates that the binary is a test.
    flaky: a flag indicating that a test is flaky.
    size: the size of a test: small, medium (default), large, enormous.
    timeout: test timeout: None (default), short, moderate, long, eternal.
        None indicates that the timeout should be chosen based on test size.
    tags: list of arbitrary text tags mostly useful for tests. E.g.: "local".
    stamp: C++ build stamp info (0 = no, 1 = yes, default -1 = bazel option).
    malloc: malloc implementation to be used for linking of cc code.
    verbose: internal numeric level of verbosity for the build rule.
    **kwargs: other common attributes for binary targets.

  For more information on the common rule attributes refer to:
  http://bazel.io/docs/build-encyclopedia.html#common-attributes
  """
  lisp_binary_guts(
      name, srcs=srcs, deps=deps, data=data, compile_data=compile_data,
      features=features, order=order, nowarn=nowarn, args=args, main=main,
      image=image, save_runtime_options=save_runtime_options,
      precompile_generics=precompile_generics, compressed=compressed,
      visibility=visibility, testonly=testonly,
      csrcs=csrcs, cdeps=cdeps, copts=copts,
      test=test, flaky=flaky, size=size, timeout=timeout,
      tags=tags, stamp=stamp, malloc=malloc, verbose=verbose, **kwargs)

def lisp_test(name, stamp=0, **kwargs):
  """Bazel rule to create a unit test from Common Lisp source files.

  Outputs: <name>

  The lisp_test rule is an alias for the lisp_binary rule.
  It takes nearly the same set of arguments as lisp_binary rule,
  yet fixes 'testonly' to 1 and 'test' to True.
  The 'stamp' argument is set to 0 by default.

  Args:
    name: the rule name. Also name of the test executable to create.
    stamp: a flag indicating whether the binaries should be stamped.
    **kwargs: other keyword arguments that are passed to lisp_binary.

  For more information on the rule attributes refer lisp_binary and
  http://bazel.io/docs/build-encyclopedia.html#common-attributes
  """
  lisp_test_guts(name, stamp=stamp, **kwargs)

def lisp_library(name,
                 srcs = [],
                 deps = [],
                 data = [],
                 compile_data = [],
                 features = [],
                 order = "serial",
                 nowarn = [],
                 image = BAZEL_LISP,
                 visibility = None,
                 testonly = 0,
                 csrcs = [],
                 cdeps = [],
                 copts = [],
                 verbose = None,
                 **kwargs):
  """Bazel rule to create a library from Common Lisp source files.

  Outputs: <name>.fasl

  Args:
    name: the rule name. Also name of the executable to create.
    srcs: is a list of source labels.
    deps: is a list of dependency labels.
    data: runtime data available to the binary in the runfile directory.
    compile_data: data used at compilation time.
    features: a list of Common Lisp features applied while building target.
    order: takes values:
        "serial" - each source is compiled in an image with
          previousdependencies loaded (default).
        "multipass" - each source is compiled in an image with
          all dependencies loaded.
        "parallel" - each source is compiled independent from others
    nowarn: a list of suppressed Lisp warning types or warning handlers.
    image: the base image used to compile the target (default BAZEL_LISP).
    visibility: list of labels controlling which other rules can use this one.
    testonly: If 1, only test targets can use this rule.
    csrcs: a list of C/C++ source labels.
    cdeps: this will link the cc dependencies into the image.
    copts: a list of string values of options to pass to cc_library.
    verbose: internal numeric level of verbosity for the build rule.
    **kwargs: other common attributes for binary targets.

  For more information on the common rule attributes refer to:
  http://bazel.io/docs/build-encyclopedia.html#common-attributes
  """
  lisp_library_guts(
      name, srcs=srcs, deps=deps, data=data, compile_data=compile_data,
      features=features, order=order, nowarn=nowarn, image=image,
      visibility=visibility, testonly=testonly,
      csrcs=csrcs, cdeps=cdeps, copts=copts, verbos=verbose, **kwargs)
