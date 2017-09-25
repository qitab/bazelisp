# -*- mode: Python; -*-

"""Definition of the Lisp build rules.

These rules are used by projects that contain Lisp sources.
They are required to build Lisp binaries or libraries, or run Lisp tests.

The three rules defined here are:
 lisp_binary - for Lisp binaries,
 lisp_library - for Lisp libraries,
 lisp_test - a test binary run with bazel test.


The code here defines a few Skylark "real" rules and wraps them in
"Skylark macro" functions. The few real rules have following names:
 _lisp_binary
 _lisp_library
 _combine_lisp_binary
 _combine_lisp_test
These "rule class names" are used to find Lisp targets.
This is useful with 'bazel query' and for tools indexing the Lisp codebase.

Examples:
  load("@lisp__bazel//:bazel/rules.bzl",
    "lisp_binary", "lisp_library", "lisp_test")

  lisp_library(
    name = "foo",
    srcs = ["foo.lisp"],
    deps = ["@lisp__alexandria//:alexandria"],
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
       "@lisp__fiveam//:fiveam",
    ],
 )

The compilation mode (--compilation_mode (fastbuild|opt|dbg)) can be
overridden for any target by defining LISP_COMPILATION_MODE.  E.g.
bazel (build|run|test) -c opt --define LISP_COMPILATION_MODE=dbg //target
"""

load("@local_config_lisp//:config.bzl",
     "config_cpu",
     "config_os")

load("@lisp__bazel//:bazel/provider.bzl",
     "transitive_deps",
     "extend_lisp_provider")

BAZEL_LISP = "@lisp__bazel//:bazel"

BAZEL_LISP_MAIN = "bazel.main::main"

# If you already have it all precompiled in your source control,
# including libsbcl.a and libsbcl-exported-symbols.lds,
# and don't want to recompile from source, use:
# SBCL_PACKAGE = "@lisp__sbcl_binary_distribution//:"
# Otherwise, use:
SBCL_PACKAGE = "@lisp__sbcl//:"

lisp_files = FileType([".lisp", ".lsp"])

# Common attributes accepted by the (internal) lisp rules.
_lisp_common_attrs = {
    "srcs": attr.label_list(
        allow_files = lisp_files),
    "deps": attr.label_list(
        providers = ["lisp"]
    ),
    "order": attr.string(
        default = "serial",
        # TODO(czak): Fix VALUES param in attr.string.
        # values = ["multipass", "serial", "parallel"]
        ),
    # runtime data - is data available at runtime.
    "data": attr.label_list(
        allow_files=True,
        cfg = "data"),
    # compile data - is data available at compile and load time.
    "compile_data": attr.label_list(
        allow_files=True),
    "lisp_features": attr.string_list(),
    "nowarn": attr.string_list(),
    # TODO(czak): Rename to "build_image".
    "image": attr.label(
        allow_files=True,
        single_file=True,
        executable=True,
        cfg="host",
        default=Label(BAZEL_LISP)),
    "verbose": attr.int(),
    # Internal, for testing coverage.
    "enable_coverage" : attr.bool()
}

def _paths(files):
  """Return the full file paths for all the 'files'.

  Args:
   files: a list of build file objects.
  Returns:
   A space-separated string of file paths.
  """
  return " ".join([f.path for f in files])

def _short_paths(files):
  """Return the short file paths for all the 'files'.

  Args:
   files: a list of build file objects.
  Returns:
   A space-separated string of file short-paths.
  """
  return " ".join([f.short_path for f in files])

def _lisp_file_stem(path):
  """Remove the .lisp or .lsp suffix from Lisp file 'path'.

  Args:
   path: a string representing a build file path and name.
  Returns:
   The path with the file extension removed.
  """
  if path.endswith(".lisp"):
    return path[:-5]
  elif path.endswith(".lsp"):
    return path[:-4]
  else:
    fail("Cannot stem a lisp file name: %s" % path)

def _concat_files(ctx, inputs, output):
  """An action to concatenate the 'inputs' into 'outputs' in context 'cxt'.

  Args:
    ctx: the build context used to instantiate the concatenate build action.
    inputs: a list of files to concatenate.
    output: a file to store the concatenated contents.
  """
  count = len(inputs)
  if count == 0:
    cmd = "touch %s" % output.path
    msg = "Linking %s (as empty FASL)" % (output.short_path,)
  elif count == 1:
    # Use CP instead of MV as this is more portable.
    cmd = "cp %s %s" % (inputs[0].path, output.path)
    msg = "Linking %s (from 1 source)" % (output.short_path,)
  else:
    cmd = "cat %s > %s" % (_paths(inputs), output.path)
    msg = "Linking %s (from %d sources)" % (output.short_path, count)

  ctx.action(
      inputs = inputs,
      outputs = [output],
      progress_message = msg,
      mnemonic = "LispConcatFASLs",
      command = cmd)

def _default_flags(ctx, trans, verbose_level):
  """Returns a list of default flags based on the context and trans provider.

  Args:
   ctx: the context of the compile action.
   trans: the Lisp provider with transitive dependencies.
   verbose_level: if positive a --verbose flags is added.
  Returns:
    A list of flags
  """
  flags = [
      "--compilation-mode",
      ctx.var.get("LISP_COMPILATION_MODE", ctx.var["COMPILATION_MODE"]),
      "--gendir", ctx.configuration.genfiles_dir.path,
      "--features", " ".join(list(trans.features))]

  if (ctx.configuration.coverage_enabled or
      (hasattr(ctx.attr, "enable_coverage") and ctx.attr.enable_coverage)):
    flags += ["--coverage"]

  if verbose_level > 0:
    flags += ["--verbose", str(verbose_level)]

  cpp_options = set(ctx.fragments.cpp.compiler_options([]))
  if "-UNDEBUG" in cpp_options:
    flags += ["--safety", "3"]
  elif "-DNDEBUG" in cpp_options:
    flags += ["--safety", "0"]

  if int(ctx.var.get("LISP_BUILD_FORCE", "0")) > 0:
    flags += ["--force"]

  return flags

def _compile_srcs(ctx, srcs, deps, image, order,
                  compile_data, flags, nowarn, verbosep):
  """Compiles the 'srcs' in the context 'ctx'.

  Args:
   ctx: the context used to instantiate the compile actions.
   srcs: the Lisp file sources.
   deps: are the dependencies for the compilation.
   image: the Lisp executable image used to compile the sources.
   order: the order in which sources are loaded and compiled.
   compile_data: is additional data used to compile the sources.
   flags: are the flags to be passed to Lisp compilation image.
   nowarn: is the list of suppressed Lisp warning types or warning handlers.
   verbosep: is a flag that if True, prints some verbose warnings.
  Returns:
   A structure with FASLs, hash files, and warning files.
  """

  if order not in ["multipass", "serial", "parallel"]:
    fail("Accepted values: multipass, serial, parallel", "order")

  build_image = list(image.files)[0]
  compile_image = build_image

  if hasattr(image, "lisp"):
    # The image already includes some deps.
    included = image.lisp
    deps = [d for d in deps if not d in included.srcs]
  env = { "LISP_MAIN": BAZEL_LISP_MAIN }

  serial = False
  multipass = False
  # Name this variable load_ instead of load to avoid Bazel syntax errors.
  load_ = []
  if order == "multipass":
    load_ += srcs
    multipass = True
  elif order == "serial":
    serial = True

  # Arbitrary heuristic to reduce load on the build system by bundling
  # FASL and source files load into one compile-image binary.
  if ((len(srcs) - 1) * len(deps) > 100):
    # Generate a SRCS image.
    compile_image = ctx.new_file(ctx.label.name + ".srcs.image")
    srcs_flags = flags
    srcs_flags += ["--outs", compile_image.path]
    if deps:   srcs_flags += ["--deps", _paths(deps)]
    if load_:   srcs_flags += ["--load", _paths(load_)]
    if nowarn: srcs_flags += ["--nowarn", " ".join(nowarn)]

    inputs = sorted(set() + [build_image] + compile_data + deps + load_)
    msg = "Preparing %s (from %d deps" % (compile_image.short_path, len(deps))
    if load_:
      msg += " and %d srcs)" % len(load_)
    else:
      msg += ")"
    ctx.action(
        outputs = [compile_image],
        inputs = inputs,
        progress_message = msg,
        mnemonic = "LispSourceImage",
        env = env,
        arguments = ["binary"] + srcs_flags,
        executable = build_image)
    # All deps included above
    # TODO(czak): Add dedup code and remove --deps-already-loaded.
    flags += ["--deps-already-loaded"]

  if multipass:
    nowarn += ["redefined-method", "redefined-function"]

  if verbosep:
    print("Target: %s" % str(ctx.label.name))
    print("Build Img: %s" % build_image.short_path)
    print("Compile Img: %s" % compile_image.short_path)

  fasls = []
  warnings = []
  hashes = []
  for src in srcs:
    stem = _lisp_file_stem(src.path)
    file_flags = flags
    outs = [ctx.new_file(stem + e) for e in ["~.fasl", "~.hash", "~.warnings"]]
    fasls    += [outs[0]]
    hashes   += [outs[1]]
    warnings += [outs[2]]
    file_flags += ["--outs", _paths(outs)]
    file_flags += ["--srcs", src.path]

    if deps:   file_flags += ["--deps", _paths(deps)]
    if load_: file_flags += ["--load", _paths(load_)]
    if nowarn: file_flags += ["--nowarn", " ".join(nowarn)]

    inputs = sorted(set([compile_image, src]) + compile_data + deps + load_)
    ctx.action(
        outputs = outs,
        inputs = inputs,
        progress_message = "Compiling %s" % src.short_path,
        mnemonic = "LispCompile",
        env = env,
        arguments = ["compile"] + file_flags,
        executable = compile_image)

    if serial: load_ += [src]

  return struct(
      fasls = fasls,
      hashes = hashes,
      warnings = warnings)

################################################################################
# Lisp Binary and Lisp Test
################################################################################

def _lisp_binary_implementation(ctx):
  """Lisp specific implementation for lisp_binary and lisp_test rules."""
  verbose_level = max(ctx.attr.verbose,
                      int(ctx.var.get("VERBOSE_LISP_BUILD", "0")))
  verbosep = verbose_level > 0

  if verbosep:
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    print("Executable Core: %s" % ctx.attr.binary_name)


  trans = extend_lisp_provider(
      transitive_deps(ctx.attr.deps, image = ctx.attr.image),
      # Add those features to trans already.
      features = ctx.attr.lisp_features,
      data = ctx.files.data,
      compile_data = ctx.files.compile_data)

  flags = _default_flags(ctx, trans, verbose_level)

  nowarn = ctx.attr.nowarn

  if ctx.files.srcs:
    compile = _compile_srcs(
        ctx = ctx,
        srcs = ctx.files.srcs,
        deps = trans.srcs,
        image = ctx.attr.image,
        order = ctx.attr.order,
        compile_data = trans.compile_data,
        flags = flags,
        nowarn = nowarn,
        verbosep = verbosep)
  else:
    compile = struct(fasls=[], hashes=[], warnings=[])

  # TODO(czak): Add --hashes, and --warnings flags to bazl.main.
  # TOOD(czak): Fix: set([1, 2, 3]) + set([2, 4])
  deps = trans.deps
  hashes = trans.hashes + compile.hashes
  warnings = trans.warnings + compile.warnings

  if hasattr(ctx.attr.image, "lisp"):
    # The image already includes some deps.
    included = ctx.attr.image.lisp
    deps = set([d for d in deps if not d in included.deps])
    hashes = set([h for h in hashes if not h in included.hashes])
    warnings = set([w for w in warnings if not w in included.warnings])
  dump_symtable = ctx.file._dump_symtable
  build_image = ctx.file.image
  if verbosep:
    print("Build image: %s" % build_image.short_path)

  inputs = sorted(set([build_image, dump_symtable])
                  + deps + compile.fasls + trans.compile_data
                  + hashes + warnings)

  core = ctx.outputs.core
  dynamic_list_lds = ctx.outputs.dynamic_list_lds
  extern_symbols = ctx.outputs.extern_symbols
  lisp_symbols = ctx.outputs.lisp_symbols
  outs = [core, dynamic_list_lds, extern_symbols, lisp_symbols]

  flags += ["--deps", _paths(deps)]
  flags += ["--srcs", _paths(compile.fasls)]
  flags += ["--warnings", _paths(warnings)]
  flags += ["--hashes", _paths(hashes)]
  flags += ["--outs", _paths(outs)]
  flags += ["--dump-extern-symbols", extern_symbols.path]
  flags += ["--dump-dynamic-list-lds", dynamic_list_lds.path]
  flags += ["--main", ctx.attr.main]
  if nowarn:                        flags += ["--nowarn", " ".join(nowarn)]
  if ctx.attr.precompile_generics:  flags += ["--precompile-generics"]
  if ctx.attr.compressed:           flags += ["--compressed"]
  if ctx.attr.save_runtime_options: flags += ["--save-runtime-options"]

  cmd = ("LISP_MAIN=%s %s binary '%s'; " +
         "LISP_MAIN=t %s --script %s > %s") % (
             BAZEL_LISP_MAIN, build_image.path, "' '".join(flags),
             core.path, dump_symtable.path, lisp_symbols.path)

  ctx.action(
      outputs = outs,
      inputs = inputs,
      progress_message = "Linking %s" % core.short_path,
      mnemonic = "LispCore",
      command = cmd)

  return struct(
      image = ctx.outputs.core,
      runtime_data = trans.runtime_data,
      # The image also provides a lisp environment.
      # TODO(czak): Need to provide the srcs as lisp_library.
      # This way it can be loaded, if this image is used to compile Lisp.
      lisp = extend_lisp_provider(
          trans,
          srcs = ctx.files.srcs,
          hashes = compile.hashes,
          warnings = compile.warnings))

# Internal rule used to generate action that creates a Lisp binary core.
# Keep the name to be _lisp_binary - Grok depends on this name to find targets.
_lisp_binary = rule(
    implementation = _lisp_binary_implementation,
    output_to_genfiles = True,
    # Access to the cpp compiler options.
    fragments = ["cpp"],
    attrs = _lisp_common_attrs + {
        "main": attr.string(default="main"),
        "precompile_generics": attr.bool(),
        "compressed": attr.bool(),
        "save_runtime_options": attr.bool(),
        "binary_name": attr.string(),
        "_dump_symtable": attr.label(
            allow_files=True,
            single_file=True,
            # TODO(czak): Need to provide a proper path for this.
            default=Label("@lisp__bazel//:bazel/dump-symtable.lisp"))},
    outputs = {"core": "%{binary_name}.core",
               "dynamic_list_lds": "%{binary_name}.dynamic-list.lds",
               "extern_symbols": "%{binary_name}.extern.S",
               "lisp_symbols": "%{binary_name}.lisp.S"})


# Attributes used by _combine_lisp_* rules.
_combine_lisp_binary_attrs = {
    "data": attr.label_list(cfg="data", allow_files=True),
    "runtime": attr.label(allow_files = True, single_file = True),
    "core": attr.label(providers=["image", "runtime_data"]),
    "_combine": attr.label(
        executable=True,
        cfg="host",
        allow_files=True,
        single_file=True,
        # TODO(czak): Need to provide a proper path.
        default = Label("@lisp__bazel//:bazel"))}

def _combine_core_and_runtime(ctx):
  """An action that combines a Lisp core and C++ runtime."""
  # Implementation: _combine_lisp_binary, _combine_lisp_test
  ctx.action(
      inputs = [ctx.file.runtime, ctx.attr.core.image],
      outputs = [ctx.outputs.executable],
      progress_message = "Linking %s" % ctx.outputs.executable.short_path,
      arguments = ["combine",
                   "--run-time", ctx.file.runtime.path,
                   "--core", ctx.attr.core.image.path,
                   "--output", ctx.outputs.executable.path,
                   "--verbose", ctx.var.get("VERBOSE_LISP_BUILD", "0")],
      executable = ctx.executable._combine)

  runfiles = ctx.runfiles(
      files = sorted(set(ctx.files.data) + ctx.attr.core.runtime_data))

  if hasattr(ctx.attr.core, "lisp"):
    trans = ctx.attr.core.lisp
    return struct(
        lisp = trans,
        runfiles = runfiles)
  else:
    return struct(runfiles = runfiles)

# Internal rule used to combine a Lisp core and C++ binary to a Lisp binary.
_combine_lisp_binary = rule(
    implementation = _combine_core_and_runtime,
    executable = True,
    attrs = _combine_lisp_binary_attrs)

# Internal rule used to combine a Lisp core and C++ binary to a Lisp test.
_combine_lisp_test = rule(
    implementation = _combine_core_and_runtime,
    executable = True,
    test = True,
    attrs = _combine_lisp_binary_attrs)

# DEPS file is used to list all the Lisp sources for a target.
# It is a quick hack to make (bazel:load ...) work.
def _dump_lisp_deps_impl(ctx):
  """Creates a file that lists all Lisp files needed by the target in order."""
  # Implementation: _dump_lisp_deps
  trans = extend_lisp_provider(
      transitive_deps(ctx.attr.deps, image = ctx.attr.image),
      # Add those to trans.
      features = ctx.attr.lisp_features,
      srcs = ctx.files.srcs)
  ctx.file_action(
      output = ctx.outputs.deps,
      content = (
          "\n".join(["feature: " + f for f in trans.features] +
                    ["src: " + f.path for f in trans.srcs])))


# linkopts entry to force use of a symbol, so the .o that has the symbol
# won't be omitted when linking the containing .a into a cc_binary
# This function is used by lisp__sbcl.BUILD
def ld_use_symbol(sym):
  return "-Wl,-u,%s%s" % (
    "_" if config_os == "darwin" else "",
    sym)


# Internal rule that creates a Lisp library DEPS file.
# DEPS file is used to list all the Lisp sources for a target.
# It is a quick hack to make (bazel:load ...) work.
_dump_lisp_deps = rule(
    implementation = _dump_lisp_deps_impl,
    attrs = {
        "library_name": attr.string(),
        # TODO(czak): Share with _lisp_common_attrs.
        "srcs": attr.label_list(allow_files = lisp_files),
        "deps": attr.label_list(providers = ["lisp"]),
        "lisp_features": attr.string_list(),
        "image": attr.label(
            allow_files=True,
            single_file=True,
            executable=True,
            cfg="host",
            default=Label(BAZEL_LISP)),
        },
    outputs = {"deps": "%{library_name}.deps"},
    executable = False,
    output_to_genfiles = True)

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
                malloc = None, # "//tcmalloc:tcmalloc_or_debug",
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
    include_build_test: flag indicating if build test should be produced.
    verbose: internal numeric level of verbosity for the build rule.
    **kwargs: other common attributes for binary targets.

  For more information on the common rule attributes refer to:
  http://bazel.io/docs/build-encyclopedia.html#common-attributes
  """
  # Macro: calling _lisp_binary, cc_binary, _combine_lisp_test/binary
  core = "%s.core.target" % name
  core_dynamic_list_lds = "%s.dynamic-list.lds" % name
  core_extern_symbols = "%s.extern.S" % name
  core_lisp_symbols = "%s.lisp.S" % name

  _lisp_binary(
      # Common lisp attributes.
      name = core,
      binary_name = name,
      srcs = srcs,
      deps = deps,
      order = order,
      data = data,
      compile_data = compile_data,
      lisp_features = features,
      nowarn = nowarn,
      image = image,
      # Binary core attributes.
      main = main,
      precompile_generics = precompile_generics,
      compressed = compressed,
      save_runtime_options = save_runtime_options,
      # Common rule attributes.
      visibility = ["//visibility:private"],
      testonly = testonly,
      verbose = verbose,
      **kwargs)

  _dump_lisp_deps(
      name = "~" + name + ".deps",
      library_name = name,
      srcs = srcs,
      deps = deps,
      lisp_features = features,
      image = image,
      visibility = ["//visibility:private"])

  # Precompile all C sources in advance, before core symbols are present.
  cdeps_library = make_cdeps_library(
      name = name,
      deps = [image] + deps,
      csrcs = csrcs,
      cdeps = cdeps,
      copts = copts,
      visibility = visibility,
      testonly = testonly)

  # Link the C++ runtime.
  runtime = "%s.rt" % name
  deps_rt = ([core_dynamic_list_lds,
              SBCL_PACKAGE + "libsbcl",
              SBCL_PACKAGE + "libsbcl-exported-symbols.lds",
              cdeps_library])
  native.cc_binary(
      name = runtime,
      linkopts = [
          # SBCL cannot generate position-independent code, and -pie
          # is becoming the default. (NOTE: until the SBCL-compiled
          # functions are actually built as an ELF library,
          # theoretically we could build with -pie by modifying the
          # build for libsbcl.a, but that'd be basically a lie since
          # most of the code would still be mapped at a fixed
          # address.)
          # "-Wl,-no-pie",
          # Ensure that symbols needed by lisp code (which grabs them
          # via dlsym at runtime) are exported in the dynamic symbol
          # table.
      ] + (
        ["-Wl,-pagezero_size,0x100000",
         "-dynamic", "-twolevel_namespace", "-bind_at_load"
        ] if config_os == "darwin" else
        ["-Wl,--export-dynamic",
         "-Wl,--dynamic-list", core_dynamic_list_lds
        ] if config_os == "linux" else
        []),
      srcs = [core_extern_symbols, core_lisp_symbols] if config_os == "linux" else [],
      deps = deps_rt,
      visibility = ["//visibility:private"],
      stamp = stamp,
      malloc = malloc,
      testonly = testonly)

  if test:
    _combine_lisp_test(
        name = name,
        runtime = runtime,
        core = core,
        data = data,
        size = size,
        timeout = timeout,
        flaky = flaky,
        args = args,
        visibility = visibility,
        testonly = testonly,
        tags = tags)
  else:
    _combine_lisp_binary(
        name = name,
        runtime = runtime,
        core = core,
        data = data,
        args = args,
        visibility = visibility,
        testonly = testonly)

def lisp_test(name, image=BAZEL_LISP, stamp=0, **kwargs):
  """Bazel rule to create a unit test from Common Lisp source files.

  Outputs: <name>

  The lisp_test rule is an alias for the lisp_binary rule.
  It takes nearly the same set of arguments as lisp_binary rule,
  yet fixes 'testonly' to 1 and 'test' to True.
  The 'stamp' argument is set to 0 by default.

  Args:
    name: the rule name. Also name of the test executable to create.
    image: the base image used to compile the target.
    stamp: a flag indicating whether the binaries should be stamped.
    **kwargs: other keyword arguments that are passed to lisp_binary.

  For more information on the rule attributes refer lisp_binary and
  http://bazel.io/docs/build-encyclopedia.html#common-attributes
  """
  # Macro: an alias for lisp_binary.
  lisp_binary(name, image=image, stamp=stamp,
              testonly=1, test=True, **kwargs)

################################################################################
# Lisp Library
################################################################################

def lisp_library_implementation(ctx,
                                srcs = None,
                                transitive = None,
                                output_fasl = None,
                                image = None,
                                order = None,
                                nowarn = None):
  """Lisp specific implementation for lisp_library rules."""
  # Implementation: _lisp_library.
  verbose_level = max(getattr(ctx.attr, "verbose", 0),
                      int(ctx.var.get("VERBOSE_LISP_BUILD", "0")))
  verbosep = verbose_level > 0
  if verbosep:
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    print("Library: %s" % ctx.label.name)

  trans = transitive or extend_lisp_provider(
      transitive_deps(ctx.attr.deps, image = ctx.attr.image),
      # Add those features to trans already.
      features = ctx.attr.lisp_features,
      data = ctx.files.data,
      compile_data = ctx.files.compile_data)

  flags = _default_flags(ctx, trans, verbose_level)
  srcs = srcs or ctx.files.srcs
  output_fasl = output_fasl or ctx.outputs.fasl
  if not srcs:
    # Need to create the declared output.
    # But it will not be recorded in the provider.
    _concat_files(ctx, [], output_fasl)
    return struct(lisp = trans)

  nowarn = nowarn or getattr(ctx.attr, "nowarn", [])
  compile = _compile_srcs(
      ctx = ctx,
      srcs = srcs,
      deps = trans.srcs,
      image = image or ctx.attr.image,
      order = order or ctx.attr.order,
      compile_data = trans.compile_data,
      flags = flags,
      nowarn = nowarn,
      verbosep = verbosep)

  # Need to concatenate the FASL files into name.fasl.
  _concat_files(ctx, compile.fasls, output_fasl)
  # This is a library, return a struct.
  return struct(
      lisp = extend_lisp_provider(
          trans,
          deps = [output_fasl],
          srcs = srcs,
          hashes = compile.hashes,
          warnings = compile.warnings))


# Internal rule that creates a Lisp library FASL file.
# Keep the _lisp_library rule class name, so Grok can find the targets.
_lisp_library = rule(
    implementation = lisp_library_implementation,
    attrs = _lisp_common_attrs,
    # Access to the cpp compiler options.
    fragments = ["cpp"],
    outputs = {"fasl": "%{name}.fasl"},
    executable = False,
    output_to_genfiles = True)

def _label(label):
  """Create a cannonical form of the 'label' that includes the colon."""
  if ":" in label: return label
  pos = (label.rfind("/") or -1) + 1
  return label + ":" + label[pos:]

def _make_cdeps_dependencies(deps):
  """Transform the 'deps' labels into cdeps dependency labels."""
  return [_label(d) + ".cdeps" for d in deps]

def make_cdeps_library(name,
                       deps = [],
                       csrcs = [],
                       cdeps = [],
                       copts = [],
                       visibility = None,
                       testonly = 0):
  """Create a CDEPS library and a .SO binary for the Lisp library with 'name'.

  Args:
    name: the name of the Lisp library.
    deps: other Lisp library names used to derive transitive dependencies.
    csrcs: the C++ sources for the library.
    cdeps: the C++ dependencies.
    copts: options passed to the C++ compiler.
    visibility: the visibility of the C++ library.
    testonly: if 1, the targets are marked as needed for tests only.
  Returns:
    The name of the C++ library: <name>.cdeps
  """
  # Macro: calling cc_library (and cc_binary).
  # Called from lisp_library and lisp_binary.
  cdeps_library = "%s.cdeps" % name
  cdeps += _make_cdeps_dependencies(deps)
  native.cc_library(
      name = cdeps_library,
      srcs = csrcs,
      deps = cdeps,
      copts = copts,
      visibility = visibility,
      testonly = testonly)
  native.cc_binary(
      name = "lib%s.so" %  name,
      deps = [cdeps_library],
      linkshared = 1,
      linkstatic = 0,
      visibility = visibility,
      testonly = testonly)
  return cdeps_library

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
  # This macro calls _make_cdeps_library, _lisp_library.

  _lisp_library(
      # Common lisp attributes.
      name = name,
      srcs = srcs,
      deps = deps,
      order = order,
      data = data,
      compile_data = compile_data,
      lisp_features = features,
      nowarn = nowarn,
      image = image,
      # Common rule attributes.
      visibility = visibility,
      testonly = testonly,
      verbose = verbose,
      **kwargs)

  make_cdeps_library(
      name = name, deps = [image] + deps,
      csrcs = csrcs, cdeps = cdeps, copts = copts,
      visibility = visibility, testonly = testonly)

  _dump_lisp_deps(
      name = "~" + name + ".deps",
      library_name = name,
      srcs = srcs,
      deps = deps,
      lisp_features = features,
      image = image,
      visibility = ["//visibility:private"])
