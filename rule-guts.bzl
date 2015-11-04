# -*- mode: Python; -*-

"""Implementation of the Lisp build rules.

These rules are used by projects that contain Lisp sources.
They are required to build Lisp binaries or libraries, or run Lisp tests.

The three rules implemented here are:
 lisp_binary_guts - for Lisp binaries,
 lisp_library_guts - for Lisp libraries,
 lisp_test_guts - a test binary run with blaze/bazel test.


The code here defines a few Skylark "real" rules and wraps them in
"Skylark macro" functions. The few real rules have following names:
 _lisp_binary
 _lisp_library
 _combine_lisp_binary
 _combine_lisp_test
These "rule class names" are used to find Lisp targets.
This is useful with 'bazel query' and for tools indexing the Lisp codebase.
"""

# TODO(czak): This needs to have a proper path.
BAZEL_LISP = "//:bazel"

BAZEL_LISP_MAIN = "bazel.main::main"

# TODO(czak): Provide an appropriate path to k8 here.
SBCL_DISTRIBUTION = "//third_party/lisp/sbcl/k8"

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
        cfg = DATA_CFG),
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
        # TODO(czak): This fails TAP.
        # cfg = HOST_CFG,
        default=Label(BAZEL_LISP)),
    "verbose": attr.int(),
    # Internal, for testing coverage.
    "enable_coverage" : attr.bool(),
    "_empty": attr.label(
        allow_files=True,
        single_file=True,
        # TODO(czak): Provide appropriate path here.
        default=Label("//:empty.fasl"))
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

def _max(a, *rest):
  """The maximum of the first number 'a' and the 'rest' numbers.

  Args:
   a: the first number.
   *rest: other numbers.
  Returns:
   The highest number.
  """
  for b in rest:
    if b > a: a = b
  return a

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
    msg = "Linking %s (as empty FASL)" % output.short_path
  elif count == 1:
    cmd = "mv %s %s" % (inputs[0].path, output.path)
    msg = "Linking %s (from 1 source)" % output.short_path
  else:
    cmd = "cat %s > %s" % (_paths(inputs), output.path)
    msg = "Linking %s (from %d sources)" % (output.short_path, count)

  ctx.action(
      inputs = inputs,
      outputs = [output],
      progress_message = msg,
      mnemonic = "LispConcatFASLs",
      command = cmd)

def _compile_srcs(ctx, srcs, deps, compile_data, flags, nowarn, verbosep):
  """Compiles the 'srcs' in the context 'ctx'.

  Args:
   ctx: the context used to instantiate the compile actions.
   srcs: the Lisp file sources.
   deps: are the dependencies for the compilation.
   compile_data: is additional data used to compile the sources.
   flags: are the flags to be passed to Lisp compilation image.
   nowarn: is the list of suppressed Lisp warning types or warning handlers.
   verbosep: is a flag that if True, prints some verbose warnings.
  Returns:
   A structure with FASLs, hash files, and warning files.
  """
  order = ctx.attr.order
  # TODO(czak): Should be "values" list in the attribute.
  if order not in ["multipass", "serial", "parallel"]:
    fail("Accepted values: multipass, serial, parallel", "order")

  build_image = ctx.file.image
  compile_image = build_image
  if hasattr(ctx.attr.image, "lisp"):
    # The image already provides the srcs.
    # TODO(czak): Fix bugs in Skylark SET operations.
    deps = set([d for d in deps if d not in set(ctx.attr.image.lisp.srcs)])
  env = { "LISP_MAIN": BAZEL_LISP_MAIN }

  serial = False
  multipass = False
  load = []
  if order == "multipass":
    load += srcs
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
    if load:   srcs_flags += ["--load", _paths(load)]
    if nowarn: srcs_flags += ["--nowarn", " ".join(nowarn)]

    inputs = list(set() + [build_image] + compile_data + deps + load)
    msg = "Preparing %s (from %d deps" % (compile_image.short_path, len(deps))
    if load:
      msg += " and %d srcs)" % len(load)
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

  fasls = []
  warnings = []
  hashes = []
  for src in srcs:
    stem = _lisp_file_stem(src.short_path)
    if verbosep: print("SRC: %s........................" % src.short_path)

    file_flags = flags
    outs = [ctx.new_file(stem + e) for e in ["~.fasl", "~.hash", "~.warnings"]]
    fasls    += [outs[0]]
    hashes   += [outs[1]]
    warnings += [outs[2]]
    file_flags += ["--outs", _paths(outs)]
    file_flags += ["--srcs", src.path]

    if deps:   file_flags += ["--deps", _paths(deps)]
    if load:   file_flags += ["--load", _paths(load)]
    if nowarn: file_flags += ["--nowarn", " ".join(nowarn)]

    inputs = list(set([compile_image, src]) + compile_data + deps + load)
    if verbosep:
      print("Outputs: %s" % _short_paths(outs))
      print("Inputs: %d" % len(inputs))
      print("Data: %s" % _paths(compile_data))
      print("Exe: %s" % compile_image.short_path)

    ctx.action(
        outputs = outs,
        inputs = inputs,
        progress_message = "Compiling %s" % src.short_path,
        mnemonic = "LispCompile",
        env = env,
        arguments = ["compile"] + file_flags,
        executable = compile_image)

    if serial: load += [src]

  return struct(
      fasls = fasls,
      hashes = hashes,
      warnings = warnings)

def compute_transitive_info(image, deps, features=[], data=[], compile_data=[]):
  """Returns a provider structure containing transitive dependencies."""
  # Figure out the transitive properties.
  trans_deps = set()
  trans_srcs = set()
  trans_hashes = set()
  trans_warnings = set()
  trans_features  = set(features)
  trans_runtime_data = set(data)
  trans_compile_data = set(compile_data)
  # Add the transitive dependencies from the image.
  # Image's DEPS and SRCS need to be removed before compilation.
  if hasattr(image, "lisp"):
    trans_deps += image.lisp.deps
    trans_srcs += image.lisp.srcs
    trans_hashes += image.lisp.hashes
    trans_warnings += image.lisp.warnings
    trans_features += image.lisp.features
    trans_runtime_data += image.lisp.runtime_data
    trans_compile_data += image.lisp.compile_data

  for dep in deps:
    trans_deps += dep.lisp.deps
    trans_deps += dep.files
    trans_srcs += dep.lisp.srcs
    trans_hashes += dep.lisp.hashes
    trans_warnings += dep.lisp.warnings
    trans_features += dep.lisp.features
    trans_runtime_data += dep.lisp.runtime_data
    trans_compile_data += dep.lisp.compile_data

  return struct(
      deps = trans_deps,
      srcs = trans_srcs,
      hashes = trans_hashes,
      warnings = trans_warnings,
      features = trans_features,
      runtime_data = trans_runtime_data,
      compile_data = trans_compile_data)

def _bazel_lisp(ctx):
  """General implementation for Lisp specific rules."""
  # Implementation: _lisp_binary (core), _lisp_library.
  srcs = ctx.files.srcs
  nowarn = ctx.attr.nowarn
  verbose_level = _max(ctx.attr.verbose,
                       int(ctx.var.get("VERBOSE_LISP_BUILD", "0")))
  verbosep = verbose_level > 0

  trans = compute_transitive_info(
      ctx.attr.image,
      ctx.attr.deps,
      ctx.attr.lisp_features,
      ctx.files.data,
      ctx.files.compile_data)

  if verbosep:
    print("Target: %s -------------------------------------" % ctx.label.name)
    print("BIN_DIR: %s" % ctx.configuration.bin_dir.path)
    print("GEN_DIR: %s" % ctx.configuration.genfiles_dir.path)
    print("FEATURES: %s" % " ".join(list(trans.features)))
    # shell_env = ctx.configuration.default_shell_env
    # for k in shell_env: print("%s: %s" % (k, shell_env[k]))
    # for k in ctx.var: print("%s: %s" % (k, ctx.var[k]))

  flags = ["--gendir", ctx.configuration.genfiles_dir.path,
           "-c", ctx.var["COMPILATION_MODE"]]

  if verbosep:       flags += ["--verbose", "%d" % verbose_level]
  if trans.features: flags += ["--features", " ".join(list(trans.features))]
  if (ctx.configuration.coverage_enabled or ctx.attr.enable_coverage):
    flags += ["--coverage"]

  trans_warnings = trans.warnings
  trans_hashes = trans.hashes
  fasls = []
  if srcs:
    result = _compile_srcs(ctx = ctx,
                           srcs = srcs,
                           deps = trans.srcs,
                           compile_data = trans.compile_data,
                           flags = flags,
                           nowarn = nowarn,
                           verbosep = verbosep)
    fasls = result.fasls
    trans_warnings += result.warnings
    trans_hashes += result.hashes

  # After the Lisp sources have been compiled finalize the main target.
  if hasattr(ctx.outputs, "core"):
    if verbosep: print("Executable Core: %s" % ctx.attr.binary_name)
    # TODO(czak): Add --hashes, and --warnings flags to bazl.main.
    # TOOD(czak): Fix: set([1, 2, 3]) + set([2, 4])
    _lisp_binary_core(ctx    = ctx,
                      srcs   = set(fasls + list(trans_warnings)),
                      deps   = trans.deps + trans_hashes,
                      data   = trans.compile_data,
                      flags  = flags,
                      nowarn = nowarn)
    return struct(
        image = ctx.outputs.core,
        runtime_data = trans.runtime_data,
        # The image also provides a lisp environment.
        lisp = struct(
            # TODO(czak): Need to provide the srcs as lisp_library.
            # This way it can be loaded, if this image is used to compile Lisp.
            deps = trans.deps,
            srcs = set(list(trans.srcs) + srcs),
            hashes = trans_hashes,
            warnings = trans_warnings,
            features = trans.features,
            runtime_data = trans.runtime_data,
            compile_data = trans.compile_data))
  else:
    if verbosep: print("Library: %s.fasl" % ctx.label.name)
    # This is a library, return a struct.
    # Need to concatenate the FASL files into name.fasl.
    _concat_files(ctx, fasls or ctx.files._empty, ctx.outputs.fasl)

    trans_deps = set(list(trans.deps) + [ctx.outputs.fasl])
    trans_srcs = set(list(trans.srcs) + srcs)

    if verbosep:
      print("DEPS: %d" % len(trans.deps))
      print("SRCS: %d" % len(trans.srcs))
      print("WARN: %d" % len(trans.warnings))
      print("HASH: %d" % len(trans.hashes))

    return struct(lisp =
                  struct(
                      deps = trans_deps,
                      srcs = trans_srcs,
                      hashes = trans_hashes,
                      warnings = trans_warnings,
                      features = trans.features,
                      runtime_data = trans.runtime_data,
                      compile_data = trans.compile_data))

# Internal rule used to generate action that creates a Lisp binary core.
# Keep the name to be _lisp_binary - Grok depends on this name to find targets.
_lisp_binary = rule(
    implementation = _bazel_lisp,
    output_to_genfiles = True,
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
            default=Label("//:dump-symtable.lisp"))},
    outputs = {"core": "%{binary_name}.core",
               "dynamic_list_lds": "%{binary_name}.dynamic-list.lds",
               "extern_symbols": "%{binary_name}.extern.S",
               "lisp_symbols": "%{binary_name}.lisp.S"})

def _lisp_binary_core(ctx, srcs, deps, data, flags, nowarn):
  """Create a lisp binary core image from 'srcs' and 'deps'.

  Args:
   ctx: is the rule context.
   srcs: contains FASL files and warning files.
   deps: contains the deps libraries.
   data: is additional data needed to create the core.
   flags: are flags passed to the Lisp compilation image.
   nowarn: is a list of suppressed Lisp warning types or warning handlers.
  """
  # Implementation: _lisp_binary (core).
  #
  # Create a Lisp core containing the Lisp compiled and linked FASLs.
  build_image = ctx.file.image
  dump_symtable = ctx.file._dump_symtable
  deps = set(deps)
  if hasattr(ctx.attr.image, "lisp"):
    # The image already provides the deps.
    # TODO(czak): SETs operation do not necessary respect order.
    deps = set([d for d in deps if d not in set(ctx.attr.image.lisp.deps)])
  inputs = list(set([build_image, dump_symtable]) + deps + srcs + data)

  core = ctx.outputs.core
  dynamic_list_lds = ctx.outputs.dynamic_list_lds
  extern_symbols = ctx.outputs.extern_symbols
  lisp_symbols = ctx.outputs.lisp_symbols
  outs = [core, dynamic_list_lds, extern_symbols, lisp_symbols]

  flags += ["--deps", _paths(deps)]
  flags += ["--srcs", _paths(srcs)]
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

# Attributes used by _combine_lisp_* rules.
_combine_lisp_binary_attrs = {
    "data": attr.label_list(cfg=DATA_CFG, allow_files=True),
    "runtime": attr.label(allow_files = True, single_file = True),
    "core": attr.label(providers=["image", "runtime_data"]),
    "_combine": attr.label(
        executable=True,
        allow_files=True,
        single_file=True,
        # TODO(czak): Need to provide a proper path.
        default = Label("//:bazel"),
        cfg = HOST_CFG)}

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

  return struct(runfiles = ctx.runfiles(
      files = list(set(ctx.files.data) + ctx.attr.core.runtime_data)))

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
  features = set()
  srcs = set()
  for dep in ctx.attr.deps:
    features += dep.lisp.features
    srcs += dep.lisp.srcs
  features += ctx.attr.features
  srcs += ctx.files.srcs
  ctx.file_action(
      output = ctx.outputs.deps,
      content = (
          "\n".join(["feature: " + f for f in features] +
                    ["src: " + f.path for f in srcs])))

# Internal rule that creates a Lisp library DEPS file.
# DEPS file is used to list all the Lisp sources for a target.
# It is a quick hack to make (bazel:load ...) work.
_dump_lisp_deps = rule(
    implementation = _dump_lisp_deps_impl,
    attrs = {
        "library_name": attr.string(),
        "srcs": attr.label_list(allow_files = lisp_files),
        "deps": attr.label_list(providers = ["lisp"]),
        },
    outputs = {"deps": "%{library_name}.deps"},
    executable = False,
    output_to_genfiles = True)

def lisp_binary_guts(name,
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
                     malloc = "//tcmalloc:tcmalloc_or_debug",
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
      features = features,
      visibility = ["//visibility:private"])

  # Precompile all C sources in advance, before core symbols are present.
  cdeps_library = _make_cdeps_library(
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
              SBCL_DISTRIBUTION + ":libsbcl",
              SBCL_DISTRIBUTION + ":libsbcl-exported-symbols.lds",
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
          "-Wl,-no-pie",
          # Ensure that symbols needed by lisp code (which grabs them
          # via dlsym at runtime) are exported in the dynamic symbol
          # table.
          "-Wl,--dynamic-list", core_dynamic_list_lds],
      srcs = [core_extern_symbols, core_lisp_symbols],
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

def lisp_test_guts(name, image=BAZEL_LISP, stamp=0, **kwargs):
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
  lisp_binary_guts(name, image=image, stamp=stamp, testonly=1, test=True, **kwargs)

# Internal rule that creates a Lisp library FASL file.
# Keep the _lisp_library rule class name, so Grok can find the targets.
_lisp_library = rule(
    implementation = _bazel_lisp,
    attrs = _lisp_common_attrs,
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

def _make_cdeps_library(name,
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

def lisp_library_guts(name,
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
  # Macro: calls _make_cdeps_library, _lisp_library.
  _make_cdeps_library(name = name, deps = [image] + deps,
                      csrcs = csrcs, cdeps = cdeps, copts = copts,
                      visibility = visibility, testonly = testonly)

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
  _dump_lisp_deps(
      name = "~" + name + ".deps",
      library_name = name,
      srcs = srcs,
      deps = deps,
      features = features,
      visibility = ["//visibility:private"])
