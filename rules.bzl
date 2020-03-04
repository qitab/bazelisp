"""Implementation of the Lisp build rules.

These rules are used by projects that contain Lisp sources.
They are required to build Lisp binaries or libraries, or run Lisp tests.

The three rules implemented here are:
 lisp_binary - for Lisp binaries,
 lisp_library - for Lisp libraries,
 lisp_test - a test binary run with blaze/bazel test.


The code here defines a few Skylark "real" rules and wraps them in
"Skylark macro" functions. The few real rules have following names:
 _lisp_library
 _lisp_core
 _skylark_wrap_lisp_test
 _skylark_wrap_lisp_binary
These "rule class names" are used to find Lisp targets.
This is useful with 'bazel query' and for tools indexing the Lisp codebase.
"""

load(
    "//:provider.bzl",
    "LispInfo",
    "collect_lisp_info",
    "extend_lisp_info",
)
load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain")
load(
    "@rules_cc//cc:action_names.bzl",
    "CPP_COMPILE_ACTION_NAME",
    "C_COMPILE_ACTION_NAME",
)

UNSUPPORTED_FEATURES = [
    "thin_lto",
    "module_maps",
    "use_header_modules",
    "fdo_instrument",
    "fdo_optimize",
]

BAZEL_LISP = "//"
BAZEL_LISP_MAIN = "bazel.main:main"
BAZEL_LISP_ENV = {"LISP_MAIN": BAZEL_LISP_MAIN}

lisp_files = [".lisp", ".lsp"]

# Common attributes accepted by the (internal) lisp rules.
_compilation_orders = ["multipass", "serial", "parallel"]
_lisp_common_attrs = [
    ("srcs", attr.label_list(allow_files = lisp_files)),
    ("deps", attr.label_list(providers = [LispInfo])),
    ("order", attr.string(
        default = "serial",
        values = _compilation_orders,
    )),
    # runtime data - is data available at runtime.
    ("data", attr.label_list(allow_files = True)),
    # compile data - is data available at compile and load time.
    ("compile_data", attr.label_list(allow_files = True)),
    ("lisp_features", attr.string_list()),
    ("nowarn", attr.string_list()),
    # TODO(czak): Rename to "build_image".
    ("image", attr.label(
        allow_single_file = True,
        executable = True,
        cfg = "target",
        default = Label(BAZEL_LISP),
    )),
    ("verbose", attr.int()),
    # For testing coverage.
    ("enable_coverage", attr.bool()),
    # For testing compilation behavior.
    ("preload_image", attr.bool()),
    # Do not add references, temporary attribute for find_cc_toolchain.
    # See go/skylark-api-for-cc-toolchain for more details.
    ("_cc_toolchain", attr.label(
        default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
    )),
]

def _paths(files, sep = " "):
    """Return the full file paths for all the 'files'.

    Args:
     files: a list of build file objects.
     sep: String to join on, by default a space.

    Returns:
      Joined string of file paths.
    """
    return sep.join([f.path for f in files])

def _spec(spec, files):
    """Return the compilation/linking specification as parsed by bazel driver.

    The specification will have the following form:
      (:<spec> "<path>" .... "<path>")
    for each spec and path of the files.

    Args:
      spec: the specification name corresponding to the command line argument.
      files: a list of build file objects to be included in the specification.

    Returns:
      A list of parenthesized paths prefixed with specs name as keyword.
    """
    return '(:{}\n "{}")\n'.format(spec, _paths(files, sep = '"\n "'))

def _concat_files(ctx, inputs, output):
    """Concatenates several FASLs into a combined FASL.

    Args:
      ctx: Rule context
      inputs: List of files to concatenate.
      output: File output for the concatenated contents.
    """
    if not inputs:
        cmd = "touch " + output.path
        msg = "Linking {} (as empty FASL)".format(output.short_path)
    elif len(inputs) == 1:
        cmd = "mv {} {}".format(inputs[0].path, output.path)
        msg = "Linking {} (from 1 source)".format(output.short_path)
    else:
        cmd = "cat {} > {}".format(_paths(inputs), output.path)
        msg = "Linking {} (from {} sources)".format(output.short_path, len(inputs))

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = [output],
        progress_message = msg,
        mnemonic = "LispConcatFASLs",
        command = cmd,
    )

def _build_flags(ctx, lisp_features, verbose_level, force_coverage_instrumentation):
    """Returns Args for flags for all Lisp build actions.

    Args:
     ctx: The rule context.
     lisp_features: Depset of transitive Lisp feature strings provided by this
         target and its dependencies.
     verbose_level: int indicating level of debugging output. If positive, a
         --verbose flags is added.
     force_coverage_instrumentation: Whether to unconditionally add code
         coverage instrumentation.

    Returns:
        Args object to be passed to Lisp build actions.
    """
    cpp_fragment = ctx.fragments.cpp
    copts = cpp_fragment.copts
    conlyopts = cpp_fragment.conlyopts
    cxxopts = cpp_fragment.cxxopts
    cc_toolchain = find_cc_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features + UNSUPPORTED_FEATURES,
    )
    c_variables = cc_common.create_compile_variables(
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        user_compile_flags = copts + conlyopts,
    )
    cpp_variables = cc_common.create_compile_variables(
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        user_compile_flags = copts + cxxopts,
        add_legacy_cxx_options = True,
    )
    c_options = cc_common.get_memory_inefficient_command_line(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
        variables = c_variables,
    )
    cpp_options = cc_common.get_memory_inefficient_command_line(
        feature_configuration = feature_configuration,
        action_name = CPP_COMPILE_ACTION_NAME,
        variables = cpp_variables,
    )

    if "-fsanitize=memory" in c_options or "-fsanitize=memory" in cpp_options:
        lisp_features = depset(["msan"], transitive = [lisp_features])
    flags = ctx.actions.args()
    flags.add(
        "--compilation-mode",
        ctx.var.get("LISP_COMPILATION_MODE", ctx.var["COMPILATION_MODE"]),
    )
    flags.add("--bindir", ctx.bin_dir.path)
    flags.add_joined("--features", lisp_features, join_with = " ")

    if ctx.coverage_instrumented() or force_coverage_instrumentation:
        flags.add("--coverage")

    if verbose_level > 0:
        flags.add("--verbose", str(verbose_level))

    # TODO(czak): Find out how to simplify passing NDEBUG here.
    if "-UNDEBUG" in cpp_options:
        flags.add("--safety", "3")
    elif "-DNDEBUG" in cpp_options:
        flags.add("--safety", "0")

    if int(ctx.var.get("LISP_BUILD_FORCE", "0")) > 0:
        flags.add("--force")

    return flags

def _list_excluding_depset(items, exclude):
    exclude_set = {item: True for item in exclude.to_list()}
    return [item for item in items if item not in exclude_set]

def lisp_compile_srcs(
        ctx,
        srcs,
        deps,
        image,
        lisp_features,
        nowarn,
        order,
        compile_data,
        verbose_level,
        force_coverage_instrumentation = False,
        preload_image = None):
    """Generate LispCompile actions, return LispInfo and FASL output.

    Args:
      ctx: The rule context.
      srcs: List of src Files.
      deps: List of immediate dependency Targets.
      image: Build image Target used to compile the sources.
      lisp_features: List of Lisp feature strings added by this target.
      nowarn: List of supressed warning type strings.
      order: Order in which to load sources, either "serial", "parallel", or
          "multipass".
      compile_data: depset of additional data Files used for compilation.
      verbose_level: int indicating level of debugging output.
      force_coverage_instrumentation: Whether to unconditionally add code
         coverage instrumentation.
      preload_image: Whether to preload all deps into a single image to use for
         compilation. If None, do this heurisitcally when there are multiple source
         files and many deps.

    Returns:
      struct with fields:
          - lisp_info: LispInfo for the target
          - output_fasl: Combined FASL for this target (which is also included in
              lisp_info.fasls if there are srcs)
          - flags: List of args to pass to all compile/binary actions
    """
    if not order in _compilation_orders:
        fail("order {} must be one of {}".format(order, _compilation_orders))

    name = ctx.label.name
    verbosep = verbose_level > 0
    lisp_info = collect_lisp_info(
        deps = deps,
        build_image = image,
        features = lisp_features,
        compile_data = compile_data,
    )
    output_fasl = ctx.actions.declare_file(name + ".fasl")
    build_flags = _build_flags(
        ctx = ctx,
        lisp_features = lisp_info.features,
        verbose_level = verbose_level,
        force_coverage_instrumentation = force_coverage_instrumentation,
    )

    if not srcs:
        _concat_files(ctx, [], output_fasl)
        return struct(
            lisp_info = lisp_info,
            output_fasl = output_fasl,
            build_flags = build_flags,
        )

    multipass = (order == "multipass")
    serial = (order == "serial")

    build_image = image[DefaultInfo].files_to_run
    compile_image = build_image

    # Lisp source files for all the transitive dependencies not already in the
    # image, loaded before compilation, passed to --deps.
    deps_srcs = lisp_info.srcs.to_list()
    if LispInfo in image:
        deps_srcs = _list_excluding_depset(deps_srcs, image[LispInfo].srcs)

    # Sources for this target loaded before compilation (after deps), passed to
    # --load. What this contains depends on the compilation order:
    # multipass: Contains everything
    # parallel: Contains nothing
    # serial: Contains previous entries in srcs (accumulated below)
    load_srcs = srcs if multipass else []

    # Arbitrary heuristic to reduce load on the build system by bundling
    # FASL and source files load into one compile-image binary.
    compile_flags = ctx.actions.args()
    if preload_image == None:
        preload_image = ((len(srcs) - 1) * len(deps_srcs) > 100)
    if preload_image:
        # Generate a SRCS image.
        compile_image = ctx.actions.declare_file(name + ".srcs.image")
        preload_image_flags = ctx.actions.args()
        preload_image_flags.add("--outs", compile_image)
        preload_image_flags.add_joined("--deps", deps_srcs, join_with = " ")
        preload_image_flags.add_joined("--load", load_srcs, join_with = " ")
        preload_image_flags.add_joined("--nowarn", nowarn, join_with = " ")

        msg = "Preparing {} (from {} deps{})".format(
            compile_image.short_path,
            len(deps),
            " and {} srcs".format(len(load_srcs)) if load_srcs else "",
        )
        ctx.actions.run(
            outputs = [compile_image],
            inputs = depset(
                load_srcs + deps_srcs,
                transitive = [lisp_info.compile_data],
            ),
            progress_message = msg,
            mnemonic = "LispSourceImage",
            env = BAZEL_LISP_ENV,
            arguments = ["binary", build_flags, preload_image_flags],
            executable = build_image,
        )

        # All deps included above. However, we need to keep --deps the same in
        # the command line below for the sake of analysis that uses extra
        # actions to examine the command-line of LispCompile actions.
        compile_flags.add("--deps-already-loaded")

    if multipass:
        nowarn = nowarn + ["redefined-method", "redefined-function"]

    # buildozer: disable=print
    if verbosep:
        print("Target: " + name)
        print("Build Img: " + build_image.short_path)
        print("Compile Img: " + compile_image.short_path)

    fasls = []
    warnings = []
    hashes = []
    for src in srcs:
        stem = src.short_path[:-len(src.extension) - 1]
        fasls.append(ctx.actions.declare_file(stem + "~.fasl"))
        hashes.append(ctx.actions.declare_file(stem + "~.hash"))
        warnings.append(ctx.actions.declare_file(stem + "~.warnings"))
        outs = [fasls[-1], hashes[-1], warnings[-1]]
        file_flags = ctx.actions.args()
        file_flags.add_joined("--outs", outs, join_with = " ")
        file_flags.add("--srcs", src)
        file_flags.add_joined("--deps", deps_srcs, join_with = " ")
        file_flags.add_joined("--load", load_srcs, join_with = " ")
        file_flags.add_joined("--nowarn", nowarn, join_with = " ")

        inputs = [src]
        inputs.extend(deps_srcs)
        inputs.extend(load_srcs)
        inputs = depset(inputs, transitive = [lisp_info.compile_data])
        ctx.actions.run(
            outputs = outs,
            inputs = inputs,
            tools = [compile_image],
            progress_message = "Compiling " + src.short_path,
            mnemonic = "LispCompile",
            env = BAZEL_LISP_ENV,
            arguments = ["compile", build_flags, compile_flags, file_flags],
            executable = compile_image,
        )
        if serial:
            load_srcs.append(src)

    # Need to concatenate the FASL files into name.fasl.
    _concat_files(ctx, fasls, output_fasl)
    lisp_info = extend_lisp_info(
        lisp_info,
        srcs = srcs,
        fasls = [output_fasl] if srcs else [],
        hashes = hashes,
        warnings = warnings,
    )
    return struct(
        lisp_info = lisp_info,
        output_fasl = output_fasl,
        build_flags = build_flags,
    )

################################################################################
# Lisp Binary and Lisp Test
################################################################################

def _lisp_core_impl(ctx):
    """Lisp specific implementation for lisp_binary and lisp_test rules."""
    core = ctx.actions.declare_file(ctx.label.name)
    verbose_level = max(
        ctx.attr.verbose,
        int(ctx.var.get("VERBOSE_LISP_BUILD", "0")),
    )
    verbosep = verbose_level > 0

    # buildozer: disable=print
    if verbosep:
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        print("Executable Core: %s" % core)

    compile = lisp_compile_srcs(
        ctx = ctx,
        srcs = ctx.files.srcs,
        deps = ctx.attr.deps,
        image = ctx.attr.image,
        lisp_features = ctx.attr.lisp_features,
        nowarn = ctx.attr.nowarn,
        order = ctx.attr.order,
        compile_data = ctx.files.compile_data,
        verbose_level = verbose_level,
        force_coverage_instrumentation = ctx.attr.enable_coverage,
        preload_image = ctx.attr.preload_image,
    )

    # TODO(czak): Add --hashes, and --warnings flags to bazl.main.
    lisp_info = compile.lisp_info

    fasls = lisp_info.fasls.to_list()
    hashes = lisp_info.hashes.to_list()
    warnings = lisp_info.warnings.to_list()

    if LispInfo in ctx.attr.image:
        # The image already includes some deps.
        included = ctx.attr.image[LispInfo]
        fasls = _list_excluding_depset(fasls, included.fasls)
        hashes = _list_excluding_depset(hashes, included.hashes)
        warnings = _list_excluding_depset(warnings, included.warnings)

    build_image = ctx.file.image

    # buildozer: disable=print
    if verbosep:
        print("Build image: %s" % build_image.short_path)

    specs = ctx.actions.declare_file(ctx.label.name + ".specs")
    ctx.actions.write(
        output = specs,
        content = (
            "".join([
                _spec("deps", fasls),
                _spec("warnings", warnings),
                _spec("hashes", hashes),
            ])
        ),
    )

    inputs = [specs]
    inputs.extend(fasls)
    inputs.extend(hashes)
    inputs.extend(warnings)
    inputs = depset(inputs, transitive = [lisp_info.compile_data])

    core_flags = ctx.actions.args()
    core_flags.add("--specs", specs)
    core_flags.add("--outs", core)
    core_flags.add("--main", ctx.attr.main)
    core_flags.add_joined("--nowarn", ctx.attr.nowarn, join_with = " ")
    if ctx.attr.precompile_generics:
        core_flags.add("--precompile-generics")
    if ctx.attr.save_runtime_options:
        core_flags.add("--save-runtime-options")
    outs = [core]
    ctx.actions.run(
        outputs = outs,
        inputs = inputs,
        progress_message = "Building lisp core " + core.short_path,
        mnemonic = "LispCore",
        env = BAZEL_LISP_ENV,
        arguments = ["core", compile.build_flags, core_flags],
        executable = build_image,
    )

    runfiles = ctx.runfiles(collect_default = True)
    if ctx.attr.image:
        runfiles = runfiles.merge(ctx.attr.image[DefaultInfo].default_runfiles)
    for compile_data in ctx.attr.compile_data:
        runfiles = runfiles.merge(compile_data[DefaultInfo].default_runfiles)
    return [
        lisp_info,
        DefaultInfo(
            runfiles = runfiles,
            files = depset([core]),
        ),
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["deps", "image"],
        ),
    ]

# Internal rule used to generate action that creates a Lisp binary core.
# Keep the name to be _lisp_core - Grok depends on this name to find targets.
_lisp_core = rule(
    implementation = _lisp_core_impl,
    # Access to the cpp compiler options.
    fragments = ["cpp"],
    attrs = dict(_lisp_common_attrs + [
        ("main", attr.string(default = "main")),
        ("precompile_generics", attr.bool()),
        ("save_runtime_options", attr.bool()),
    ]),
)

# Attributes used by _skylark_wrap_lisp_* rules.
_skylark_wrap_lisp_attrs = {
    "binary": attr.label(allow_single_file = True),
    "data": attr.label_list(allow_files = True),
    "core": attr.label(providers = [LispInfo]),
    # TODO(sfreilich): After there's some API for accessing native rule
    # internals in Skylark rules, rewrite lisp_* macros to be rule functions
    # instead and remove these additional attributes used in instrumented_files.
    # (Same for lisp_library below.)
    "instrumented_srcs": attr.label_list(allow_files = True),
    "instrumented_deps": attr.label_list(allow_files = True),
}

def _skylark_wrap_lisp_impl(ctx):
    """A Skylark rule that provides Lisp-related providers for a cc_binary."""
    out = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.run_shell(
        inputs = [ctx.file.binary],
        outputs = [out],
        progress_message = "Copying to " + out.short_path,
        command = "mv {} {}".format(ctx.file.binary.path, out.path),
    )

    # Forward the runfiles. The data attribute is passed here as well, but just
    # so that targets in data can be referenced in attrs of this. In
    # particular, a lisp_test's args attribute might include
    # $(location :something-in-data).
    runfiles = ctx.runfiles(files = [out])
    runfiles = runfiles.merge(ctx.attr.core[DefaultInfo].default_runfiles)
    return [
        ctx.attr.core[LispInfo],
        DefaultInfo(
            runfiles = ctx.attr.core[DefaultInfo].default_runfiles,
            executable = out,
        ),
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["instrumented_srcs"],
            dependency_attributes = ["instrumented_deps"],
        ),
    ]

# Rule used to wrap an internal cc_binary rule to provide Lisp Skylark
# providers for lisp_binary.
_skylark_wrap_lisp_binary = rule(
    implementation = _skylark_wrap_lisp_impl,
    executable = True,
    attrs = _skylark_wrap_lisp_attrs,
)

# Rule used to wrap an internal cc_binary rule to provide Lisp Skylark
# providers for lisp_test.
_skylark_wrap_lisp_test = rule(
    implementation = _skylark_wrap_lisp_impl,
    executable = True,
    test = True,
    attrs = _skylark_wrap_lisp_attrs,
)

# DEPS file is used to list all the Lisp sources for a target.
# It is a quick hack to make (bazel:load ...) work.
def _dump_lisp_deps_impl(ctx):
    """Creates a file that lists all Lisp files needed by the target in order."""
    lisp_info = ctx.attr.target[LispInfo]
    out = ctx.actions.declare_file(ctx.attr.target.label.name + ".deps")
    ctx.actions.write(
        output = out,
        content = (
            "\n".join(["feature: " + f for f in lisp_info.features.to_list()] +
                      ["src: " + f.path for f in lisp_info.srcs.to_list()])
        ),
    )
    return [DefaultInfo(files = depset([out]))]

# Internal rule that creates a Lisp library DEPS file.
# DEPS file is used to list all the Lisp sources for a target.
# It is a quick hack to make (bazel:load ...) work.
_dump_lisp_deps = rule(
    implementation = _dump_lisp_deps_impl,
    attrs = {
        "target": attr.label(mandatory = True, providers = [LispInfo]),
    },
)

def _add_tag(tag, tags):
    if tag not in tags:
        tags.append(tag)

def lisp_binary(
        name,
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
        allow_save_lisp = False,
        visibility = None,
        testonly = 0,
        cdeps = [],
        test = False,
        flaky = False,
        size = "medium",
        timeout = None,
        shard_count = None,
        tags = [],
        stamp = -1,
        malloc = "@bazel_tools//tools/cpp:malloc",
        verbose = None,
        **kwargs):
    """Bazel rule to create a binary executable from Common Lisp source files.

    The image file contains necessary SBCL contribs to be called into a
    development environment like emacs/swank for a debug session.

    This generates a binary with the name specified in the name attribute,
    which will run the function specified in the main attribute on startup
    (default cl-user::main). Lisp debugger and interactive functions are
    disabled. This can be overridden using the LISP_MAIN environment variable
    at runtime to specify a different entry-point function. LISP_MAIN=T will
    cause the binary to invoke the interactive top-level REPL with an enabled
    Lisp debugger. Specifying main="nil" has the same effect.

    By default, the binary is transformed into a standard ELF format that's
    compatible with standard C debugging tools. That means you can get
    stacktraces from combined C/C++ and Lisp code from such tools with the Lisp
    function names correctly included. Binaries transformed in this way will
    not behave correctly when save-lisp-and-die is called, so binaries that
    require this functionality (e.g. ones used in "image") must set
    allow_save_lisp to True.

    For more information on the common rule attributes refer to:
    https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes

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
            previous sources loaded (default).
          "multipass" - each source is compiled in an image with
            all sources loaded.
          "parallel" - each source is compiled independently from others.
      nowarn: a list of suppressed Lisp warning types or warning handlers.
      args: default arguments passed to the binary or test on execution.
      main: specifies the entry point function (default: main).
      image: the base image used to compile the target (default BAZEL_LISP).
          The executable generated by this rule needs to run save-lisp-and-die,
          so if it's a lisp_binary target, it must have the attribute
          allow_save_lisp set to True.
      save_runtime_options: save runtime options and prevent those being
          interpreted by the target binary with a main entry point (default True).
          Setting this to False allows SBCL to process following flags:
           --help, --version, --core, --dynamic-space-size, --control-stack-size.
      precompile_generics: precompile generic functions if True (as by default).
        Must be set to False to allow compiling arbitary amounts of code to
        memory, so should be set to false for targets intended for use as a
        compilation image.
      allow_save_lisp: Whether the binary format should be left in the
        configuration expected by save-lisp-and-die (default False). By
        default, this rule transforms the binary into a more standard ELF
        format that is compatible with C debugging tools, to allow easier
        debugging of Lisp code which interoperates with C/C++ code via a C
        foreign function interface and to allow easier profiling/debugging of
        a mix of Lisp and other code with a common toolchain.
      visibility: list of labels controlling which other rules can use this one.
      testonly: If 1, only test targets can use this rule.
      cdeps: this will link the cc dependencies into the image.
      test: indicates that the binary is a test.
      flaky: a flag indicating that a test is flaky.
      size: the size of a test: small, medium (default), large, enormous.
      timeout: test timeout: None (default), short, moderate, long, eternal.
          None indicates that the timeout should be chosen based on test size.
      shard_count: number of shards used for the test.
      tags: list of arbitrary text tags mostly useful for tests. E.g.: "local".
      stamp: C++ build stamp info (0 = no, 1 = yes, default -1 = bazel option).
      malloc: malloc implementation to be used for linking of cc code.
      verbose: internal numeric level of verbosity for the build rule.
      **kwargs: other common attributes for all targets.
    """

    # We have essentially three ways to produce a Lisp binary:
    #
    # (1) Call SAVE-LISP-AND-DIE and use that output (used internally by these
    #     rules). This is a slightly funny binary file in that it has a (large)
    #     opaque blob of bytes at the end comprising the whole Lisp heap. But
    #     that's the garden-variety SBCL executable.
    #
    #     No C code other than SBCL's support is present in the text file,
    #     as the dump step does not know how to write bytes from any source
    #     other than the SBCL executable and the in-memory heap.
    #
    # (2) Partially ELFinated binary (allow_save_lisp=True): After
    #     SAVE-LISP-AND-DIE, slurp the Lisp heap back out of the resulting
    #     file, and turn it into a data section in a proper ELF file, but a
    #     relatively opaque section with the only C symbols acting to demarcate
    #     the bounds of the Lisp spaces. Feed that in to a regular link step
    #     (with C++ libaries and such, and the SBCL main). This binary when
    #     launched will "parse" the Lisp heap out of the data section and begin
    #     life as usual.
    #
    #     The main distinctions between this and the executable generated by
    #     SAVE-LISP-AND-DIE are that:
    #     - the heap is not randomly glued on at the end, but instead a true
    #       section that survives manipulation by various ELF tools.
    #     - C++ code other than SBCL's runtime is directly present in the image
    #     In almost all respects this acts like a garden-variety Lisp binary.
    #
    # (3) Fully ELFinated binary (default): Starting with SAVE-LISP-AND-DIE,
    #     turn the attached Lisp heap into two ELF sections: one comprising
    #     the '.text' and nothing but, and one containing everything else that
    #     did not go in the '.text' section. The latter section is akin to what
    #     goes in one section for partial ELF mode (see below), minus what
    #     became '.text'.
    #
    #     The precise details of how we produce the '.text' section are
    #     unimportant, but simultaneously it is a proper segment of the final
    #     file and a GC-managed space. (This is actually an astonishing feat,
    #     not without drawbacks, namely, that it has to be read/write memory
    #     "because Lisp".) It gets moved into dynamic space in memory when the
    #     binary starts up, so there's not a constraint on binaries in this
    #     format compiling additional codes. However, since the format of the
    #     image in memory differs from what the implementation expects,
    #     save-lisp-and-die will not work. Thus, binaries in this format can't
    #     be used as a build image (for that, set allow_save_lisp=True).
    #
    # In truth, there is one other kind of executable which has no Lisp heap
    # attached, and requires a --core argument, but we seldom if ever use that.
    #
    # It is extremely important to understand another limitation as well: in no
    # scenario does the system linker receive "--export-dynamic" for *all* C
    # symbols, i.e. literally --export-dynamic as opposed to
    # --export-dynamic-symbol=something.
    #
    # This means that you CAN NOT refer to an arbitrary C symbol from Lisp,
    # though we do still link with libdl to look up symbols "because reasons".
    # Any C symbol that the Lisp code has to know about must have its address
    # in a 'reloc' section of the ELF file whence came the Lisp heap.
    # Mention of "one" or "two" sections in the above descriptions refers to
    # the number of sections of Lisp data. In either case, there are other
    # ELF sections which inform the linker how to link Lisp to C code.

    core = name + ".core.target"
    _lisp_core(
        name = core,
        # Common lisp attributes.
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
        save_runtime_options = save_runtime_options,
        # Common rule attributes.
        visibility = ["//visibility:private"],
        testonly = testonly,
        verbose = verbose,
        **kwargs
    )

    # Discard kwargs that are just for _lisp_core.
    kwargs.pop("preload_image", None)
    kwargs.pop("enable_coverage", None)

    # Precompile all C sources in advance, before core symbols are present.
    cdeps_library = make_cdeps_library(
        name = name,
        deps = [image] + deps,
        cdeps = cdeps,
        visibility = visibility,
        testonly = testonly,
        tags = tags,
        **kwargs
    )

    internal_tags = list(tags)
    _add_tag("manual", internal_tags)

    # SBCL cannot generate position-independent code, and -pie is becoming the
    # default. (NOTE: until the SBCL-compiled functions are actually built as
    # an ELF library, theoretically we could build with -pie by modifying the
    # build for libsbcl.a, but that'd be basically a lie since most of the code
    # would still be mapped at a fixed address.)
    linkopts = ["-Wl,-no-pie"]

    # Either way, we need to link with the cdeps and SBCL C++.
    link_deps = [
        cdeps_library,
        "@local_sbcl//:c-support",
    ]

    if allow_save_lisp:
        # Copy entire native SBCL core into a binary blob in a normal '.o' file
        elfinate_outs = [name + "-core.o", name + "-syms.lds"]
        link_srcs = [name + "-core.o"]
        elfinate_cmd_template = (
            "$(location @local_sbcl//:elfinate) copy " +
            "$(location {core}) $(location {name}-core.o) && " +
            "nm -p $(location {name}-core.o) | " +
            "awk '" +
            '{{print $$2";"}}BEGIN{{print "{{"}}END{{print "}};"}}' +
            "' >$(location {name}-syms.lds)"
        )
        linkopts.append("-Wl,--dynamic-list=$(location {}-syms.lds)".format(name))
        link_deps.append(name + "-syms.lds")
    else:
        # Produce a '.s' file holding only compiled Lisp code and a '-core.o'
        # containing the balance of the original Lisp spaces.
        elfinate_outs = [name + ".s", name + ".core", name + "-core.o"]
        link_srcs = [name + ".s", name + "-core.o"]
        elfinate_cmd_template = (
            "$(location @local_sbcl//:elfinate) split " +
            "$(location {core}) $(location {name}.s)"
        )

    native.genrule(
        name = name + "-parts",
        tools = ["@local_sbcl//:elfinate"],
        srcs = [core],
        outs = elfinate_outs,
        cmd = elfinate_cmd_template.format(
            core = core,
            name = name,
        ),
        visibility = ["//visibility:private"],
        testonly = testonly,
        tags = internal_tags,
    )

    # The final executable still needs to be produced by a Skylark rule, so
    # it can get the LispInfo and instrumented_files_info providers
    # correct. That means the internal rule must be cc_binary, only the
    # outermost rule is a test for lisp_test.
    binary = name + "-combined"
    native.cc_binary(
        name = binary,
        linkopts = linkopts,
        srcs = link_srcs,
        deps = link_deps,
        data = data,
        visibility = ["//visibility:private"],
        stamp = stamp,
        malloc = malloc,
        testonly = testonly,
        tags = internal_tags,
    )

    if test:
        skylark_wrap_rule = _skylark_wrap_lisp_test

        # TODO(sfreilich): This should object when test_kwargs are set on a
        # non-test rule.
        test_kwargs = dict(
            size = size,
            timeout = timeout,
            flaky = flaky,
            shard_count = shard_count,
        )
    else:
        skylark_wrap_rule = _skylark_wrap_lisp_binary
        test_kwargs = {}
    skylark_wrap_rule(
        name = name,
        binary = binary,
        instrumented_srcs = srcs,
        instrumented_deps = deps + [image, cdeps_library],
        core = core,
        data = data,
        args = args,
        visibility = visibility,
        testonly = testonly,
        tags = tags,
        **test_kwargs
    )

    _dump_lisp_deps(
        name = name + ".deps",
        target = name,
        visibility = ["//visibility:private"],
        tags = ["manual"],
        testonly = testonly,
    )

def lisp_test(name, image = BAZEL_LISP, stamp = 0, testonly = 1, **kwargs):
    """Bazel rule to create a unit test from Common Lisp source files.

    Outputs: <name>

    The lisp_test rule is an alias for the lisp_binary rule.
    It takes nearly the same set of arguments as lisp_binary rule,
    yet fixes 'testonly' to 1 and 'test' to True.
    The 'stamp' argument is set to 0 by default.

    For more information on the rule attributes refer lisp_binary and
    https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes

    Args:
      name: the rule name. Also name of the test executable to create.
      image: the base image used to compile the target.
      stamp: a flag indicating whether the binaries should be stamped.
      testonly: Whether this target should only be a dependency of testonly
          targets and tests.
      **kwargs: other keyword arguments that are passed to lisp_binary.
    """

    # Macro: an alias for lisp_binary.
    lisp_binary(
        name,
        image = image,
        stamp = stamp,
        test = True,
        testonly = testonly,
        **kwargs
    )

################################################################################
# Lisp Library
################################################################################

def _lisp_library_impl(ctx):
    """Lisp specific implementation for lisp_library rules."""
    verbose_level = max(
        getattr(ctx.attr, "verbose", 0),
        int(ctx.var.get("VERBOSE_LISP_BUILD", "0")),
    )
    verbosep = verbose_level > 0

    # buildozer: disable=print
    if verbosep:
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        print("Library: %s" % ctx.label.name)

    compile = lisp_compile_srcs(
        ctx = ctx,
        srcs = ctx.files.srcs,
        deps = ctx.attr.deps,
        image = ctx.attr.image,
        lisp_features = ctx.attr.lisp_features,
        nowarn = ctx.attr.nowarn,
        order = ctx.attr.order,
        compile_data = ctx.files.compile_data,
        verbose_level = verbose_level,
        force_coverage_instrumentation = ctx.attr.enable_coverage,
        preload_image = ctx.attr.preload_image,
    )

    runfiles = ctx.runfiles(collect_default = True)
    if ctx.attr.image:
        runfiles = runfiles.merge(ctx.attr.image[DefaultInfo].default_runfiles)
    for compile_data in ctx.attr.compile_data:
        runfiles = runfiles.merge(compile_data[DefaultInfo].default_runfiles)
    return [
        compile.lisp_info,
        DefaultInfo(
            runfiles = runfiles,
            files = depset([compile.output_fasl]),
        ),
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["instrumented_deps"],
        ),
    ]

# Internal rule that creates a Lisp library FASL file.
# Keep the _lisp_library rule class name, so Grok can find the targets.
_lisp_library = rule(
    implementation = _lisp_library_impl,
    attrs = dict(_lisp_common_attrs + [
        # After there's some API for accessing native rule internals, we can get
        # rid of this, but while we're implementing this as a macro that
        # generates native and Skylark rules, this rule needs some additional
        # attributes in order to get coverage instrumentation correct.
        ("instrumented_deps", attr.label_list(allow_files = True)),
    ]),
    # Access to the cpp compiler options.
    fragments = ["cpp"],
    outputs = {"fasl": "%{name}.fasl"},
)

def _label(label):
    """Create a cannonical form of the 'label' that includes the colon."""
    if ":" in label:
        return label
    pos = (label.rfind("/") or -1) + 1
    return label + ":" + label[pos:]

def _make_cdeps_dependencies(deps):
    """Transform the 'deps' labels into cdeps dependency labels."""
    return [_label(d) + ".cdeps" for d in deps]

def make_cdeps_library(
        name,
        deps = [],
        cdeps = [],
        tags = [],
        testonly = False,
        visibility = "//visibility:private",
        **kwargs):
    """Create native.cc_library representing a Lisp target's cdeps.

    This target is named [name].cdeps.

    Args:
      name: Name of the Lisp target.
      deps: Immediate Lisp deps for the Lisp target.
      cdeps: C++ dependencies for the target.
      tags: Blaze tags for the cdeps target.
      testonly: Whether the target should be testonly. Should be true if the
          Lisp target is a test or testonly.
      visibility: Visibility for the Lisp target.
      **kwargs: Common attributes for all targets.

    Returns:
      The name of the C++ library: <name>.cdeps
    """
    cdeps_library = name + ".cdeps"
    tags = list(tags)
    _add_tag("manual", tags)
    native.cc_library(
        name = cdeps_library,
        deps = cdeps + _make_cdeps_dependencies(deps),
        tags = tags,
        testonly = testonly,
        visibility = visibility,
        **kwargs
    )

    # This is used for dynamic loading of targets from the REPL.
    native.cc_binary(
        name = "lib{}.so".format(name),
        deps = [cdeps_library],
        linkshared = 1,
        linkstatic = 0,
        tags = tags,
        visibility = ["//visibility:private"],
        testonly = True,
        **kwargs
    )
    return cdeps_library

def lisp_library(
        name,
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
        cdeps = [],
        verbose = None,
        **kwargs):
    """Bazel rule to create a library from Common Lisp source files.

    For more information on the common rule attributes refer to:
    https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes

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
            previous sources loaded (default).
          "multipass" - each source is compiled in an image with
            all sources loaded.
          "parallel" - each source is compiled independently from others.
      nowarn: a list of suppressed Lisp warning types or warning handlers.
      image: the base image used to compile the target (default BAZEL_LISP).
      visibility: list of labels controlling which other rules can use this one.
      testonly: If 1, only test targets can use this rule.
      cdeps: this will link the cc dependencies into the image.
      verbose: internal numeric level of verbosity for the build rule.
      **kwargs: other common attributes.
    """
    # This macro calls _make_cdeps_library, _lisp_library, _dump_lisp_deps.

    cdeps_library = make_cdeps_library(
        name = name,
        deps = [image] + deps,
        cdeps = cdeps,
        visibility = visibility,
        testonly = testonly,
    )

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
        # lisp_library attributes (for coverage instrumentation).
        instrumented_deps = deps + [image, cdeps_library],
        # Common rule attributes.
        visibility = visibility,
        testonly = testonly,
        verbose = verbose,
        **kwargs
    )

    _dump_lisp_deps(
        name = name + ".deps",
        target = name,
        visibility = ["//visibility:private"],
        tags = ["manual"],
        testonly = testonly,
    )
