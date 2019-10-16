"""Implementation of the Lisp build rules.

These rules are used by projects that contain Lisp sources.
They are required to build Lisp binaries or libraries, or run Lisp tests.

The three rules implemented here are:
 lisp_binary - for Lisp binaries,
 lisp_library - for Lisp libraries,
 lisp_test - a test binary run with blaze/bazel test.


The code here defines a few Skylark "real" rules and wraps them in
"Skylark macro" functions. The few real rules have following names:
 _lisp_binary
 _lisp_library
 _skylark_wrap_lisp_test
 _skylark_wrap_lisp_binary
These "rule class names" are used to find Lisp targets.
This is useful with 'bazel query' and for tools indexing the Lisp codebase.
"""

load(
    "//:provider.bzl",
    "LispInfo",
    "extend_lisp_provider",
    "output_dir_info",
    "transitive_deps",
)
load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain")
load(
    "//third_party/bazel/tools/build_defs/cc:action_names.bzl",
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

# TODO(czak): This needs to have a proper path.
BAZEL_LISP = "//:bazel"

BAZEL_LISP_MAIN = "bazel.main::main"

lisp_files = [".lisp", ".lsp"]

# Common attributes accepted by the (internal) lisp rules.
_lisp_common_attrs = [
    ("srcs", attr.label_list(allow_files = lisp_files)),
    ("deps", attr.label_list(providers = [LispInfo])),
    ("order", attr.string(
        default = "serial",
        values = ["multipass", "serial", "parallel"],
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

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = [output],
        progress_message = msg,
        mnemonic = "LispConcatFASLs",
        command = cmd,
    )

def _default_flags(ctx, trans, verbose_level):
    """Returns a list of default flags based on the context and trans provider.

    Args:
     ctx: the context of the compile action.
     trans: the Lisp provider with transitive dependencies.
     verbose_level: if positive a --verbose flags is added.

    Returns:
      A list of flags
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

    # TODO(dougk): Use a --define flag in bazelrc for this.
    sanitizer = ["msan"] if "-fsanitize=memory" in c_options else []
    flags = [
        "--compilation-mode",
        ctx.var.get("LISP_COMPILATION_MODE", ctx.var["COMPILATION_MODE"]),
        "--bindir",
        ctx.bin_dir.path,
        "--features",
        " ".join(trans.features.to_list() + sanitizer),
    ]

    if (ctx.coverage_instrumented() or
        (hasattr(ctx.attr, "enable_coverage") and ctx.attr.enable_coverage)):
        flags += ["--coverage"]

    if verbose_level > 0:
        flags += ["--verbose", str(verbose_level)]

    # TODO(czak): Find out how to simplify passing NDEBUG here.
    if "-UNDEBUG" in cpp_options:
        flags += ["--safety", "3"]
    elif "-DNDEBUG" in cpp_options:
        flags += ["--safety", "0"]

    if int(ctx.var.get("LISP_BUILD_FORCE", "0")) > 0:
        flags += ["--force"]

    return flags

def _compile_srcs(
        ctx,
        srcs,
        deps,
        image,
        order,
        compile_data,
        flags,
        nowarn,
        verbosep,
        preload_image):
    """Compiles the 'srcs' in the context 'ctx'.

    Args:
     ctx: the context used to instantiate the compile actions.
     srcs: the Lisp file sources.
     deps: depset of Lisp files from dependencies
     image: the Lisp executable image used to compile the sources.
     order: the order in which sources are loaded and compiled.
     compile_data: is additional data used to compile the sources.
     flags: are the flags to be passed to Lisp compilation image.
     nowarn: is the list of suppressed Lisp warning types or warning handlers.
     verbosep: is a flag that if True, prints some verbose warnings.
     preload_image: Whether to preload all deps into a single image to use for
       compilation. If None, do this heurisitcally when there are multiple source
       files and many deps.

    Returns:
      A structure with FASLs, hash files, and warning files.
    """
    if not srcs:
        return struct(fasls = [], hashes = [], warnings = [])

    build_image = image.files.to_list()[0]
    compile_image = build_image

    image_srcs = image[LispInfo].srcs if LispInfo in image else []
    deps = [d for d in deps.to_list() if not d in image_srcs]
    env = {"LISP_MAIN": BAZEL_LISP_MAIN}
    multipass = (order == "multipass")
    serial = (order == "serial")
    load_ = srcs if multipass else []

    # Arbitrary heuristic to reduce load on the build system by bundling
    # FASL and source files load into one compile-image binary.
    #
    # TODO(sfreilich): Use a build_setting instead:
    # google3/third_party/bazel_skylib/rules/common_settings.bzl
    if preload_image == None:
        preload_image = ((len(srcs) - 1) * len(deps) > 100)
    if preload_image:
        # Generate a SRCS image.
        compile_image = ctx.actions.declare_file(ctx.label.name + ".srcs.image")
        srcs_flags = flags[:]
        srcs_flags += ["--outs", compile_image.path]
        if deps:
            srcs_flags += ["--deps", _paths(deps)]
        if load_:
            srcs_flags += ["--load", _paths(load_)]
        if nowarn:
            srcs_flags += ["--nowarn", " ".join(nowarn)]

        inputs = sorted(depset(
            [build_image] + load_,
            transitive = [compile_data, depset(deps)],
        ).to_list())
        msg = "Preparing {} (from {} deps{})".format(
            compile_image.short_path,
            len(deps),
            " and {} srcs".format(len(load_)) if load_ else "",
        )
        ctx.actions.run(
            outputs = [compile_image],
            inputs = inputs,
            progress_message = msg,
            mnemonic = "LispSourceImage",
            env = env,
            arguments = ["binary"] + srcs_flags,
            executable = build_image,
        )

        # All deps included above. However, we need to keep --deps the same in
        # the command line below for the sake of analysis that uses extra
        # actions to examine the command-line of LispCompile actions.
        flags = flags + ["--deps-already-loaded"]

    if multipass:
        nowarn = nowarn + ["redefined-method", "redefined-function"]

    # buildozer: disable=print
    if verbosep:
        print("Target: " + ctx.label.name)
        print("Build Img: " + build_image.short_path)
        print("Compile Img: " + compile_image.short_path)

    fasls = []
    warnings = []
    hashes = []
    for src in srcs:
        stem = src.short_path[:-len(src.extension) - 1]
        file_flags = flags[:]
        fasls.append(ctx.actions.declare_file(stem + "~.fasl"))
        hashes.append(ctx.actions.declare_file(stem + "~.hash"))
        warnings.append(ctx.actions.declare_file(stem + "~.warnings"))
        outs = [fasls[-1], hashes[-1], warnings[-1]]
        file_flags += ["--outs", _paths(outs)]
        file_flags += ["--srcs", src.path]

        if deps:
            file_flags += ["--deps", _paths(deps)]
        if load_:
            file_flags += ["--load", _paths(load_)]
        if nowarn:
            file_flags += ["--nowarn", " ".join(nowarn)]

        inputs = sorted(depset(
            [src] + load_,
            transitive = [compile_data, depset(deps)],
        ).to_list())
        ctx.actions.run(
            outputs = outs,
            inputs = inputs,
            tools = [compile_image],
            progress_message = "Compiling " + src.short_path,
            mnemonic = "LispCompile",
            env = env,
            arguments = ["compile"] + file_flags,
            executable = compile_image,
        )
        if serial:
            load_.append(src)

    return struct(
        fasls = fasls,
        hashes = hashes,
        warnings = warnings,
    )

################################################################################
# Lisp Binary and Lisp Test
################################################################################

def _lisp_binary_implementation(ctx):
    """Lisp specific implementation for lisp_binary and lisp_test rules."""
    verbose_level = max(
        ctx.attr.verbose,
        int(ctx.var.get("VERBOSE_LISP_BUILD", "0")),
    )
    verbosep = verbose_level > 0

    # buildozer: disable=print
    if verbosep:
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        print("Executable Core: %s" % ctx.outputs.executable)

    trans = extend_lisp_provider(
        transitive_deps(ctx.attr.deps, build_image = ctx.attr.image),
        # Add those features to trans already.
        features = ctx.attr.lisp_features,
        compile_data = ctx.files.compile_data,
    )

    flags = _default_flags(ctx, trans, verbose_level)

    nowarn = ctx.attr.nowarn

    compile = _compile_srcs(
        ctx = ctx,
        srcs = ctx.files.srcs,
        deps = trans.srcs,
        image = ctx.attr.image,
        order = ctx.attr.order,
        compile_data = trans.compile_data,
        flags = flags,
        nowarn = nowarn,
        verbosep = verbosep,
        preload_image = ctx.attr.preload_image,
    )

    # TODO(czak): Add --hashes, and --warnings flags to bazl.main.
    deps = trans.deps
    hashes = depset(compile.hashes, transitive = [trans.hashes])
    warnings = depset(compile.warnings, transitive = [trans.warnings])

    if LispInfo in ctx.attr.image:
        # The image already includes some deps.
        included = ctx.attr.image[LispInfo]
        deps = depset([d for d in deps.to_list() if not d in included.deps])
        hashes = depset([h for h in hashes.to_list() if not h in included.hashes])
        warnings = depset([w for w in warnings.to_list() if not w in included.warnings])

    build_image = ctx.file.image

    # buildozer: disable=print
    if verbosep:
        print("Build image: %s" % build_image.short_path)
    specs = ctx.actions.declare_file(ctx.label.name + ".specs")
    ctx.actions.write(
        output = specs,
        content = (
            "".join([
                _spec("srcs", compile.fasls),
                _spec("deps", deps.to_list()),
                _spec("warnings", warnings.to_list()),
                _spec("hashes", hashes.to_list()),
            ])
        ),
    )

    inputs = sorted(depset(
        [specs] + compile.fasls,
        transitive = [
            deps,
            trans.compile_data,
            hashes,
            warnings,
        ],
    ).to_list())

    core = ctx.outputs.executable
    outs = [core]
    flags += ["--specs", specs.path]
    flags += ["--outs", _paths(outs)]
    flags += ["--main", ctx.attr.main]
    if nowarn:
        flags += ["--nowarn", " ".join(nowarn)]
    if ctx.attr.precompile_generics:
        flags += ["--precompile-generics"]
    if ctx.attr.save_runtime_options:
        flags += ["--save-runtime-options"]

    ctx.actions.run_shell(
        outputs = outs,
        inputs = inputs,
        tools = [build_image],
        progress_message = "Building lisp core %s" % core.short_path,
        mnemonic = "LispCore",
        command = "LISP_MAIN=%s %s binary '%s'" % (
            BAZEL_LISP_MAIN,
            build_image.path,
            "' '".join(flags),
        ),
    )

    if compile.fasls:
        output_fasl = ctx.actions.declare_file(ctx.label.name + ".fasl")
        _concat_files(ctx, compile.fasls, output_fasl)
        output_fasls = [output_fasl]
    else:
        output_fasls = []

    runfiles = ctx.runfiles(files = outs, collect_default = True)
    if ctx.attr.image:
        runfiles = runfiles.merge(ctx.attr.image[DefaultInfo].default_runfiles)
    for compile_data in ctx.attr.compile_data:
        runfiles = runfiles.merge(compile_data[DefaultInfo].default_runfiles)
    return [
        output_dir_info(ctx),
        extend_lisp_provider(
            trans,
            srcs = ctx.files.srcs,
            deps = output_fasls,
            hashes = compile.hashes,
            warnings = compile.warnings,
        ),
        DefaultInfo(runfiles = runfiles, executable = core),
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["deps", "image"],
        ),
    ]

# Internal rule used to generate action that creates a Lisp binary core.
# Keep the name to be _lisp_binary - Grok depends on this name to find targets.
_lisp_binary = rule(
    implementation = _lisp_binary_implementation,
    # Access to the cpp compiler options.
    fragments = ["cpp"],
    attrs = dict(_lisp_common_attrs + [
        ("main", attr.string(default = "main")),
        ("precompile_generics", attr.bool()),
        ("save_runtime_options", attr.bool()),
    ]),
    executable = True,
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
        progress_message = "Copying to %s" % out.short_path,
        command = (
            "mv " + ctx.file.binary.path + " " + out.path
        ),
    )

    # Forward the runfiles. The data attribute is passed here as well, but just
    # so that targets in data can be referenced in attrs of this. In
    # particular, a lisp_test's args attribute might include
    # $(location :something-in-data).
    runfiles = ctx.runfiles(files = [out])
    runfiles = runfiles.merge(ctx.attr.core[DefaultInfo].default_runfiles)
    return [
        output_dir_info(ctx),
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
            "\n".join(["feature: " + f for f in lisp_info.features] +
                      ["src: " + f.path for f in lisp_info.srcs])
        ),
    )
    return [
        output_dir_info(ctx),
        DefaultInfo(files = depset([out])),
    ]

# Internal rule that creates a Lisp library DEPS file.
# DEPS file is used to list all the Lisp sources for a target.
# It is a quick hack to make (bazel:load ...) work.
_dump_lisp_deps = rule(
    implementation = _dump_lisp_deps_impl,
    attrs = {
        "target": attr.label(mandatory = True, providers = [LispInfo]),
    },
)

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
        elfcore = True,
        visibility = None,
        testonly = 0,
        csrcs = [],
        cdeps = [],
        copts = [],
        test = False,
        flaky = False,
        size = "medium",
        timeout = None,
        shard_count = None,
        tags = [],
        stamp = -1,
        # TODO(czak): Need to provide proper path here.
        malloc = "@bazel_tools//tools/cpp:malloc",
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
      save_runtime_options: save runtime options and prevent those being
          interpreted by the target binary with a main entry point (default True).
          Setting this to False allows SBCL to process following flags:
           --help, --version, --core, --dynamic-space-size, --control-stack-size.
      precompile_generics: precompile generic functions if True (as by default).
        Must be set to False to allow compiling arbitary amounts of code to
        memory, so should be set to false for targets intended for use as a
        compilation image.
      elfcore: whether code should be placed in an ELF section (default True).
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
      shard_count: number of shards used for the test.
      tags: list of arbitrary text tags mostly useful for tests. E.g.: "local".
      stamp: C++ build stamp info (0 = no, 1 = yes, default -1 = bazel option).
      malloc: malloc implementation to be used for linking of cc code.
      verbose: internal numeric level of verbosity for the build rule.
      **kwargs: other common attributes for binary targets.
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
    # (2) Partially ELFinated binary (elfcore=False): After SAVE-LISP-AND-DIE,
    #     slurp the Lisp heap back out of the resulting file, and turn it into
    #     a data section in a proper ELF file, but a relatively opaque section
    #     with the only C symbols acting to demarcate the bounds of the Lisp
    #     spaces. Feed that in to a regular link step (with C++ libaries and
    #     such, and the SBCL main). This binary when launched will "parse" the
    #     Lisp heap out of the data section and begin life as usual.
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
    #     "because Lisp".) Suffice it to say that it imposes a constraint on
    #     the size of the code space, as it gets _directly_ memory-mapped from
    #     the '.text' section in the resulting executable. That means that
    #     executables in this format are not suitable for use as a build image
    #     (for that, set elfcore=False).
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
    _lisp_binary(
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
        # For testing, control the behavior where deps are preloads in
        # compilation image.
        preload_image = kwargs.pop("preload_image", None),
        **kwargs
    )

    # Precompile all C sources in advance, before core symbols are present.
    cdeps_library = make_cdeps_library(
        name = name,
        deps = [image] + deps,
        csrcs = csrcs,
        cdeps = cdeps,
        copts = copts,
        visibility = visibility,
        testonly = testonly,
        **kwargs
    )

    if elfcore:
        # Produce a '.s' file holding only compiled Lisp code and a '-core.o'
        # containing the balance of the original Lisp spaces.
        native.genrule(
            name = name + "-parts",
            tools = ["@local_sbcl//:elfinate"],
            srcs = [core],
            outs = [name + ".s", name + ".core", name + "-core.o"],
            cmd = "$(location @local_sbcl//:elfinate) split " +
                  "$(location %s) $(location %s.s)" % (core, name),
            visibility = ["//visibility:private"],
            testonly = testonly,
        )

        # The final executable still needs to be produced by a Skylark rule, so
        # it can get the LispInfo and instrumented_files_info providers
        # correct. That means the internal rule must be cc_binary, only the
        # outermost rule is a test for lisp_test.
        binary = name + "-combined"
        native.cc_binary(
            name = binary,
            linkopts = [
                # SBCL cannot generate position-independent code, and -pie
                # is becoming the default. (NOTE: until the SBCL-compiled
                # functions are actually built as an ELF library,
                # theoretically we could build with -pie by modifying the
                # build for libsbcl.a, but that'd be basically a lie since
                # most of the code would still be mapped at a fixed
                # address.)
                "-Wl,-no-pie",
            ],
            srcs = [name + ".s", name + "-core.o"],
            deps = [cdeps_library, "@local_sbcl//:c-support"],
            copts = copts,
            visibility = ["//visibility:private"],
            stamp = stamp,
            malloc = malloc,
            testonly = testonly,
        )
    else:
        # Copy entire native SBCL core into a binary blob in a normal '.o' file
        native.genrule(
            name = name + "-elfcore",
            tools = ["@local_sbcl//:elfinate"],
            srcs = [core],
            outs = [name + "-core.o", name + "-syms.lds"],
            cmd = ("$(location @local_sbcl//:elfinate) copy " +
                   "$(location {core}) $(location {name}-core.o) && " +
                   "nm -p $(location {name}-core.o) | " +
                   "awk '" +
                   '{{print $$2";"}}BEGIN{{print "{{"}}END{{print "}};"}}' +
                   "' >$(location {name}-syms.lds)").format(
                core = core,
                name = name,
            ),
            visibility = ["//visibility:private"],
            testonly = testonly,
        )

        # Link that '.o' file with cdeps and SBCL's main
        binary = name + "-combined"
        native.cc_binary(
            name = binary,
            linkopts = [
                ("-Wl,--dynamic-list=$(location %s-syms.lds)" % name),
                "-Wl,-no-pie",
            ],
            srcs = [name + "-core.o"],
            deps = [
                name + "-syms.lds",
                cdeps_library,
                "@local_sbcl//:c-support",
            ],
            data = data,
            copts = copts,
            visibility = ["//visibility:private"],
            stamp = stamp,
            malloc = malloc,
            testonly = testonly,
        )

    # Note that this treats csrcs the same as the srcs of targets in cdeps. That's
    # not quite intuitive, but just adding csrcs to instrumented_srcs (and adding
    # cdeps and _make_cdeps_dependencies(deps) to instrumented_deps instead of
    # cdeps_library doesn't work, it doesn't include the right metadata files
    # included by the InstrumentedFilesProvider of the native cc_library rule).
    instrumented_srcs = srcs
    instrumented_deps = deps + [image, cdeps_library]

    if test:
        _skylark_wrap_lisp_test(
            name = name,
            binary = binary,
            instrumented_srcs = instrumented_srcs,
            instrumented_deps = instrumented_deps,
            core = core,
            data = data,
            size = size,
            timeout = timeout,
            flaky = flaky,
            shard_count = shard_count,
            args = args,
            visibility = visibility,
            testonly = testonly,
            tags = tags,
        )
    else:
        _skylark_wrap_lisp_binary(
            name = name,
            binary = binary,
            instrumented_srcs = instrumented_srcs,
            instrumented_deps = instrumented_deps,
            core = core,
            data = data,
            args = args,
            visibility = visibility,
            testonly = testonly,
            tags = tags,
        )

    _dump_lisp_deps(
        name = name + ".deps",
        target = name,
        visibility = ["//visibility:private"],
        tags = ["manual"],
        testonly = testonly,
    )

def lisp_test(name, image = BAZEL_LISP, stamp = 0, **kwargs):
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
      **kwargs: other keyword arguments that are passed to lisp_binary.
    """

    # Macro: an alias for lisp_binary.
    lisp_binary(
        name,
        image = image,
        stamp = stamp,
        testonly = 1,
        test = True,
        **kwargs
    )

################################################################################
# Lisp Library
################################################################################

# Note that this is part of the cl_protobufs_aspect implementation in
# google3/lisp/devtools/proto/clpb.bzl.
def lisp_library_implementation(
        ctx,
        srcs = None,
        transitive = None,
        output_fasl = None,
        image = None,
        order = None,
        nowarn = None,
        include_rule_providers = True):
    """Lisp specific implementation for lisp_library rules.

    Args:
       ctx: Rule context
       srcs: Overrides ctx.attr.srcs
       transitive: LispInfo representing transitive deps, overrides generation
         of that based on deps and image
       output_fasl: Overrides ctx.outputs.fasl
       image: Overrides ctx.attr.image
       order: Overrides ctx.attr.order
       nowarn: Overrides ctx.attr.nowarn
       include_rule_providers: Set this to false when using it in the implementation
         of an aspect that wraps a tree of dependencies in Lisp libraries. That
         prevents DefaultInfo from being generated for both the aspect and the
         enclosing rule, which is prohibited.

    Returns:
        List of providers.
    """
    verbose_level = max(
        getattr(ctx.attr, "verbose", 0),
        int(ctx.var.get("VERBOSE_LISP_BUILD", "0")),
    )
    verbosep = verbose_level > 0

    # buildozer: disable=print
    if verbosep:
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        print("Library: %s" % ctx.label.name)

    build_image = image or ctx.attr.image
    trans = transitive or extend_lisp_provider(
        transitive_deps(ctx.attr.deps, build_image = build_image),
        # Add those features to trans already.
        features = ctx.attr.lisp_features,
        compile_data = ctx.files.compile_data,
    )

    flags = _default_flags(ctx, trans, verbose_level)
    srcs = srcs or ctx.files.srcs
    output_fasl = output_fasl or ctx.outputs.fasl

    nowarn = nowarn or getattr(ctx.attr, "nowarn", [])
    compile = _compile_srcs(
        ctx = ctx,
        srcs = srcs,
        deps = trans.srcs,
        image = build_image,
        order = order or ctx.attr.order,
        compile_data = trans.compile_data,
        flags = flags,
        nowarn = nowarn,
        verbosep = verbosep,
        preload_image = getattr(ctx.attr, "preload_image", None),
    )

    # Need to concatenate the FASL files into name.fasl.
    _concat_files(ctx, compile.fasls, output_fasl)

    providers = [
        extend_lisp_provider(
            trans,
            deps = [output_fasl] if compile.fasls else [],
            srcs = srcs,
            hashes = compile.hashes,
            warnings = compile.warnings,
        ),
    ]
    if include_rule_providers:
        runfiles = ctx.runfiles(collect_default = True)
        if build_image:
            runfiles = runfiles.merge(build_image[DefaultInfo].default_runfiles)
        for compile_data in ctx.attr.compile_data:
            runfiles = runfiles.merge(compile_data[DefaultInfo].default_runfiles)
        providers.extend([
            output_dir_info(ctx),
            DefaultInfo(
                runfiles = runfiles,
                files = depset([output_fasl]),
            ),
            coverage_common.instrumented_files_info(
                ctx,
                source_attributes = ["srcs"],
                dependency_attributes = ["instrumented_deps"],
            ),
        ])
    return providers

# Internal rule that creates a Lisp library FASL file.
# Keep the _lisp_library rule class name, so Grok can find the targets.
_lisp_library = rule(
    implementation = lisp_library_implementation,
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
        csrcs = [],
        cdeps = [],
        copts = [],
        features = [],
        visibility = None,
        testonly = 0):
    """Create a CDEPS library for the Lisp library with 'name'.

    Args:
      name: the name of the Lisp library.
      deps: other Lisp library names used to derive transitive dependencies.
      csrcs: the C++ sources for the library.
      cdeps: the C++ dependencies.
      copts: options passed to the C++ compiler.
      features: features passed through to cc_library.
      visibility: the visibility of the C++ library.
      testonly: if 1, the targets are marked as needed for tests only.

    Returns:
      The name of the C++ library: <name>.cdeps
    """

    # Macro: calling cc_library.
    # Called from lisp_library and lisp_binary.
    cdeps_library = "%s.cdeps" % name
    cdeps = cdeps + _make_cdeps_dependencies(deps)
    native.cc_library(
        name = cdeps_library,
        srcs = csrcs,
        deps = cdeps,
        copts = copts,
        features = features,
        visibility = visibility,
        testonly = testonly,
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
        csrcs = [],
        cdeps = [],
        copts = [],
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
      csrcs: a list of C/C++ source labels.
      cdeps: this will link the cc dependencies into the image.
      copts: a list of string values of options to pass to cc_library.
      verbose: internal numeric level of verbosity for the build rule.
      **kwargs: other common attributes for binary targets.
    """
    # This macro calls _make_cdeps_library, _lisp_library, _dump_lisp_deps.

    # For testing, control the behavior that preloads deps before compilation.
    preload_image = kwargs.pop("preload_image", None)

    cdeps_library = make_cdeps_library(
        name = name,
        deps = [image] + deps,
        csrcs = csrcs,
        cdeps = cdeps,
        copts = copts,
        features = features,
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
        # lisp_library attributes (for coverage instrumentation). Note that this
        # treats csrcs the same as the srcs of targets in cdeps. See longer note
        # about instrumented_deps in definition of lisp_binary above.
        instrumented_deps = deps + [image, cdeps_library],
        # Common rule attributes.
        visibility = visibility,
        testonly = testonly,
        verbose = verbose,
        preload_image = preload_image,
        **kwargs
    )

    _dump_lisp_deps(
        name = name + ".deps",
        target = name,
        visibility = ["//visibility:private"],
        tags = ["manual"],
        testonly = testonly,
    )
