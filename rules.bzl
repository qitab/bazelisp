# Copyright 2015-2020 Google LLC
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.

"""Build rules for Common Lisp.

The three rules defined here are:
  lisp_library - The basic unit of compilation
  lisp_binary - Outputs an executable binary
  lisp_test - Outputs a binary that is run with the test command

Usage example:

load("//your/path/to/bazel:rules.bzl", "lisp_binary", "lisp_library", "lisp_test")

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
"""

load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain", "use_cc_toolchain")
load(
    ":provider.bzl",
    "LispInfo",
    "collect_lisp_info",
    "extend_lisp_info",
)

_BAZEL_LISP_IMAGE_MAIN = "bazel.main:main"
_BAZEL_LISP_IMAGE_ENV = {"LISP_MAIN": _BAZEL_LISP_IMAGE_MAIN}
_ELFINATE = "//:elfinate"
_DEFAULT_LIBSBCL = "@local_sbcl//:c-support"

_LISP_LIBRARY_ATTRS = {
    "srcs": attr.label_list(
        allow_files = [".lisp", ".lsp"],
        doc = ("Common Lisp (`.lisp` or `.lsp`) source files. If there are " +
               "multiple files in `srcs`, each is compiled with its " +
               "predecessors loaded."),
    ),
    "deps": attr.label_list(
        providers = [LispInfo],
        doc = ("Common Lisp dependencies (generally [`lisp_library`]" +
               "(#lisp-library), but you can put [`lisp_binary`]" +
               "(#lisp-binary) in deps for testing)."),
    ),
    "cdeps": attr.label_list(
        providers = [CcInfo],
        doc = ("C++ dependencies (generally [`cc_library`]" +
               "(https://docs.bazel.build/versions/master/be/c-cpp.html" +
               "#cc_library))."),
    ),
    "block_compile": attr.bool(
        default = False,
        doc = ("Whether to block-compile the sources. By default, this will " +
               "cause sources to be block-compiled together as a single " +
               "block, that behavior can be overridden by " +
               "block_compile_specified_only."),
    ),
    "block_compile_specified_only": attr.bool(
        default = False,
        doc = ("If true, block compilation only considers multiple top-level " +
               "forms together if those are between explicit (START-BLOCK) " +
               "and (END-BLOCK)."),
    ),
    "data": attr.label_list(
        allow_files = True,
        doc = ("Data available to this target and its consumers in the " +
               "runfiles directory at runtime."),
    ),
    "compile_data": attr.label_list(
        allow_files = True,
        doc = ("Data available to this target and its consumers at build " +
               "time, added to the inputs of LispCompile and LispCore " +
               "actions."),
    ),
    "add_features": attr.string_list(
        doc = ("Names of symbols (by default in the keyword package) to be " +
               "added to `\\*features\\*` of this library and its consumers, at " +
               "compile time and in the resulting binary. Note that this " +
               "differs from the [`features`](https://docs.bazel.build/" +
               "versions/master/be/common-definitions.html#common.features) " +
               "attribute common to all build rules which controls " +
               "[toolchain](https://docs.bazel.build/versions/master/" +
               "toolchains.html) features."),
    ),
    "nowarn": attr.string_list(
        doc = "Suppressed Lisp warning types or warning handlers.",
    ),
    "image": attr.label(
        allow_single_file = True,
        executable = True,
        cfg = "target",
        default = Label("//:lfc"),
        doc = (
            "Lisp binary used as Bazel compilation image. This should be a " +
            "binary with the main function `#'bazel:main` defined in " +
            "`main.lisp`."
        ),
    ),
    "verbose": attr.int(
        default = 0,
        doc = ("Enable verbose debugging output when analyzing and " +
               "compiling this target (`0` = none (default), `3` = max)."),
    ),
    "instrument_coverage": attr.int(
        values = [-1, 0, 1],
        default = -1,
        doc = (
            "Force coverage instrumentation. Possible values:\n" +
            "\n" +
            "`0`: Never instrument this target. Should be used if the" +
            "target compiles generated source files or does not compile" +
            "with coverage instrumentation.\n" +
            "\n" +
            "`1`: Always instrument this target. Generally should not be " +
            "used outside of tests for the coverage implementation.\n" +
            "\n" +
            "`-1` (default): If coverage data collection is enabled, " +
            "instrument this target per [`--instrumentation_filter]" +
            "(https://docs.bazel.build/versions/master/" +
            "command-line-reference.html#flag--instrumentation_filter).`"
        ),
    ),
}

_LISP_BINARY_ATTRS = dict(_LISP_LIBRARY_ATTRS)
_LISP_BINARY_ATTRS.update({
    "main": attr.string(
        default = "main",
        doc = ("Name of function (by default in the `cl-user` package) or " +
               "snippet of Lisp code to run when starting the binary. " +
               '`"nil"` or `"t"` to start the default REPL. Can be ' +
               "overridden by naming a function (or `nil` or `t`) in the " +
               "`LISP_MAIN` environment variable."),
    ),
    "_malloc_dontuse": attr.label(
        default = Label("@bazel_tools//tools/cpp:malloc"),
        providers = [CcInfo],
        doc = ("Target providing a custom malloc implementation. Same as " +
               "[`cc_binary.malloc`](https://docs.bazel.build/versions/" +
               "master/be/c-cpp.html#cc_binary.malloc)."),
    ),
    "stamp": attr.int(
        values = [-1, 0, 1],
        default = -1,
        doc = ("Same as [`cc_binary.stamp`](https://docs.bazel.build/" +
               "versions/master/be/c-cpp.html#cc_binary.stamp)."),
    ),
    "allow_save_lisp": attr.bool(
        default = False,
        doc = ("Whether to preserve the ability to run `save-lisp-and-die` " +
               "instead of altering the binary format to be more compatible " +
               "with C++ debugging tools (which, for example, allows you to " +
               "get combined stacktraces of C/C++ and Lisp code). Must be " +
               "`True` for targets used as a compilation image."),
    ),
    "precompile_generics": attr.bool(
        default = True,
        doc = "If `False`, skip precompiling generic functions.",
    ),
    "save_runtime_options": attr.bool(
        default = True,
        doc = ("If `False`, process SBCL runtime options at the " +
               "command-line on binary startup."),
    ),
    "runtime": attr.label(
        default = Label(_DEFAULT_LIBSBCL),
        providers = [CcInfo],
        doc = ("SBCL C++ dependencies. Consumers should generally omit this " +
               "attr and use the default value."),
    ),
    "helper_script": attr.label(
        default = Label("//:imagesave.lisp"),
        allow_single_file = True,
    ),
    "_elfinate": attr.label(
        default = Label(_ELFINATE),
        executable = True,
        allow_single_file = True,
        cfg = "target",
    ),
    "_custom_malloc": attr.label(
        default = configuration_field(
            fragment = "cpp",
            name = "custom_malloc",
        ),
        providers = [CcInfo],
    ),
})

_LISP_TEST_ATTRS = dict(_LISP_BINARY_ATTRS)
_LISP_TEST_ATTRS.update({
    # For tests we want to avoid taking time to convert the Lisp text space to an ELF section.
    # "allow-save-lisp" implements that, which doesn't really mean that a test may invoke
    # SAVE-LISP. It just coincidental that allowing save-lisp implies NOT converting to ELF.
    "allow_save_lisp": attr.bool(default = True, doc = "do not touch"),
    "precompile_generics": attr.bool(default = False, doc = "do not touch"),
    "stamp": attr.int(
        values = [-1, 0, 1],
        default = 0,
        doc = ("Same as [`cc_test.stamp`](https://docs.bazel.build/" +
               "versions/master/be/c-cpp.html#cc_test.stamp). Build version " +
               "stamping is disabled by default."),
    ),
})

def _concat_fasls(ctx, inputs, output):
    """Concatenates several FASLs into a combined FASL.

    Args:
      ctx: Rule context
      inputs: List of files to concatenate.
      output: File output for the concatenated contents.
    """
    if not inputs:
        return None
    elif len(inputs) == 1:
        return inputs[0]
    else:
        cat_command = "cat ${@:2} > $1"
        cat_args = ctx.actions.args()
        cat_args.add(output)
        cat_args.add_all(inputs)
        ctx.actions.run_shell(
            inputs = inputs,
            outputs = [output],
            progress_message = "Combining %{output}",
            mnemonic = "LispConcatFASLs",
            command = cat_command,
            arguments = [cat_args],
        )
        return output

def _build_flags(ctx, add_features, verbose_level, instrument_coverage):
    """Returns Args for flags for all Lisp build actions.

    Args:
     ctx: The rule context.
     add_features: Depset of transitive Lisp feature strings provided by this
         target and its dependencies.
     verbose_level: int indicating level of debugging output. If positive, a
         --verbose flags is added.
     instrument_coverage: Controls coverage instrumentation, with the following
         values:
         -1 (default) - Instruments if coverage is enabled for this target.
         0 - Instruments never.
         1 - Instruments always (for testing purposes).

    Returns:
        Args object to be passed to Lisp build actions.
    """
    cc_toolchain = find_cc_toolchain(ctx)

    # Needs to match logic for the :msan config_setting target. Unfortunately,
    # config_setting rules don't yet have a Starlark API. Note that this is not
    # equivalent to looking at ctx.var.get("msan_config"), we want to know if
    # msan is used in this specific configuration, not if it's an msan build in
    # general. (It might be better to look at whether msan is enabled in
    # features with cc_common.configure_features (_cc_configure_features) and
    # cc_common.is_enabled, but the important thing is that the behavior is
    # consistent between this target and the targets in its image and runtime
    # attrs.)
    if cc_toolchain.compiler in ["msan", "msan-track-origins"]:
        add_features = depset(["msan"], transitive = [add_features])
    flags = ctx.actions.args()

    # Maybe there's an explicit LISP_COMPILATION_MODE. Usually not.
    lisp_compilation_mode = ctx.var.get("LISP_COMPILATION_MODE", "none")
    if lisp_compilation_mode == "none":
        # if C++ wants -UNDEBUG then we want Lisp code safety, i.e. fastbuild
        if "-UNDEBUG" in ctx.fragments.cpp.copts:
            lisp_compilation_mode = "fastbuild"
        else:  # failing that, use the command-line-given ("-c") mode
            lisp_compilation_mode = ctx.var["COMPILATION_MODE"]
    flags.add("--compilation-mode", lisp_compilation_mode)
    flags.add("--bindir", ctx.bin_dir.path)
    flags.add_joined("--features", add_features, join_with = " ")

    if (instrument_coverage > 0 or
        (instrument_coverage < 0 and ctx.coverage_instrumented())):
        flags.add("--coverage")

    if verbose_level > 0:
        flags.add("--verbose", str(verbose_level))

    if int(ctx.var.get("LISP_BUILD_FORCE", "0")) > 0:
        flags.add("--force")

    return flags

def _list_excluding_depset(items, exclude):
    exclude_set = {item: True for item in exclude.to_list()}
    return [item for item in items if item not in exclude_set]

def _is_arm_cpu(ctx):
    gnu_triple = ctx.toolchains["@bazel_tools//tools/cpp:toolchain_type"].cc.target_gnu_system_name
    return gnu_triple[0:5] == "aarch"

def _executor_for_ctx(ctx):
    if _is_arm_cpu(ctx):
        return {"requires-arch:arm": "1"}
    else:
        return {}

def lisp_compile_srcs(
        ctx,
        srcs = [],
        deps = [],
        cdeps = [],
        block_compile = False,
        block_compile_specified_only = False,
        image = None,
        add_features = [],
        nowarn = [],
        compile_data = [],
        verbose_level = 0,
        instrument_coverage = -1,
        indexer_metadata = []):
    """Generate LispCompile actions, return LispInfo and FASL output.

    This is the core functionality shared by the Lisp build rules.

    Args:
      ctx: The rule context.
      srcs: list of src Files.
      deps: list of immediate Lisp dependency Targets.
      cdeps: list of immediate C++ dependency Targets.
      block_compile: Whether to block-compile this target.
      block_compile_specified_only: Whether to only combine top-level forms
          in blokcs that are in explicitly specified (with
          `(start-block)` and `(end-block)`) when block compiling.
      image: Build image Target used to compile the sources.
      add_features: list of Lisp feature strings added by this target.
      nowarn: List of suppressed warning type strings.
      compile_data: list of data dependency Targets whose outputs and runfiles
         are made available at load/compile time for this target and its
         consumers.
      verbose_level: int indicating level of debugging output.
      instrument_coverage: Controls coverage instrumentation, with the following values:
         -1 (default) - Instruments if coverage is enabled for this target.
         0 - Instruments never.
         1 - Instruments always (for testing purposes).
      indexer_metadata: Extra metadata files to be passed to the --deps
         flag of LispCompile when the Kythe indexer is run. Ignored by the
         build image itself, but this appears in the command-line for the
         LispCompile action which can be inspected by action_listener.

    Returns:
      struct with fields:
          - lisp_info: LispInfo for the target
          - output_fasl: Combined FASL for this target (which is also included in
              lisp_info.fasls if there are srcs)
          - build_flags: Args to pass to all LispCompile and LispCore actions
    """
    name = ctx.label.name
    verbosep = verbose_level > 0
    indexer_build = (ctx.var.get("GROK_ELLIPSIS_BUILD", "0") == "1")

    lisp_info = collect_lisp_info(
        deps = deps,
        cdeps = cdeps,
        build_image = image,
        features = add_features,
        compile_data = compile_data,
    )
    build_flags = _build_flags(
        ctx = ctx,
        add_features = lisp_info.features,
        verbose_level = verbose_level,
        instrument_coverage = instrument_coverage,
    )

    if not srcs:
        return struct(
            lisp_info = lisp_info,
            output_fasl = None,
            build_flags = build_flags,
        )

    build_image = image[DefaultInfo].files_to_run
    compile_image = build_image

    # Lisp source files for all the transitive dependencies not already in the
    # image, loaded before compilation, passed to --deps.
    deps_srcs = lisp_info.srcs.to_list()
    if LispInfo in image:
        deps_srcs = _list_excluding_depset(deps_srcs, image[LispInfo].srcs)
    if indexer_build:
        deps_srcs.extend(indexer_metadata)

    # Sources for this target loaded before compilation (after deps)
    load_srcs = []

    compile_flags = ctx.actions.args()

    # buildozer: disable=print
    if verbosep:
        print("Target: " + name)
        print("Build Img: " + build_image.executable.short_path)
        print("Compile Img: " + compile_image.executable.short_path)

    fasls = []
    output_fasl = ctx.actions.declare_file(name + ".fasl")
    if block_compile:
        # Compile all at once
        compile_srcs_list = [srcs]
        progress_message = "Compiling %{label}"
    else:
        # Compile one at a time
        compile_srcs_list = [[src] for src in srcs]
        progress_message = "Compiling %{input}"

    for compile_srcs in compile_srcs_list:
        if len(compile_srcs_list) == 1:
            # Either we're compiling everything together for block-compilation
            # or there's only one src.
            compile_fasl = output_fasl
        else:
            # We're in the one-at-a-time case and there are multiple srcs.
            src = compile_srcs[0]
            stem = "{}~/{}".format(name, src.short_path[:-len(src.extension) - 1])
            compile_fasl = ctx.actions.declare_file(stem + ".fasl")
        fasls.append(compile_fasl)
        action_flags = ctx.actions.args()
        action_flags.add("--outs", compile_fasl)
        action_flags.add_joined("--srcs", compile_srcs, join_with = " ")
        action_flags.add_joined("--deps", deps_srcs, join_with = " ")
        action_flags.add_joined("--load", load_srcs, join_with = " ")
        action_flags.add_joined("--nowarn", nowarn, join_with = " ")
        if block_compile:
            action_flags.add("--block-compile")
            if block_compile_specified_only:
                action_flags.add("--block-compile-specified-only")
        gc = ctx.var.get("LISPGC", "gencgc")
        heapsize = "6GB" if gc == "gencgc" else "8GB"
        ctx.actions.run(
            outputs = [compile_fasl],
            inputs = depset(
                compile_srcs + deps_srcs + load_srcs,
                transitive = [lisp_info.compile_data],
                order = "preorder",
            ),
            progress_message = progress_message,
            mnemonic = "LispCompile",
            env = _BAZEL_LISP_IMAGE_ENV,
            arguments = [
                "--dynamic-space-size",
                heapsize,
                "compile",
                build_flags,
                compile_flags,
                action_flags,
            ],
            executable = compile_image,
            toolchain = None,
            execution_requirements = _executor_for_ctx(ctx),
        )
        load_srcs.extend(compile_srcs)

    if indexer_build:
        srcs = indexer_metadata + srcs
    lisp_info = extend_lisp_info(
        lisp_info,
        srcs = srcs,
        fasls = [output_fasl] if srcs else [],
    )
    return struct(
        lisp_info = lisp_info,
        output_fasl = _concat_fasls(ctx, fasls, output_fasl),
        build_flags = build_flags,
    )

def _cc_configure_features(ctx, cc_toolchain):
    return cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )

def _lisp_output_group_info(ctx, lisp_info, fasl_list):
    outputs = {"fasl": fasl_list}
    return OutputGroupInfo(**outputs)

def _lisp_instrumented_files_info(ctx):
    return coverage_common.instrumented_files_info(
        ctx,
        source_attributes = ["srcs"],
        dependency_attributes = ["deps", "cdeps", "image", "data"],
    )

def _lisp_runfiles(ctx):
    runfiles = ctx.runfiles(files = ctx.files.data)
    transitive_runfiles = []
    for runfiles_attr in (
        ctx.attr.srcs,
        ctx.attr.deps,
        ctx.attr.cdeps,
        ctx.attr.data,
    ):
        for target in runfiles_attr:
            transitive_runfiles.append(target[DefaultInfo].default_runfiles)
    transitive_runfiles.append(ctx.attr.image[DefaultInfo].default_runfiles)

    for tr in transitive_runfiles:
        runfiles = runfiles.merge(tr)
    return runfiles

def _lisp_providers(ctx, lisp_info, fasl, executable = None):
    executable_list = [executable] if executable != None else []
    fasl_list = [fasl] if fasl != None else []
    return [
        DefaultInfo(
            runfiles = _lisp_runfiles(ctx),
            files = depset(executable_list or fasl_list),
            executable = executable,
        ),
        lisp_info,
        _lisp_output_group_info(ctx, lisp_info, fasl_list),
        _lisp_instrumented_files_info(ctx),
    ]

################################################################################
# Lisp Binary and Lisp Test
################################################################################

def _lisp_binary_impl(ctx):
    """Implementation for lisp_binary and lisp_test rules."""
    name = ctx.label.name
    core_object_file = ctx.actions.declare_file(name + "-core.o")

    # allow_save_lisp implies that SBCL will write the .o file by itself.
    # It's clearly the wrong name for an attribute with that semantics, but
    # certain SWEs had stronger opinions than mine about what to name it.
    # Since ARM does not support "full ELF" mode, we just use the nice .o file
    if _is_arm_cpu(ctx) or ctx.attr.allow_save_lisp:
        core = core_object_file  # SBCL will produce an ELF '.o' file on its own
    else:
        core = ctx.actions.declare_file(name + ".core")  # it's a preliminary step

    verbose_level = max(
        ctx.attr.verbose,
        int(ctx.var.get("VERBOSE_LISP_BUILD", "0")),
    )
    verbosep = verbose_level > 0

    compile = lisp_compile_srcs(
        ctx = ctx,
        srcs = ctx.files.srcs,
        deps = ctx.attr.deps,
        cdeps = ctx.attr.cdeps,
        image = ctx.attr.image,
        add_features = ctx.attr.add_features,
        nowarn = ctx.attr.nowarn,
        compile_data = ctx.attr.compile_data,
        verbose_level = verbose_level,
        instrument_coverage = ctx.attr.instrument_coverage,
    )

    lisp_info = compile.lisp_info

    fasls = lisp_info.fasls.to_list()

    if LispInfo in ctx.attr.image:
        # The image already includes some deps.
        included = ctx.attr.image[LispInfo]
        fasls = _list_excluding_depset(fasls, included.fasls)

    build_image = ctx.file.image

    # buildozer: disable=print
    if verbosep:
        print("Build image: %s" % build_image.short_path)

    specs = ctx.actions.declare_file(name + ".specs")
    content = ctx.actions.args()
    content.set_param_file_format("multiline")
    content.add_joined(fasls, format_joined = '(:deps\n "%s")', join_with = '"\n "')
    ctx.actions.write(
        output = specs,
        content = content,
    )

    if ctx.file.helper_script == None:
        inputs = [specs]
    else:
        inputs = [specs, ctx.file.helper_script]
    inputs.extend(fasls)
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
    ctx.actions.run(
        outputs = [core],
        inputs = inputs,
        progress_message = "Building Lisp core %{output}",
        mnemonic = "LispCore",
        env = _BAZEL_LISP_IMAGE_ENV,
        arguments = ["core", compile.build_flags, core_flags],
        executable = build_image,
        toolchain = None,
        execution_requirements = _executor_for_ctx(ctx),
    )

    cc_toolchain = find_cc_toolchain(ctx)
    feature_configuration = _cc_configure_features(ctx, cc_toolchain)

    # The support in ELFinator exists for -pie code, and a non-elfinated SBCL binaries
    # are always position-independent; however, extreme inefficiency is imparted to ELF
    # binaries that are position-independent. Lisp pointers are all absolute, and a
    # typical Lisp heap might contain 3 to 5 million pointers to functions, therefore
    # require that many relocations on each invocation to adjust to wherever the system
    # moved the text segment. C on the other hand uses function pointers sparingly.
    # I don't have "typical" numbers of pointers, and it can't be inferred from a binary,
    # but it's nothing like having 40,000 closures over #<FUNCTION ALWAYS-BOUND {xxxxxx}>
    # (which is the SLOT-BOUNDP method "fast method function" for every defstruct slot)
    # and another 40,000 over CALL-NEXT-METHOD and so on and so on.
    linkopts = ["-Wl,-no-pie"]

    compilation_outputs = [
        cc_common.create_compilation_outputs(
            # This file contains the SBCL core, essentially as '.data' in the
            # object file, so it can be linked as PIC or not. For the other
            # dependencies, we still want the link action to choose normally
            # between PIC and non-PIC outputs.
            objects = depset([core_object_file]),
            pic_objects = depset([core_object_file]),
        ),
    ]
    if not _is_arm_cpu(ctx) and not ctx.attr.allow_save_lisp:
        # Produce a '.s' file holding only compiled Lisp code and a
        # '-core.o' containing the balance of the original Lisp spaces.
        assembly_file = ctx.actions.declare_file(name + ".s")
        elfinate_outs = [assembly_file, core_object_file]

        # The .s file will get re-assembled before it's linked into the binary.
        # Note that this cc_common.compile action is declared before the
        # action below which runs elfinate to create this input. The elfinate
        # action still ends up first when the graph of actions is computed.
        compilation_context, asm_compilation_output = cc_common.compile(
            name = name,
            actions = ctx.actions,
            feature_configuration = feature_configuration,
            cc_toolchain = cc_toolchain,
            srcs = [assembly_file],
        )
        compilation_outputs.append(asm_compilation_output)
        ctx.actions.run_shell(
            outputs = elfinate_outs,
            tools = [ctx.executable._elfinate],
            inputs = [core],
            command = ctx.executable._elfinate.path + " " + core.path + " " + assembly_file.path,
            progress_message = "Elfinating Lisp core %{output}",
            mnemonic = "LispElfinate",
            toolchain = None,
        )

    runtimes_ccinfos = []

    # The rule's malloc attribute can be overridden by the --custom_malloc flag.
    malloc = ctx.attr._custom_malloc or ctx.attr._malloc_dontuse
    linking_outputs = cc_common.link(
        name = name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        user_link_flags = linkopts,
        # compilation_outpus contains all the Lisp code. If allow_save_lisp,
        # it's all in the -core.o file. Otherwise, the compiled Lisp code was
        # disassembled and reassembled, and this contains the output from that
        # plus the remainder of the Lisp core in the -core.o file.
        compilation_outputs = cc_common.merge_compilation_outputs(
            compilation_outputs = compilation_outputs,
        ),
        # linking_contexts contains all the C++ code to be linked in.
        linking_contexts = [
            # C++ code from transitive dependencies.
            lisp_info.cc_info.linking_context,
            # SBCL's C++ dependencies.
            ctx.attr.runtime[CcInfo].linking_context,
            # A custom malloc library gets linked in like any other library.
            # It's important that each binary gets a single malloc
            # implementation, so this does not get propagated to any of the
            # binary's consumers.
            malloc[CcInfo].linking_context,
        ] + [info.linking_context for info in runtimes_ccinfos],
        stamp = ctx.attr.stamp,
        output_type = "executable",
    )

    return _lisp_providers(
        ctx = ctx,
        lisp_info = lisp_info,
        fasl = compile.output_fasl,
        executable = linking_outputs.executable,
    )

lisp_binary = rule(
    implementation = _lisp_binary_impl,
    executable = True,
    exec_groups = {"cpp_link": exec_group()},
    attrs = _LISP_BINARY_ATTRS,
    fragments = ["cpp"],
    toolchains = use_cc_toolchain(),
    doc = """
Supports all of the same attributes as [`lisp_library`](#lisp_library), plus
additional attributes governing the behavior of the completed binary. The
[`main`](#lisp_binary-main) attribute defines behavior (generally specifying a
function to run with no arguments) when the binary is started. By default, it
runs `(cl-user::main)`.

Example:

    lisp_binary(
        name = "binary"
        srcs = ["binary.lisp"],
        main = "binary:main",
        deps = [":library"],
    )""",
)

def lisp_test(name, **kwargs):
    """Macro wrapper on lisp_test_rule appending an extra tag

    Args:
        name: Rule name.
        **kwargs: Passed through to lisp_binary"""

    tags = kwargs.pop("tags", [])

    moretags = ["notsan"]
    alltags = tags + [x for x in moretags if x not in tags]
    _lisp_test(name = name, tags = alltags, **kwargs)

_lisp_test = rule(
    implementation = _lisp_binary_impl,
    executable = True,
    test = True,
    attrs = _LISP_TEST_ATTRS,
    fragments = ["cpp"],
    toolchains = use_cc_toolchain(),
    doc = """
Like [`lisp_binary`](#lisp_binary), for defining tests to be run with the
[`test`](https://docs.bazel.build/versions/master/user-manual.html#test)
command. The [`main`](#lisp_test-main) attribute should name a function which
runs the tests, outputs information about failing assertions, and exits with a
non-zero exit status if there are any failures.

Example:

    lisp_test(
        name = "library-test"
        srcs = ["library-test.lisp"],
        main = "library-test:run-tests",
        deps = [
            ":library",
            "//path/to/unit-test:framework",
        ],
    )""",
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

    cc_toolchain = find_cc_toolchain(ctx)

    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features + ["module_maps"],
    )

    c_compile_variables = cc_common.create_compile_variables(
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        user_compile_flags = ctx.fragments.cpp.copts + ctx.fragments.cpp.conlyopts,
        source_file = "dummyfile.c",
    )

    # We don't really need a complete C++ command-line, but I do not know how to detect
    # presence of -D{thingy} in the opaque object that eventually becomes the command-line.
    # So the technique of materializing the compiler invocation as a string list then
    # searching for the flag of interest is perfectly adequate albeit brain-dead.
    #
    # And another problem: when I attempt to do this load() to use an abstract constant
    # as you're "supposed to"
    #  load("@rules_cc//cc:action_names.bzl", "CPP_COMPILE_ACTION_NAME")
    # then all tests in //lisp/devtools/bazel/macro-tests fail. Gimme a friggin break for once.
    command_line = cc_common.get_memory_inefficient_command_line(
        feature_configuration = feature_configuration,
        action_name = "c++-compile",  # should be CPP_COMPILE_ACTION_NAME ?
        variables = c_compile_variables,
    )
    if "-D_LIBCPP_GOOGLE3_ENABLE_SIZE_BASED_VECTOR" in command_line:
        augment_lisp_features = ["size-based-stdvector"] + ctx.attr.add_features
    else:
        augment_lisp_features = ctx.attr.add_features
    if "-DADDRESS_SANITIZER" in command_line or "-DHWADDRESS_SANITIZER" in command_line:
        augment_lisp_features = ["address-sanitizer"] + augment_lisp_features
    if "-DNDEBUG" in command_line and not "-UNDEBUG" in command_line:
        augment_lisp_features = ["copt-ndebug"] + augment_lisp_features

    compile = lisp_compile_srcs(
        ctx = ctx,
        srcs = ctx.files.srcs,
        deps = ctx.attr.deps,
        cdeps = ctx.attr.cdeps,
        block_compile = ctx.attr.block_compile,
        block_compile_specified_only = ctx.attr.block_compile_specified_only,
        image = ctx.attr.image,
        add_features = augment_lisp_features,
        nowarn = ctx.attr.nowarn,
        compile_data = ctx.attr.compile_data,
        verbose_level = verbose_level,
        instrument_coverage = ctx.attr.instrument_coverage,
    )

    return _lisp_providers(
        ctx = ctx,
        lisp_info = compile.lisp_info,
        fasl = compile.output_fasl,
    )

lisp_library = rule(
    implementation = _lisp_library_impl,
    attrs = _LISP_LIBRARY_ATTRS,
    fragments = ["cpp"],
    toolchains = use_cc_toolchain(),
    doc = """
The basic compilation unit for Lisp code. Can have Lisp dependencies
([`deps`](#lisp_library-deps)) and C/C++ dependencies
([`cdeps`](#lisp_library-cdeps)).

Example:

    lisp_test(
        name = "library"
        srcs = ["library.lisp"],
        cdeps = [":cc-dependency-ci"],
        deps = [":dependency"],
    )""",
)
