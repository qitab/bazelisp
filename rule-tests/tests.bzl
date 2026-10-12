"""Tests for the underlying Lisp Starlark buildrules.

Tests for the macros wrapping these rules are in
google3/lisp/devtools/bazel/macro-tests/.
"""

load("@bazel_skylib//lib:unittest.bzl", "analysistest", "asserts")
load("//:provider.bzl", "LispInfo")

# An argument pair that looks like --dynamic-space-size <anything>
# is changed to this constant value so we don't have to update the
# tests just because an invocation of SBCL wants more memory.
DYNSPACE = '"--dynamic-space-size", "nGB"'

def create_empty_files(names):  # buildozer: disable=unnamed-macro
    for name in names:
        native.genrule(
            name = "gen_empty_" + name,
            outs = [name],
            cmd = "touch $@",
            testonly = 1,
            visibility = ["//visibility:private"],
            output_to_bindir = True,
        )

def _empty_output(ctx, suffix):
    output = ctx.actions.declare_file(ctx.label.name + suffix)
    ctx.actions.write(output, "")
    return output

def _fake_lisp_rule_impl(ctx):
    # If this rule is used as the image of a Lisp rule, it needs to output
    # something that can be used as a compile image.
    ctx.actions.run_shell(
        mnemonic = "FakeLispRule",
        tools = [ctx.file._default_lisp_image],
        outputs = [ctx.outputs.executable],
        command = "mv {} {}".format(
            ctx.file._default_lisp_image.path,
            ctx.outputs.executable.path,
        ),
    )
    fake_src_file = ctx.file.src

    # The executable will end up in runfiles by default, and that's enough for us to check
    # that's getting propagated. For the rest of these, generate some content that lets
    # us verify these are getting propagated.
    return [
        LispInfo(
            fasls = depset([_empty_output(ctx, ".fasl")]),
            srcs = depset([fake_src_file]),
            features = depset([ctx.label.name + "-feature"]),
            compile_data = depset([_empty_output(ctx, ".compile-data")]),
            cc_info = CcInfo(),
        ),
        coverage_common.instrumented_files_info(
            ctx = ctx,
            source_attributes = ["src"],
        ),
    ]

fake_lisp_rule = rule(
    implementation = _fake_lisp_rule_impl,
    attrs = {
        "src": attr.label(allow_single_file = [".lisp"]),
        "_default_lisp_image": attr.label(
            default = "//:test-image",
            executable = True,
            cfg = "target",
            allow_single_file = True,
        ),
    },
    executable = True,
)

def _lisp_providers_test_impl(ctx):
    """Asserts the contents of providers returned by lisp_* are as expected."""
    env = analysistest.begin(ctx)
    target_under_test = analysistest.target_under_test(env)
    default_info = target_under_test[DefaultInfo]
    executable = default_info.files_to_run.executable
    lisp_info = target_under_test[LispInfo]

    # Executable should be output for binary/test targets.
    if ctx.attr.executable:
        asserts.equals(env, ctx.attr.executable, executable.basename)
    else:
        asserts.equals(env, None, executable)

    # Outputs should be the combined .fasl for a lisp_library target,
    # and the executable for a binary/test target.
    outputs = sorted([f.basename for f in default_info.files.to_list()])
    asserts.equals(env, [ctx.attr.output], outputs)

    # Runfiles collects runfiles transitively from srcs/deps/data/compile_data.
    runfiles = sorted([f.basename for f in default_info.default_runfiles.files.to_list()])
    asserts.equals(env, sorted(ctx.attr.runfiles), runfiles)

    # This isn't asserting on the specific order and structure of these
    # depsets. If we need to carefully preserve graph order for some reason,
    # we'll need to have more precise asserts and fake_lisp_rule will need to
    # distinguish more carefully what outputs it's pretending come from its
    # dependencies.

    # LispInfo.fasls contains a single combined fasl for each target in the
    # dependencies.
    fasls = sorted([f.basename for f in lisp_info.fasls.to_list()])
    asserts.equals(env, sorted(ctx.attr.fasls), fasls)

    # LispInfo.srcs contains compilation outputs for each
    # source file in the transitive dependencies.
    srcs = sorted([f.basename for f in lisp_info.srcs.to_list()])
    expected_srcs = sorted(ctx.attr.src_outputs_for)
    expected_src_stems = [src.rsplit(".", 1)[0] for src in expected_srcs]
    asserts.equals(env, expected_srcs, srcs)

    # LispInfo.features collects features (go/clhs/*features*) provided by
    # transitive dependencies.
    lisp_features = [
        x
        for x in lisp_info.features.to_list()
        if (x != "size-based-stdvector" and x != "copt-ndebug")
    ]
    asserts.equals(env, sorted(ctx.attr.lisp_features), sorted(lisp_features))

    return analysistest.end(env)

lisp_providers_test = analysistest.make(
    impl = _lisp_providers_test_impl,
    attrs = {
        "executable": attr.string(),
        "output": attr.string(),
        "fasls": attr.string_list(),
        "src_outputs_for": attr.string_list(),
        "lisp_features": attr.string_list(),
        "compile_data": attr.string_list(),
        "runfiles": attr.string_list(),
    },
)

def _abbreviate_paths(ctx, env, s):
    bin_path = analysistest.target_bin_dir_path(env)
    return s.replace(bin_path, "bin").replace(ctx.label.package, "package")

def _lisp_instrumented_files_info_test_impl(ctx):
    """Asserts InstrumentedFilesInfo returned by lisp_* is as expected."""
    env = analysistest.begin(ctx)
    target_under_test = analysistest.target_under_test(env)
    instrumented_files_info = target_under_test[InstrumentedFilesInfo]

    instrumented_files = [
        _abbreviate_paths(ctx, env, f.path)
        for f in instrumented_files_info.instrumented_files.to_list()
    ]
    asserts.equals(
        env,
        sorted(ctx.attr.instrumented_files),
        sorted(instrumented_files),
    )

    return analysistest.end(env)

lisp_instrumented_files_info_test = analysistest.make(
    impl = _lisp_instrumented_files_info_test_impl,
    attrs = {
        "instrumented_files": attr.string_list(),
    },
    config_settings = {
        "//command_line_option:collect_code_coverage": "1",
        "//command_line_option:instrument_test_targets": "1",
        "//command_line_option:instrumentation_filter": "//rule-tests[:/]",
    },
)

def _command(ctx, env, argv):
    argv = list(argv)
    if argv[0].endswith("/bash"):
        argv[0] = "bash"
    return [_abbreviate_paths(ctx, env, arg) for arg in argv]

def _remove_size_based_stdvector(string):
    strings = [x for x in string.split(" ") if x != "size-based-stdvector"]
    return " ".join(strings)

def _lisp_actions_test_impl(ctx):
    """Asserts the command lines generated for run/run_shell actions for lisp_* are as expected."""
    env = analysistest.begin(ctx)
    commands = []
    for a in analysistest.target_actions(env):
        if a.argv and a.mnemonic.startswith("Lisp"):
            # argv is immutable. I looked for how to mutate it.
            # YAQS says "A workaround is to make a copy of the list before appending to it."
            # but then doesn't tell you how to copy. copy() would be the Python way
            # however this isn't quite exactly Python and so copy doesn't work.
            copy_of_argv = []
            copy_of_argv.extend(a.argv)

            # now mutate the copy
            for i in range(len(copy_of_argv)):
                if copy_of_argv[i] == "--dynamic-space-size":
                    copy_of_argv[1 + i] = "nGB"  # dummy value
                if copy_of_argv[i] == "--features":
                    copy_of_argv[1 + i] = _remove_size_based_stdvector(copy_of_argv[1 + i])
            commands.append("{}: {}".format(a.mnemonic, _command(ctx, env, copy_of_argv)))
    if len(ctx.attr.commands) == len(commands):
        for expected_command, actual_command in zip(ctx.attr.commands, commands):
            asserts.equals(env, expected_command, actual_command)
    else:
        asserts.equals(env, ctx.attr.commands, commands)
    return analysistest.end(env)

def lisp_actions_test_rule(compilation_mode):
    return analysistest.make(
        impl = _lisp_actions_test_impl,
        attrs = {
            "commands": attr.string_list(),
        },
        config_settings = {
            "//command_line_option:compilation_mode": compilation_mode,
            "//command_line_option:collect_code_coverage": "0",
        },
    )

_lisp_actions_test = lisp_actions_test_rule("fastbuild")

_lisp_actions_dbg_test = lisp_actions_test_rule("dbg")

_lisp_actions_opt_test = lisp_actions_test_rule("opt")

def with_nomsan(rule_fn, name, **kwargs):
    # Add 'nomsan' to the tags for these tests automatically. The actions
    # generated for the Lisp rules differ a bit for msan. It would be nice to
    # specify msan behavior with a config transition (similar to what's done
    # with compilation_mode above), but that's not possible because msan
    # configuration uses --define instead of more specific flags
    # (see b/129478178 and http://google3/tools/cpp/BUILD?l=184&rcl=270817502).
    tags = kwargs.pop("tags", [])
    if not "nomsan" in tags:
        tags.append("nomsan")
    rule_fn(name = name, tags = tags, **kwargs)

def lisp_actions_test(name, **kwargs):
    with_nomsan(_lisp_actions_test, name, **kwargs)

def lisp_actions_dbg_test(name, **kwargs):
    with_nomsan(_lisp_actions_dbg_test, name, **kwargs)

def lisp_actions_opt_test(name, **kwargs):
    with_nomsan(_lisp_actions_opt_test, name, **kwargs)

def _has_action_test_impl(ctx):
    env = analysistest.begin(ctx)
    for a in analysistest.target_actions(env):
        if a.argv and a.mnemonic == ctx.attr.mnemonic:
            command = _command(ctx, env, a.argv)
            asserts.true(
                env,
                any([ctx.attr.command_contains in arg for arg in command]),
                "{} was not in command: {}".format(
                    ctx.attr.command_contains,
                    command,
                ),
            )
            return analysistest.end(env)
    analysistest.fail(env, "Could not find {} action".format(ctx.attr.mnemonic))
    return analysistest.end(env)

has_action_test = analysistest.make(
    impl = _has_action_test_impl,
    attrs = {
        "mnemonic": attr.string(mandatory = True),
        "command_contains": attr.string(default = ""),
    },
    config_settings = {
        "//command_line_option:compilation_mode": "fastbuild",
        "//command_line_option:collect_code_coverage": "0",
    },
)
