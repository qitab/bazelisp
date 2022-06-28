load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load(
    ":tests.bzl",
    "DYNSPACE",
    "create_empty_files",
    "fake_lisp_rule",
    "has_action_test",
    "lisp_actions_dbg_test",
    "lisp_actions_opt_test",
    "lisp_actions_test",
    "lisp_deps_analysis_test",
    "lisp_instrumented_files_info_test",
    "lisp_providers_test",
)
load(
    "//:rules.bzl",
    "lisp_binary",
    "lisp_library",
    "lisp_test",
)

licenses(["notice"])

# This can create some of the files needed for these tests, but the tests for
# coverage instrumentation behavior need actual empty source files, since
# the logic which populates InstrumentedFilesInfo.instrumented_files
# (InstrumentedFilesCollector.collect) excludes output files.
create_empty_files(names = [
    "linkstamp.cc",
    "malloc.cc",
    "binary-allow-save-lisp.lisp",
    "library-serial-1.lisp",
    "library-serial-2.lisp",
    "library-parallel-1.lisp",
    "library-parallel-2.lisp",
    "library-multipass-1.lisp",
    "library-multipass-2.lisp",
    "library-instrument-coverage.lisp",
    "binary-instrument-coverage.lisp",
    "binary-custom-malloc.lisp",
])

fake_lisp_rule(
    name = "data",
    testonly = 1,
    src = "data.lisp",
)

fake_lisp_rule(
    name = "compile-data",
    testonly = 1,
    src = "compile-data.lisp",
)

fake_lisp_rule(
    name = "image",
    testonly = 1,
    src = "image.lisp",
)

fake_lisp_rule(
    name = "dep",
    testonly = 1,
    src = "dep.lisp",
)

cc_library(
    name = "cdeps",
    srcs = ["csrc.cc"],
    linkstamp = "linkstamp.cc",
)

cc_library(
    name = "custom-malloc",
    srcs = ["malloc.cc"],
)

lisp_binary(
    name = "binary",
    testonly = 1,
    srcs = ["binary.lisp"],
    add_features = ["binary-feature"],
    cdeps = [":cdeps"],
    compile_data = [":compile-data"],
    data = [":data"],
    image = ":image",
    main = "values",
    deps = [":dep"],
)

lisp_binary(
    name = "binary-allow-save-lisp",
    testonly = 1,
    srcs = ["binary-allow-save-lisp.lisp"],
    add_features = ["binary-feature"],
    allow_save_lisp = True,
    cdeps = [":cdeps"],
    compile_data = [":compile-data"],
    data = [":data"],
    image = ":image",
    main = "values",
    deps = [":dep"],
)

lisp_test(
    name = "test",
    srcs = ["test.lisp"],
    add_features = ["test-feature"],
    cdeps = [":cdeps"],
    compile_data = [":compile-data"],
    data = [":data"],
    image = ":image",
    main = "values",
    deps = [":dep"],
)

lisp_library(
    name = "library",
    testonly = 1,
    srcs = ["library.lisp"],
    add_features = ["library-feature"],
    cdeps = [":cdeps"],
    compile_data = [":compile-data"],
    data = [":data"],
    image = ":image",
    deps = [":dep"],
)

lisp_library(
    name = "empty-library",
    testonly = 1,
    image = ":image",
)

# To summarize the behavior:
# * Most things are from self, deps, image.
# * Runfiles is also from data, compile_data.
# * compile_data is also from compile_data

lisp_providers_test(
    name = "binary-lisp-providers-test",
    executable = "binary",
    fasls = [
        "binary.fasl",
        "dep.fasl",
        "image.fasl",
    ],
    lisp_features = [
        "dep-feature",
        "binary-feature",
        "image-feature",
    ],
    output = "binary",
    runfiles = [
        "binary",
        "data",
        "dep",
        "image",
    ],
    src_outputs_for = [
        "binary.lisp",
        "dep.lisp",
        "image.lisp",
    ],
    target_under_test = ":binary",
)

lisp_deps_analysis_test(
    name = "binary-lisp-deps-test",
    expected_content = (
        "feature: dep-feature\n" +
        "feature: image-feature\n" +
        "feature: binary-feature\n" +
        "src: package/dep.lisp\n" +
        "src: package/image.lisp\n" +
        "src: package/binary.lisp\n"
    ),
    expected_out = "binary.deps",
    target_under_test = ":binary",
)

lisp_instrumented_files_info_test(
    name = "binary-instrumented-files-test",
    instrumented_files = [
        "package/binary.lisp",
        "package/csrc.cc",
        "package/data.lisp",
        "package/dep.lisp",
        "package/image.lisp",
    ],
    target_under_test = ":binary",
)

lisp_providers_test(
    name = "test-lisp-providers-test",
    executable = "test",
    fasls = [
        "test.fasl",
        "dep.fasl",
        "image.fasl",
    ],
    lisp_features = [
        "dep-feature",
        "test-feature",
        "image-feature",
    ],
    output = "test",
    runfiles = [
        "test",
        "data",
        "dep",
        "image",
    ],
    src_outputs_for = [
        "test.lisp",
        "dep.lisp",
        "image.lisp",
    ],
    target_under_test = ":test",
)

lisp_deps_analysis_test(
    name = "test-lisp-deps-test",
    expected_content = (
        "feature: dep-feature\n" +
        "feature: image-feature\n" +
        "feature: test-feature\n" +
        "src: package/dep.lisp\n" +
        "src: package/image.lisp\n" +
        "src: package/test.lisp\n"
    ),
    expected_out = "test.deps",
    target_under_test = ":test",
)

lisp_instrumented_files_info_test(
    name = "test-instrumented-files-test",
    instrumented_files = [
        "package/test.lisp",
        "package/csrc.cc",
        "package/data.lisp",
        "package/dep.lisp",
        "package/image.lisp",
    ],
    target_under_test = ":test",
)

lisp_providers_test(
    name = "library-lisp-providers-test",
    fasls = [
        "library.fasl",
        "dep.fasl",
        "image.fasl",
    ],
    lisp_features = [
        "dep-feature",
        "library-feature",
        "image-feature",
    ],
    output = "library.fasl",
    runfiles = [
        "data",
        "dep",
        "image",
    ],
    src_outputs_for = [
        "library.lisp",
        "dep.lisp",
        "image.lisp",
    ],
    target_under_test = ":library",
)

lisp_deps_analysis_test(
    name = "library-lisp-deps-test",
    expected_content = (
        "feature: dep-feature\n" +
        "feature: image-feature\n" +
        "feature: library-feature\n" +
        "src: package/dep.lisp\n" +
        "src: package/image.lisp\n" +
        "src: package/library.lisp\n"
    ),
    expected_out = "library.deps",
    target_under_test = ":library",
)

lisp_instrumented_files_info_test(
    name = "library-instrumented-files-test",
    instrumented_files = [
        "package/library.lisp",
        "package/csrc.cc",
        "package/data.lisp",
        "package/dep.lisp",
        "package/image.lisp",
    ],
    target_under_test = ":library",
)

# These commands are abbreviated in several ways:
# * The full path to bash is repalced with "bash"
# * The bin path is replaced with "bin"
# * The path to this package is replaced with "package"

# Note the lisp_binary rule being tested here has the same implementation as
# lisp_test.
lisp_actions_test(
    name = "lisp-binary-one-file-actions-test",
    commands = [
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature ' +
         'binary-feature", ' +
         # The FASL is under the target name instead of src path if thre's
         # a single src.
         '"--outs", "bin/package/binary.fasl ' +
         "bin/package/binary~/package/binary.hash " +
         'bin/package/binary~/binary.warnings", ' +
         '"--srcs", "package/binary.lisp", ' +
         '"--deps", "package/dep.lisp"]'),
        ("LispCore: " +
         '["bin/package/image", "core", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature ' +
         'binary-feature", ' +
         '"--specs", "bin/package/binary.specs", ' +
         '"--outs", "bin/package/binary.core", ' +
         '"--main", "values", "--precompile-generics", ' +
         '"--save-runtime-options"]'),
        ("LispElfinate: " +
         '["bash", "-c", "$1 split $2 $3", "", ' +
         '"bin/elfinate", ' +
         '"bin/package/binary.core", ' +
         '"bin/package/binary.s"]'),
    ],
    target_under_test = ":binary",
)

lisp_actions_test(
    name = "lisp-binary-allow-save-lisp-actions-test",
    commands = [
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature ' +
         'binary-feature", ' +
         # The FASL is under the target name instead of src path if thre's
         # a single src.
         '"--outs", "bin/package/binary-allow-save-lisp.fasl ' +
         "bin/package/binary-allow-save-lisp~/package/binary-allow-save-lisp.hash " +
         'bin/package/binary-allow-save-lisp~/binary-allow-save-lisp.warnings", ' +
         '"--srcs", "bin/package/binary-allow-save-lisp.lisp", ' +
         '"--deps", "package/dep.lisp"]'),
        ("LispCore: " +
         '["bin/package/image", "core", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature ' +
         'binary-feature", ' +
         '"--specs", "bin/package/binary-allow-save-lisp.specs", ' +
         '"--outs", "bin/package/binary-allow-save-lisp.core", ' +
         '"--main", "values", "--precompile-generics", "--save-runtime-options"]'),
        ("LispElfinate: " +
         '["bash", "-c", "$1 copy $2 $3 && nm -p $3 | ' +
         'awk \'{print $2\\";\\"}BEGIN{print \\"{\\"}END{print \\"};\\"}\' > $4", "", ' +
         '"bin/elfinate", ' +
         '"bin/package/binary-allow-save-lisp.core", ' +
         '"bin/package/binary-allow-save-lisp-core.o", ' +
         '"bin/package/binary-allow-save-lisp-syms.lds"]'),
    ],
    target_under_test = ":binary-allow-save-lisp",
)

lisp_actions_test(
    name = "lisp-library-one-file-actions-test",
    commands = [
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature ' +
         'library-feature", ' +
         # The FASL is under the target name instead of src path if thre's
         # a single src.
         '"--outs", "bin/package/library.fasl ' +
         "bin/package/library~/package/library.hash " +
         'bin/package/library~/library.warnings", ' +
         '"--srcs", "package/library.lisp", ' +
         '"--deps", "package/dep.lisp"]'),
    ],
    target_under_test = ":library",
)

lisp_actions_dbg_test(
    name = "lisp-library-one-file-dbg-actions-test",
    commands = [
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "dbg", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature ' +
         'library-feature", ' +
         '"--outs", "bin/package/library.fasl ' +
         "bin/package/library~/package/library.hash " +
         'bin/package/library~/library.warnings", ' +
         '"--srcs", "package/library.lisp", ' +
         '"--deps", "package/dep.lisp"]'),
    ],
    target_under_test = ":library",
)

lisp_actions_opt_test(
    name = "lisp-library-one-file-opt-actions-test",
    commands = [
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "opt", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature ' +
         'library-feature", ' +
         '"--outs", "bin/package/library.fasl ' +
         "bin/package/library~/package/library.hash " +
         'bin/package/library~/library.warnings", ' +
         '"--srcs", "package/library.lisp", ' +
         '"--deps", "package/dep.lisp"]'),
    ],
    target_under_test = ":library",
)

lisp_actions_test(
    name = "lisp-library-zero-files-actions-test",
    # With no srcs, there are no LispCompile or LispConcatFASLs actions generated.
    commands = [],
    target_under_test = ":empty-library",
)

# More tests for different compilation structures. Testing these with just lisp_library,
# since the implementation of the relevant details is shared.

lisp_library(
    name = "library-serial",
    testonly = 1,
    srcs = [
        "library-serial-1.lisp",
        "library-serial-2.lisp",
    ],
    image = ":image",
    deps = [":dep"],
)

lisp_library(
    name = "library-parallel",
    testonly = 1,
    srcs = [
        "library-parallel-1.lisp",
        "library-parallel-2.lisp",
    ],
    image = ":image",
    order = "parallel",
    deps = [":dep"],
)

lisp_library(
    name = "library-multipass",
    testonly = 1,
    srcs = [
        "library-multipass-1.lisp",
        "library-multipass-2.lisp",
    ],
    image = ":image",
    order = "multipass",
    deps = [":dep"],
)

lisp_actions_test(
    name = "lisp-library-serial-actions-test",
    commands = [
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature", "--outs", ' +
         '"bin/package/library-serial~/package/library-serial-1.fasl ' +
         "bin/package/library-serial~/package/library-serial-1.hash " +
         'bin/package/library-serial~/package/library-serial-1.warnings", ' +
         '"--srcs", "bin/package/library-serial-1.lisp", ' +
         '"--deps", "package/dep.lisp"]'),
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature", ' +
         '"--outs", "bin/package/library-serial~/package/library-serial-2.fasl ' +
         "bin/package/library-serial~/package/library-serial-2.hash " +
         'bin/package/library-serial~/package/library-serial-2.warnings", ' +
         '"--srcs", "bin/package/library-serial-2.lisp", ' +
         '"--deps", "package/dep.lisp", ' +
         # Serial, so for the second compile we load the first file.
         '"--load", "bin/package/library-serial-1.lisp"]'),
        ("LispConcatFASLs: " +
         '["bash", "-c", "cat ${@:2} > $1", "", ' +
         '"bin/package/library-serial.fasl", ' +
         '"bin/package/library-serial~/package/library-serial-1.fasl", ' +
         '"bin/package/library-serial~/package/library-serial-2.fasl"]'),
    ],
    target_under_test = ":library-serial",
)

lisp_actions_test(
    name = "lisp-library-parallel-actions-test",
    commands = [
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature", "--outs", ' +
         '"bin/package/library-parallel~/package/library-parallel-1.fasl ' +
         "bin/package/library-parallel~/package/library-parallel-1.hash " +
         'bin/package/library-parallel~/package/library-parallel-1.warnings", ' +
         '"--srcs", "bin/package/library-parallel-1.lisp", ' +
         '"--deps", "package/dep.lisp"]'),
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature", ' +
         '"--outs", "bin/package/library-parallel~/package/library-parallel-2.fasl ' +
         "bin/package/library-parallel~/package/library-parallel-2.hash " +
         'bin/package/library-parallel~/package/library-parallel-2.warnings", ' +
         '"--srcs", "bin/package/library-parallel-2.lisp", ' +
         '"--deps", "package/dep.lisp"]'),
        ("LispConcatFASLs: " +
         '["bash", "-c", "cat ${@:2} > $1", "", ' +
         '"bin/package/library-parallel.fasl", ' +
         '"bin/package/library-parallel~/package/library-parallel-1.fasl", ' +
         '"bin/package/library-parallel~/package/library-parallel-2.fasl"]'),
    ],
    target_under_test = ":library-parallel",
)

lisp_actions_test(
    name = "lisp-library-multipass-actions-test",
    commands = [
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature", "--outs", ' +
         '"bin/package/library-multipass~/package/library-multipass-1.fasl ' +
         "bin/package/library-multipass~/package/library-multipass-1.hash " +
         'bin/package/library-multipass~/package/library-multipass-1.warnings", ' +
         '"--srcs", "bin/package/library-multipass-1.lisp", ' +
         '"--deps", "package/dep.lisp", ' +
         # Multipass both source files get loaded before compilation.
         '"--load", "bin/package/library-multipass-1.lisp ' +
         'bin/package/library-multipass-2.lisp", ' +
         # And some now-unavoidable warnings get suppressed.
         '"--nowarn", "redefined-method redefined-function"]'),
        ("LispCompile: " +
         '["bin/package/image", ' + DYNSPACE + ', "compile", "--compilation-mode", "fastbuild", ' +
         '"--bindir", "bin", "--features", "dep-feature image-feature", ' +
         '"--outs", "bin/package/library-multipass~/package/library-multipass-2.fasl ' +
         "bin/package/library-multipass~/package/library-multipass-2.hash " +
         'bin/package/library-multipass~/package/library-multipass-2.warnings", ' +
         '"--srcs", "bin/package/library-multipass-2.lisp", ' +
         '"--deps", "package/dep.lisp", ' +
         '"--load", "bin/package/library-multipass-1.lisp ' +
         'bin/package/library-multipass-2.lisp", ' +
         '"--nowarn", "redefined-method redefined-function"]'),
        ("LispConcatFASLs: " +
         '["bash", "-c", "cat ${@:2} > $1", "", ' +
         '"bin/package/library-multipass.fasl", ' +
         '"bin/package/library-multipass~/package/library-multipass-1.fasl", ' +
         '"bin/package/library-multipass~/package/library-multipass-2.fasl"]'),
    ],
    target_under_test = ":library-multipass",
)

# Testing the effect of individual attrs:

lisp_library(
    name = "library-instrument-coverage",
    testonly = 1,
    srcs = ["library-instrument-coverage.lisp"],
    image = ":image",
    instrument_coverage = 1,
)

has_action_test(
    name = "library-instrument-coverage-test",
    command_contains = "--coverage",
    mnemonic = "LispCompile",
    target_under_test = ":library-instrument-coverage",
)

lisp_binary(
    name = "binary-instrument-coverage",
    testonly = 1,
    srcs = ["binary-instrument-coverage.lisp"],
    image = ":image",
    instrument_coverage = 1,
    main = "nil",
)

has_action_test(
    name = "binary-instrument-coverage-test",
    command_contains = "--coverage",
    mnemonic = "LispCompile",
    target_under_test = ":binary-instrument-coverage",
)

lisp_binary(
    name = "binary-custom-malloc",
    testonly = 1,
    srcs = ["binary-custom-malloc.lisp"],
    image = ":image",
    main = "nil",
    malloc = ":custom-malloc",
)

has_action_test(
    name = "binary-custom-malloc-test",
    command_contains = "bin/package/_objs/custom-malloc/malloc.pic.o",
    mnemonic = "CppLink",
    target_under_test = ":binary-custom-malloc",
)

# Would be nice to have tests that demonstrate stamp is working correctly, but
# I don't see where that shows up in the CppLink command-line, and the argv for
# the CppLinkstampCompile action is not exposed to Starlark.

bzl_library(
    name = "tests_bzl",
    srcs = ["tests.bzl"],
    deps = [
        "//:build_rules",
        "@bazel_skylib//lib:unittest",
    ],
)
