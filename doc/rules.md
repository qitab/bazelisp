<!-- mdformat off(generated file) -->
<!-- Generated with Stardoc: http://skydoc.bazel.build -->

# Bazelisp - Common Lisp Build Rules for Bazel

<a id="lisp_binary"></a>

## lisp_binary

<pre>
lisp_binary(<a href="#lisp_binary-name">name</a>, <a href="#lisp_binary-deps">deps</a>, <a href="#lisp_binary-srcs">srcs</a>, <a href="#lisp_binary-data">data</a>, <a href="#lisp_binary-add_features">add_features</a>, <a href="#lisp_binary-allow_save_lisp">allow_save_lisp</a>, <a href="#lisp_binary-block_compile">block_compile</a>,
            <a href="#lisp_binary-block_compile_specified_only">block_compile_specified_only</a>, <a href="#lisp_binary-cdeps">cdeps</a>, <a href="#lisp_binary-compile_data">compile_data</a>, <a href="#lisp_binary-image">image</a>, <a href="#lisp_binary-instrument_coverage">instrument_coverage</a>, <a href="#lisp_binary-main">main</a>,
            <a href="#lisp_binary-malloc">malloc</a>, <a href="#lisp_binary-nowarn">nowarn</a>, <a href="#lisp_binary-order">order</a>, <a href="#lisp_binary-precompile_generics">precompile_generics</a>, <a href="#lisp_binary-runtime">runtime</a>, <a href="#lisp_binary-save_runtime_options">save_runtime_options</a>, <a href="#lisp_binary-stamp">stamp</a>, <a href="#lisp_binary-verbose">verbose</a>)
</pre>

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
    )

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :------------- | :------------- | :------------- | :------------- | :------------- |
| <a id="lisp_binary-name"></a>name |  A unique name for this target.   | <a href="https://bazel.build/concepts/labels#target-names">Name</a> | required |  |
| <a id="lisp_binary-deps"></a>deps |  Common Lisp dependencies (generally [`lisp_library`](#lisp-library), but you can put [`lisp_binary`](#lisp-binary) in deps for testing).   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_binary-srcs"></a>srcs |  Common Lisp (`.lisp` or `.lsp`) source files. If there are multiple files in `srcs`, which other files in `srcs` are loaded before each file is compiled depends on the `order` attr.   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_binary-data"></a>data |  Data available to this target and its consumers in the runfiles directory at runtime.   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_binary-add_features"></a>add_features |  Names of symbols (by default in the keyword package) to be added to `\*features\*` of this library and its consumers, at compile time and in the resulting binary. Note that this differs from the [`features`](https://docs.bazel.build/versions/master/be/common-definitions.html#common.features) attribute common to all build rules which controls [toolchain](https://docs.bazel.build/versions/master/toolchains.html) features.   | List of strings | optional |  `[]`  |
| <a id="lisp_binary-allow_save_lisp"></a>allow_save_lisp |  Whether to preserve the ability to run `save-lisp-and-die` instead of altering the binary format to be more compatible with C++ debugging tools (which, for example, allows you to get combined stacktraces of C/C++ and Lisp code). Must be `True` for targets used as a compilation image.   | Boolean | optional |  `False`  |
| <a id="lisp_binary-block_compile"></a>block_compile |  Whether to block-compile the sources. By default, this will cause sources to be block-compiled together as a single block, that behavior can be overridden by block_compile_specified_only.   | Boolean | optional |  `False`  |
| <a id="lisp_binary-block_compile_specified_only"></a>block_compile_specified_only |  If true, block compilation only considers multiple top-level forms together if those are between explicit (START-BLOCK) and (END-BLOCK).   | Boolean | optional |  `False`  |
| <a id="lisp_binary-cdeps"></a>cdeps |  C++ dependencies (generally [`cc_library`](https://docs.bazel.build/versions/master/be/c-cpp.html#cc_library)).   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_binary-compile_data"></a>compile_data |  Data available to this target and its consumers at build time, added to the inputs of LispCompile and LispCore actions.   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_binary-image"></a>image |  Lisp binary used as Bazel compilation image. This should be a binary with the main function `#'bazel:main` defined in `main.lisp`.   | <a href="https://bazel.build/concepts/labels">Label</a> | optional |  `"//third_party/lisp/bazel:image"`  |
| <a id="lisp_binary-instrument_coverage"></a>instrument_coverage |  Force coverage instrumentation. Possible values:<br><br>`0`: Never instrument this target. Should be used if thetarget compiles generated source files or does not compilewith coverage instrumentation.<br><br>`1`: Always instrument this target. Generally should not be used outside of tests for the coverage implementation.<br><br>`-1` (default): If coverage data collection is enabled, instrument this target per [`--instrumentation_filter](https://docs.bazel.build/versions/master/command-line-reference.html#flag--instrumentation_filter).`   | Integer | optional |  `-1`  |
| <a id="lisp_binary-main"></a>main |  Name of function (by default in the `cl-user` package) or snippet of Lisp code to run when starting the binary. `"nil"` or `"t"` to start the default REPL. Can be overridden by naming a function (or `nil` or `t`) in the `LISP_MAIN` environment variable.   | String | optional |  `"main"`  |
| <a id="lisp_binary-malloc"></a>malloc |  Target providing a custom malloc implementation. Same as [`cc_binary.malloc`](https://docs.bazel.build/versions/master/be/c-cpp.html#cc_binary.malloc). Note that these rules do not respect [`--custom_malloc`](https://docs.bazel.build/versions/master/command-line-reference.html#flag--custom_malloc).   | <a href="https://bazel.build/concepts/labels">Label</a> | optional |  `"//third_party/tcmalloc"`  |
| <a id="lisp_binary-nowarn"></a>nowarn |  Suppressed Lisp warning types or warning handlers.   | List of strings | optional |  `[]`  |
| <a id="lisp_binary-order"></a>order |  Compilation order, one of:<br><br>`"serial"` (default) - Each source is compiled in an image with previous sources loaded. (Note that in this configuration you should put a comment at the top of the list of srcs if there is more than one, so that formatters like Buildozer do not change the order.)<br><br>`"multipass"` - Each source is compiled in an image with all sources loaded.<br><br>`"parallel"` - Each source is compiled independently.   | String | optional |  `"serial"`  |
| <a id="lisp_binary-precompile_generics"></a>precompile_generics |  If `False`, skip precompiling generic functions.   | Boolean | optional |  `True`  |
| <a id="lisp_binary-runtime"></a>runtime |  SBCL C++ dependencies. Consumers should generally omit this attr and use the default value.   | <a href="https://bazel.build/concepts/labels">Label</a> | optional |  `"//third_party/lisp/sbcl:c-support"`  |
| <a id="lisp_binary-save_runtime_options"></a>save_runtime_options |  If `False`, process SBCL runtime options at the command-line on binary startup.   | Boolean | optional |  `True`  |
| <a id="lisp_binary-stamp"></a>stamp |  Same as [`cc_binary.stamp`](https://docs.bazel.build/versions/master/be/c-cpp.html#cc_binary.stamp).   | Integer | optional |  `-1`  |
| <a id="lisp_binary-verbose"></a>verbose |  Enable verbose debugging output when analyzing and compiling this target (`0` = none (default), `3` = max).   | Integer | optional |  `0`  |


<a id="lisp_library"></a>

## lisp_library

<pre>
lisp_library(<a href="#lisp_library-name">name</a>, <a href="#lisp_library-deps">deps</a>, <a href="#lisp_library-srcs">srcs</a>, <a href="#lisp_library-data">data</a>, <a href="#lisp_library-add_features">add_features</a>, <a href="#lisp_library-block_compile">block_compile</a>, <a href="#lisp_library-block_compile_specified_only">block_compile_specified_only</a>,
             <a href="#lisp_library-cdeps">cdeps</a>, <a href="#lisp_library-compile_data">compile_data</a>, <a href="#lisp_library-image">image</a>, <a href="#lisp_library-instrument_coverage">instrument_coverage</a>, <a href="#lisp_library-nowarn">nowarn</a>, <a href="#lisp_library-order">order</a>, <a href="#lisp_library-verbose">verbose</a>)
</pre>

The basic compilation unit for Lisp code. Can have Lisp dependencies
([`deps`](#lisp_library-deps)) and C/C++ dependencies
([`cdeps`](#lisp_library-cdeps)).

Example:

    lisp_test(
        name = "library"
        srcs = ["library.lisp"],
        cdeps = [":cc-dependency-ci"],
        deps = [":dependency"],
    )

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :------------- | :------------- | :------------- | :------------- | :------------- |
| <a id="lisp_library-name"></a>name |  A unique name for this target.   | <a href="https://bazel.build/concepts/labels#target-names">Name</a> | required |  |
| <a id="lisp_library-deps"></a>deps |  Common Lisp dependencies (generally [`lisp_library`](#lisp-library), but you can put [`lisp_binary`](#lisp-binary) in deps for testing).   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_library-srcs"></a>srcs |  Common Lisp (`.lisp` or `.lsp`) source files. If there are multiple files in `srcs`, which other files in `srcs` are loaded before each file is compiled depends on the `order` attr.   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_library-data"></a>data |  Data available to this target and its consumers in the runfiles directory at runtime.   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_library-add_features"></a>add_features |  Names of symbols (by default in the keyword package) to be added to `\*features\*` of this library and its consumers, at compile time and in the resulting binary. Note that this differs from the [`features`](https://docs.bazel.build/versions/master/be/common-definitions.html#common.features) attribute common to all build rules which controls [toolchain](https://docs.bazel.build/versions/master/toolchains.html) features.   | List of strings | optional |  `[]`  |
| <a id="lisp_library-block_compile"></a>block_compile |  Whether to block-compile the sources. By default, this will cause sources to be block-compiled together as a single block, that behavior can be overridden by block_compile_specified_only.   | Boolean | optional |  `False`  |
| <a id="lisp_library-block_compile_specified_only"></a>block_compile_specified_only |  If true, block compilation only considers multiple top-level forms together if those are between explicit (START-BLOCK) and (END-BLOCK).   | Boolean | optional |  `False`  |
| <a id="lisp_library-cdeps"></a>cdeps |  C++ dependencies (generally [`cc_library`](https://docs.bazel.build/versions/master/be/c-cpp.html#cc_library)).   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_library-compile_data"></a>compile_data |  Data available to this target and its consumers at build time, added to the inputs of LispCompile and LispCore actions.   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_library-image"></a>image |  Lisp binary used as Bazel compilation image. This should be a binary with the main function `#'bazel:main` defined in `main.lisp`.   | <a href="https://bazel.build/concepts/labels">Label</a> | optional |  `"//third_party/lisp/bazel:image"`  |
| <a id="lisp_library-instrument_coverage"></a>instrument_coverage |  Force coverage instrumentation. Possible values:<br><br>`0`: Never instrument this target. Should be used if thetarget compiles generated source files or does not compilewith coverage instrumentation.<br><br>`1`: Always instrument this target. Generally should not be used outside of tests for the coverage implementation.<br><br>`-1` (default): If coverage data collection is enabled, instrument this target per [`--instrumentation_filter](https://docs.bazel.build/versions/master/command-line-reference.html#flag--instrumentation_filter).`   | Integer | optional |  `-1`  |
| <a id="lisp_library-nowarn"></a>nowarn |  Suppressed Lisp warning types or warning handlers.   | List of strings | optional |  `[]`  |
| <a id="lisp_library-order"></a>order |  Compilation order, one of:<br><br>`"serial"` (default) - Each source is compiled in an image with previous sources loaded. (Note that in this configuration you should put a comment at the top of the list of srcs if there is more than one, so that formatters like Buildozer do not change the order.)<br><br>`"multipass"` - Each source is compiled in an image with all sources loaded.<br><br>`"parallel"` - Each source is compiled independently.   | String | optional |  `"serial"`  |
| <a id="lisp_library-verbose"></a>verbose |  Enable verbose debugging output when analyzing and compiling this target (`0` = none (default), `3` = max).   | Integer | optional |  `0`  |


<a id="lisp_test"></a>

## lisp_test

<pre>
lisp_test(<a href="#lisp_test-name">name</a>, <a href="#lisp_test-deps">deps</a>, <a href="#lisp_test-srcs">srcs</a>, <a href="#lisp_test-data">data</a>, <a href="#lisp_test-add_features">add_features</a>, <a href="#lisp_test-allow_save_lisp">allow_save_lisp</a>, <a href="#lisp_test-block_compile">block_compile</a>,
          <a href="#lisp_test-block_compile_specified_only">block_compile_specified_only</a>, <a href="#lisp_test-cdeps">cdeps</a>, <a href="#lisp_test-compile_data">compile_data</a>, <a href="#lisp_test-image">image</a>, <a href="#lisp_test-instrument_coverage">instrument_coverage</a>, <a href="#lisp_test-main">main</a>, <a href="#lisp_test-malloc">malloc</a>,
          <a href="#lisp_test-nowarn">nowarn</a>, <a href="#lisp_test-order">order</a>, <a href="#lisp_test-precompile_generics">precompile_generics</a>, <a href="#lisp_test-runtime">runtime</a>, <a href="#lisp_test-save_runtime_options">save_runtime_options</a>, <a href="#lisp_test-stamp">stamp</a>, <a href="#lisp_test-verbose">verbose</a>)
</pre>

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
    )

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :------------- | :------------- | :------------- | :------------- | :------------- |
| <a id="lisp_test-name"></a>name |  A unique name for this target.   | <a href="https://bazel.build/concepts/labels#target-names">Name</a> | required |  |
| <a id="lisp_test-deps"></a>deps |  Common Lisp dependencies (generally [`lisp_library`](#lisp-library), but you can put [`lisp_binary`](#lisp-binary) in deps for testing).   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_test-srcs"></a>srcs |  Common Lisp (`.lisp` or `.lsp`) source files. If there are multiple files in `srcs`, which other files in `srcs` are loaded before each file is compiled depends on the `order` attr.   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_test-data"></a>data |  Data available to this target and its consumers in the runfiles directory at runtime.   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_test-add_features"></a>add_features |  Names of symbols (by default in the keyword package) to be added to `\*features\*` of this library and its consumers, at compile time and in the resulting binary. Note that this differs from the [`features`](https://docs.bazel.build/versions/master/be/common-definitions.html#common.features) attribute common to all build rules which controls [toolchain](https://docs.bazel.build/versions/master/toolchains.html) features.   | List of strings | optional |  `[]`  |
| <a id="lisp_test-allow_save_lisp"></a>allow_save_lisp |  Whether to preserve the ability to run `save-lisp-and-die` instead of altering the binary format to be more compatible with C++ debugging tools (which, for example, allows you to get combined stacktraces of C/C++ and Lisp code). Must be `True` for targets used as a compilation image.   | Boolean | optional |  `False`  |
| <a id="lisp_test-block_compile"></a>block_compile |  Whether to block-compile the sources. By default, this will cause sources to be block-compiled together as a single block, that behavior can be overridden by block_compile_specified_only.   | Boolean | optional |  `False`  |
| <a id="lisp_test-block_compile_specified_only"></a>block_compile_specified_only |  If true, block compilation only considers multiple top-level forms together if those are between explicit (START-BLOCK) and (END-BLOCK).   | Boolean | optional |  `False`  |
| <a id="lisp_test-cdeps"></a>cdeps |  C++ dependencies (generally [`cc_library`](https://docs.bazel.build/versions/master/be/c-cpp.html#cc_library)).   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_test-compile_data"></a>compile_data |  Data available to this target and its consumers at build time, added to the inputs of LispCompile and LispCore actions.   | <a href="https://bazel.build/concepts/labels">List of labels</a> | optional |  `[]`  |
| <a id="lisp_test-image"></a>image |  Lisp binary used as Bazel compilation image. This should be a binary with the main function `#'bazel:main` defined in `main.lisp`.   | <a href="https://bazel.build/concepts/labels">Label</a> | optional |  `"//third_party/lisp/bazel:image"`  |
| <a id="lisp_test-instrument_coverage"></a>instrument_coverage |  Force coverage instrumentation. Possible values:<br><br>`0`: Never instrument this target. Should be used if thetarget compiles generated source files or does not compilewith coverage instrumentation.<br><br>`1`: Always instrument this target. Generally should not be used outside of tests for the coverage implementation.<br><br>`-1` (default): If coverage data collection is enabled, instrument this target per [`--instrumentation_filter](https://docs.bazel.build/versions/master/command-line-reference.html#flag--instrumentation_filter).`   | Integer | optional |  `-1`  |
| <a id="lisp_test-main"></a>main |  Name of function (by default in the `cl-user` package) or snippet of Lisp code to run when starting the binary. `"nil"` or `"t"` to start the default REPL. Can be overridden by naming a function (or `nil` or `t`) in the `LISP_MAIN` environment variable.   | String | optional |  `"main"`  |
| <a id="lisp_test-malloc"></a>malloc |  Target providing a custom malloc implementation. Same as [`cc_binary.malloc`](https://docs.bazel.build/versions/master/be/c-cpp.html#cc_binary.malloc). Note that these rules do not respect [`--custom_malloc`](https://docs.bazel.build/versions/master/command-line-reference.html#flag--custom_malloc).   | <a href="https://bazel.build/concepts/labels">Label</a> | optional |  `"//third_party/tcmalloc"`  |
| <a id="lisp_test-nowarn"></a>nowarn |  Suppressed Lisp warning types or warning handlers.   | List of strings | optional |  `[]`  |
| <a id="lisp_test-order"></a>order |  Compilation order, one of:<br><br>`"serial"` (default) - Each source is compiled in an image with previous sources loaded. (Note that in this configuration you should put a comment at the top of the list of srcs if there is more than one, so that formatters like Buildozer do not change the order.)<br><br>`"multipass"` - Each source is compiled in an image with all sources loaded.<br><br>`"parallel"` - Each source is compiled independently.   | String | optional |  `"serial"`  |
| <a id="lisp_test-precompile_generics"></a>precompile_generics |  If `False`, skip precompiling generic functions.   | Boolean | optional |  `True`  |
| <a id="lisp_test-runtime"></a>runtime |  SBCL C++ dependencies. Consumers should generally omit this attr and use the default value.   | <a href="https://bazel.build/concepts/labels">Label</a> | optional |  `"//third_party/lisp/sbcl:c-support"`  |
| <a id="lisp_test-save_runtime_options"></a>save_runtime_options |  If `False`, process SBCL runtime options at the command-line on binary startup.   | Boolean | optional |  `True`  |
| <a id="lisp_test-stamp"></a>stamp |  Same as [`cc_test.stamp`](https://docs.bazel.build/versions/master/be/c-cpp.html#cc_test.stamp). Build version stamping is disabled by default.   | Integer | optional |  `0`  |
| <a id="lisp_test-verbose"></a>verbose |  Enable verbose debugging output when analyzing and compiling this target (`0` = none (default), `3` = max).   | Integer | optional |  `0`  |


<a id="lisp_compile_srcs"></a>

## lisp_compile_srcs

<pre>
lisp_compile_srcs(<a href="#lisp_compile_srcs-ctx">ctx</a>, <a href="#lisp_compile_srcs-srcs">srcs</a>, <a href="#lisp_compile_srcs-deps">deps</a>, <a href="#lisp_compile_srcs-cdeps">cdeps</a>, <a href="#lisp_compile_srcs-block_compile">block_compile</a>, <a href="#lisp_compile_srcs-block_compile_specified_only">block_compile_specified_only</a>, <a href="#lisp_compile_srcs-image">image</a>,
                  <a href="#lisp_compile_srcs-add_features">add_features</a>, <a href="#lisp_compile_srcs-nowarn">nowarn</a>, <a href="#lisp_compile_srcs-order">order</a>, <a href="#lisp_compile_srcs-compile_data">compile_data</a>, <a href="#lisp_compile_srcs-verbose_level">verbose_level</a>, <a href="#lisp_compile_srcs-instrument_coverage">instrument_coverage</a>,
                  <a href="#lisp_compile_srcs-indexer_metadata">indexer_metadata</a>)
</pre>

Generate LispCompile actions, return LispInfo and FASL output.

This is the core functionality shared by the Lisp build rules.


**PARAMETERS**


| Name  | Description | Default Value |
| :------------- | :------------- | :------------- |
| <a id="lisp_compile_srcs-ctx"></a>ctx |  The rule context.   |  none |
| <a id="lisp_compile_srcs-srcs"></a>srcs |  list of src Files.   |  `[]` |
| <a id="lisp_compile_srcs-deps"></a>deps |  list of immediate Lisp dependency Targets.   |  `[]` |
| <a id="lisp_compile_srcs-cdeps"></a>cdeps |  list of immediate C++ dependency Targets.   |  `[]` |
| <a id="lisp_compile_srcs-block_compile"></a>block_compile |  Whether to block-compile this target.   |  `False` |
| <a id="lisp_compile_srcs-block_compile_specified_only"></a>block_compile_specified_only |  Whether to only combine top-level forms in blokcs that are in explicitly specified (with `(start-block)` and `(end-block)`) when block compiling.   |  `False` |
| <a id="lisp_compile_srcs-image"></a>image |  Build image Target used to compile the sources.   |  `None` |
| <a id="lisp_compile_srcs-add_features"></a>add_features |  list of Lisp feature strings added by this target.   |  `[]` |
| <a id="lisp_compile_srcs-nowarn"></a>nowarn |  List of suppressed warning type strings.   |  `[]` |
| <a id="lisp_compile_srcs-order"></a>order |  Order in which to load sources, either "serial", "parallel", or "multipass".   |  `"serial"` |
| <a id="lisp_compile_srcs-compile_data"></a>compile_data |  list of data dependency Targets whose outputs and runfiles are made available at load/compile time for this target and its consumers.   |  `[]` |
| <a id="lisp_compile_srcs-verbose_level"></a>verbose_level |  int indicating level of debugging output.   |  `0` |
| <a id="lisp_compile_srcs-instrument_coverage"></a>instrument_coverage |  Controls coverage instrumentation, with the following values: -1 (default) - Instruments if coverage is enabled for this target. 0 - Instruments never. 1 - Instruments always (for testing purposes).   |  `-1` |
| <a id="lisp_compile_srcs-indexer_metadata"></a>indexer_metadata |  Extra metadata files to be passed to the --deps flag of LispCompile when the Kythe indexer is run. Ignored by the build image itself, but this appears in the command-line for the LispCompile action which can be inspected by action_listener.   |  `[]` |

**RETURNS**

struct with fields:
    - lisp_info: LispInfo for the target
    - output_fasl: Combined FASL for this target (which is also included in
        lisp_info.fasls if there are srcs)
    - build_flags: Args to pass to all LispCompile and LispCore actions


