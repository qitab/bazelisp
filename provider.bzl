"""Functions generating or modifying the LispInfo provider struct."""

LispInfo = provider(fields = {
    "deps": "Depset of transitive dependencies",
    "srcs": "Depset of transitive sources",
    "hashes": "Depset of md5 hash files for transitive sources",
    "warnings": "Depset of files of warnings checked at link time (FASL load) for transitive sources",
    "features": "Depset of transitive declared Lisp features",
    "compile_data": "Depset of data used at load/compile time for all dependencies",
})

# TODO(b/142249042): Remove this workaround.
# For use in testing. These paths end up in the command lines generated for
# compilation actions, and analysis tests can assert about those:
# https://docs.bazel.build/versions/master/skylark/testing.html
# This is a workaround, it would be better if that was provided by DefaultInfo
# or handled by something in unittest.bzl.
OutputDirInfo = provider(fields = {
    "bin_path": "ctx.bin_dir.path",
})

def output_dir_info(ctx):
    return OutputDirInfo(
        bin_path = ctx.bin_dir.path,
    )

def transitive_deps(deps = [], build_image = None):
    """Create LispInfo with transitive (but not immediate) deps.

    Given a list of depsets create a structure containing the transitive
    dependencies. Note the DEPS themselves are not added to the provider
    transitive list.

    Args:
      deps: the list of dependencies,
      build_image: the image used to build this which may contain dependencies
        as well.

    Returns:
      A structure containing the transitive dependancies
    """
    lisp_infos = [d[LispInfo] for d in deps if LispInfo in d]
    if build_image and LispInfo in build_image:
        lisp_infos.append(build_image[LispInfo])

    return LispInfo(
        deps = depset(transitive = [li.deps for li in lisp_infos]),
        srcs = depset(transitive = [li.srcs for li in lisp_infos]),
        hashes = depset(transitive = [li.hashes for li in lisp_infos]),
        warnings = depset(transitive = [li.warnings for li in lisp_infos]),
        features = depset(transitive = [li.features for li in lisp_infos]),
        compile_data = depset(transitive = [li.compile_data for li in lisp_infos]),
    )

def extend_lisp_provider(
        base,
        deps = [],
        srcs = [],
        hashes = [],
        warnings = [],
        features = [],
        compile_data = []):
    """Returns a LispInfo, adding the immediate info for a rule.

     Args:
      base: the base LispInfo provider to be extended.
      deps: more dependencies to be added to the old provider.
      srcs: more sources
      hashes: more hashes files
      warnings: more deferred warning files
      features: more features
      compile_data: more compile_data
    """
    return LispInfo(
        deps = depset(deps, transitive = [base.deps]),
        srcs = depset(srcs, transitive = [base.srcs]),
        hashes = depset(hashes, transitive = [base.hashes]),
        warnings = depset(warnings, transitive = [base.warnings]),
        features = depset(features, transitive = [base.features]),
        compile_data = depset(compile_data, transitive = [base.compile_data]),
    )

# buildozer: disable=print
def print_provider(p):
    """Prints the LispInfo provider.

    Args:
        p: A LispInfo provider.
    """
    if p.deps:
        print("Deps: %s" % [d.short_path for d in p.deps])
    if p.srcs:
        print("Srcs: %s" % [s.short_path for s in p.srcs])
    if p.hashes:
        print("Hashes: %s" % [h.short_path for h in p.hashes])
    if p.warnings:
        print("Warnings: %s" % [w.short_path for w in p.warnings])
    if p.features:
        print("Features: %s" % list(p.features))
    if p.compile_data:
        print("Compile Data: %s" % list(p.compile_data))
