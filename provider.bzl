# Copyright 2015-2020 Google LLC
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.

"""Functions generating or modifying the LispInfo provider struct."""

LispInfo = provider(
    doc = "Information about a lisp_* target and its transitive dependencies.",
    fields = {
        "fasls": "Depset of FASLs for transitive dependencies",
        "srcs": "Depset of transitive sources",
        "hashes": "Depset of md5 hash files for transitive sources",
        "warnings": (
            "Depset of files of warnings checked at link time (FASL load) " +
            "for transitive sources"
        ),
        "features": "Depset of transitive declared Lisp features",
        "compile_data": (
            "Depset of files from transitive compile_data, made available " +
            "at build time as well as runtime."
        ),
        "cc_info": "CcInfo representing transitive C++ dependencies for linking",
    },
)

def collect_lisp_info(deps = [], cdeps = [], build_image = None, features = [], compile_data = []):
    """Create a LispInfo collecting the data needed for Lisp compilation.

    Args:
        deps: Immediate Lisp dependencies of this target.
        cdeps: Immediate C++ dependencies of this target.
        build_image: Optional build image Target, which may also contain
            dependencies. May be unset if this target just propagates LispInfo
            from its dependencies.
        features: Lisp features added by this target.
        compile_data: Data dependency Targets of this target, which are made
            available to this target and its consumers' at build time as well
            as runtime.

    Returns:
        LispInfo
    """
    lisp_infos = [dep[LispInfo] for dep in deps]
    if build_image and LispInfo in build_image:
        lisp_infos.append(build_image[LispInfo])

    cc_infos = [lisp_info.cc_info for lisp_info in lisp_infos]
    for cdep in cdeps:
        cc_infos.append(cdep[CcInfo])

    transitive_compile_data = [li.compile_data for li in lisp_infos]
    for compile_data_target in compile_data:
        default_info = compile_data_target[DefaultInfo]
        if default_info.files:
            transitive_compile_data.append(default_info.files)
        if default_info.default_runfiles:
            transitive_compile_data.append(default_info.default_runfiles.files)

    return LispInfo(
        fasls = depset(
            transitive = [li.fasls for li in lisp_infos],
            order = "postorder",
        ),
        srcs = depset(
            transitive = [li.srcs for li in lisp_infos],
            order = "postorder",
        ),
        hashes = depset(transitive = [li.hashes for li in lisp_infos]),
        warnings = depset(transitive = [li.warnings for li in lisp_infos]),
        features = depset(
            features,
            transitive = [li.features for li in lisp_infos],
        ),
        compile_data = depset(transitive = transitive_compile_data),
        cc_info = cc_common.merge_cc_infos(cc_infos = cc_infos),
    )

def extend_lisp_info(
        base,
        fasls = [],
        srcs = [],
        hashes = [],
        warnings = []):
    """Extends a LispInfo with compilation inputs and outputs.

     Args:
      base: The base LispInfo provider to be extended.
      fasls: FASLs generated for this target.
      srcs: This target's Lisp sources.
      hashes: Hash files for each file in srcs.
      warnings: Warnings files for each file in srcs.
    """
    return LispInfo(
        fasls = depset(fasls, transitive = [base.fasls], order = "postorder"),
        srcs = depset(srcs, transitive = [base.srcs], order = "postorder"),
        hashes = depset(hashes, transitive = [base.hashes]),
        warnings = depset(warnings, transitive = [base.warnings]),
        features = base.features,
        compile_data = base.compile_data,
        cc_info = base.cc_info,
    )

# buildozer: disable=print
def print_provider(p):
    """Prints the LispInfo provider.

    Args:
        p: A LispInfo provider.
    """
    if p.fasls:
        print("FASLs: %s" % [f.short_path for f in p.fasls.to_list()])
    if p.srcs:
        print("Srcs: %s" % [s.short_path for s in p.srcs.to_list()])
    if p.hashes:
        print("Hashes: %s" % [h.short_path for h in p.hashes.to_list()])
    if p.warnings:
        print("Warnings: %s" % [w.short_path for w in p.warnings.to_list()])
    if p.features:
        print("Features: %s" % p.features.to_list())
    if p.compile_data:
        print("Compile Data: %s" % p.compile_data.to_list())
