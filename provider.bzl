"""Functions generating or modifying the Lisp provider struct.

A Lisp (lisp) provider is a struct with:
  deps: transitive dependencies,
  srcs: transitive sources,
  hashes: md5 hash files of the sources,
  warnings: files with warnings checked at link time (FASL load),
  features: transitive declared Lisp features,
  runtime_data: data used at runtime for all dependencies,
  compile_data: data used at load/compile time for all dependencies

Note that the fields on the Lisp provider contain sets of file names
for all transitive dependencies.
"""

def transitive_deps(deps = [], image = None):
    """Given a list of depsets create a structure containing the
    transitive dependencies.

    Note the DEPS themselves are not added to the provider transitive list.

    Args:
      deps: the list of dependencies,
      image: the image which may contain dependencies as well.

    Returns:
      A structure containing the transitive dependancies
    """

    # Figure out the transitive properties.
    trans_deps = []
    trans_srcs = []
    trans_hashes = []
    trans_warnings = []
    trans_features = []
    trans_runtime_data = []
    trans_compile_data = []

    # Add the transitive dependencies from the image.
    # Image's DEPS and SRCS need to be removed before compilation.
    if image and hasattr(image, "lisp"):
        trans_deps += [image.lisp.deps]
        trans_srcs += [image.lisp.srcs]
        trans_hashes += [image.lisp.hashes]
        trans_warnings += [image.lisp.warnings]
        trans_features += [image.lisp.features]
        trans_runtime_data += [image.lisp.runtime_data]
        trans_compile_data += [image.lisp.compile_data]

    for dep in deps:
        if hasattr(dep, "lisp"):
            trans_deps += [dep.lisp.deps]
            trans_srcs += [dep.lisp.srcs]
            trans_hashes += [dep.lisp.hashes]
            trans_warnings += [dep.lisp.warnings]
            trans_features += [dep.lisp.features]
            trans_runtime_data += [dep.lisp.runtime_data]
            trans_compile_data += [dep.lisp.compile_data]

    return struct(
        deps = depset(transitive = trans_deps),
        srcs = depset(transitive = trans_srcs),
        hashes = depset(transitive = trans_hashes),
        warnings = depset(transitive = trans_warnings),
        features = depset(transitive = trans_features),
        runtime_data = depset(transitive = trans_runtime_data),
        compile_data = depset(transitive = trans_compile_data),
    )

def extend_lisp_provider(
        base,
        deps = [],
        srcs = [],
        hashes = [],
        warnings = [],
        features = [],
        data = [],
        compile_data = []):
    """Returns a provider structure with added dependencies.

     Args:
      base: the base provider to be extended.
      deps: more dependencies to be added to the old provider.
      srcs: more sources
      hashes: more hashes files
      warnings: more deferred warning files
      features: more features
      data: more runtime data
      compile_data: more compile_data
    """
    return struct(
        deps = depset(deps, transitive = [base.deps]),
        srcs = depset(srcs, transitive = [base.srcs]),
        hashes = depset(hashes, transitive = [base.hashes]),
        warnings = depset(warnings, transitive = [base.warnings]),
        features = depset(features, transitive = [base.features]),
        runtime_data = depset(data, transitive = [base.runtime_data]),
        compile_data = depset(compile_data, transitive = [base.compile_data]),
    )

def print_provider(p):
    "Prints the Lisp provider P."
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
    if p.runtime_data:
        print("Runtime Data: %s" % list(p.runtime_data))
    if p.compile_data:
        print("Compile Data: %s" % list(p.compile_data))
