# -*- mode: Python; -*-

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

def transitive_deps(deps=[], image=None):
  """Returns a provider structure containing transitive dependencies.

  Note the DEPS themselves are not added to the provider transitive list.

  Args:
   deps: the list of dependencies,
   image: the image which may contain dependencies as well.
"""
  # Figure out the transitive properties.
  trans_deps = set()
  trans_srcs = set()
  trans_hashes = set()
  trans_warnings = set()
  trans_features  = set()
  trans_runtime_data = set()
  trans_compile_data = set()
  # Add the transitive dependencies from the image.
  # Image's DEPS and SRCS need to be removed before compilation.
  if image and hasattr(image, "lisp"):
    trans_deps += image.lisp.deps
    trans_srcs += image.lisp.srcs
    trans_hashes += image.lisp.hashes
    trans_warnings += image.lisp.warnings
    trans_features += image.lisp.features
    trans_runtime_data += image.lisp.runtime_data
    trans_compile_data += image.lisp.compile_data

  for dep in deps:
    if hasattr(dep, "lisp"):
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

def extend_lisp_provider (base, deps=[], srcs=[], hashes=[], warnings=[],
                          features=[], data=[], compile_data=[]):
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
     deps = set(list(base.deps) + list(deps)),
     srcs = set(list(base.srcs) + list(srcs)),
     hashes = set(list(base.hashes) + list(hashes)),
     warnings = set(list(base.warnings) + list(warnings)),
     features = set(list(base.features) + list(features)),
     runtime_data = set(list(base.runtime_data) + list(data)),
     compile_data = set(list(base.compile_data) + list(compile_data)))
