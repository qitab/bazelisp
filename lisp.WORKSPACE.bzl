### You must call this function with the path to the base directory
### in which lisp__bazel is installed, e.g. "/home/tunes/src/google/bazelisp"
def lisp_repositories (base_dir):
  native.local_repository(
      name = "lisp__bazel",
      path = base_dir,
  )

  ### EDIT ME! For bootstrap purpose, you need a sbcl binary distribution to compile sbcl.
  ### Here we checkin /usr as a default that might work on your machine if e.g. you install
  ### the sbcl package from Debian or Ubuntu. If you compile and install SBCL yourself from
  ### source with the default settings, that'd be "/usr/local". On my machine it's in
  ### "~/local/stow/sbcl". YMMV. In a production setting, we strongly recommend that you either
  ### check in the binary distribution in your source control, or that you include the underlying
  ### operating system's packages in the version-controled perimeter, or e.g. use NixOS.
  ### NB: If said binary distribution of sbcl already contains libsbcl.a and
  ### libsbcl-exported-symbols.lds, and is properly version-controled, then you can directly
  ### use SBCL_PACKAGE = "@lisp__sbcl_binary_distribution//:" in @lisp__bazel//:bazel/rule-guts.bzl
  ### and save yourself having to build sbcl with bazel.
  native.new_local_repository(
      name = "lisp__sbcl_binary_distribution",
      path = "/usr",
      build_file = base_dir + "/build_defs/lisp__sbcl_binary_distribution.BUILD"
  )

  native.new_http_archive(
      name = "c__zlib",
      url = "http://zlib.net/zlib-1.2.8.tar.gz",
      sha256 = "36658cb768a54c1d4dec43c3116c27ed893e88b02ecfcb44f2166f9c0b7f2a0d",
      strip_prefix = "zlib-1.2.8",
      build_file = base_dir + "/build_defs/c__zlib.BUILD",
  )

  native.new_git_repository(
      name = "lisp__sbcl",
      commit = "4593810d673b88eac32f30cb42d39bcfaedc74f7",
      remote = "git://git.code.sourceforge.net/p/sbcl/sbcl",
      build_file = base_dir + "/build_defs/lisp__sbcl.BUILD"
  )

  native.new_git_repository(
      name = "lisp__alexandria",
      commit = "5a17c072970cf50213f7f896c40e6e640638391f",
      remote = "https://gitlab.common-lisp.net/alexandria/alexandria.git",
      build_file = base_dir + "/build_defs/lisp__alexandria.BUILD"
  )

  native.new_git_repository(
      name = "lisp__babel",
      commit = "6aaea300d55dcddedc1d584114605d92249570e5",
      remote = "https://github.com/cl-babel/babel.git",
      build_file = base_dir + "/build_defs/lisp__babel.BUILD"
  )

  native.new_git_repository(
      name = "lisp__trivial_features",
      commit = "c6ebccf5539798ace8cbe277e8ae85c39742a19c",
      remote = "https://github.com/trivial-features/trivial-features.git",
      build_file = base_dir + "/build_defs/lisp__trivial_features.BUILD"
  )

  native.local_repository(
      name = "lisp__hello",
      path = base_dir + "/hello",
  )
