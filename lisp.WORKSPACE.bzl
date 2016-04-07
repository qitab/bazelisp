### You must call this function with the path to the base directory
### in which lisp__bazel is installed, e.g. "/home/tunes/src/google/bazelisp"
###
### Example: Assuming your WORKSPACE is under ~/src/google/bazel,
### symlink lisp.WORKSPACE.bzl into ~/src/google/bazel/ and add
### to your ~/src/google/bazel/WORKSPACE the following lines (uncommented):
###    load("/lisp.WORKSPACE", "lisp_repositories")
###    lisp_repositories("/home/fare/src/google/bazelisp")
###
### But you must also edit the entry for lisp__sbcl_binary_distribution.

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
  ### use SBCL_PACKAGE = "@lisp__sbcl_binary_distribution//:" in @lisp__bazel//:bazel/rules.bzl
  ### instead of SBCL_PACKAGE = "@lisp__sbcl//:" and save yourself having to build sbcl with bazel.
  native.new_local_repository(
      name = "lisp__sbcl_binary_distribution",
      path = "/home/fare/local/stow/sbcl",
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
      commit = "b911cdc2080a7a16d5537d959728f0c250fa3f4e",
      remote = "git://git.code.sourceforge.net/p/sbcl/sbcl",
      build_file = base_dir + "/build_defs/lisp__sbcl.BUILD"
  )


  ### The libraries below are in asciibetical order.

  native.new_git_repository(
      name = "lisp__alexandria",
      commit = "5a17c072970cf50213f7f896c40e6e640638391f",
      remote = "https://gitlab.common-lisp.net/alexandria/alexandria.git",
      build_file = base_dir + "/build_defs/lisp__alexandria.BUILD"
  )

  native.new_git_repository(
      name = "lisp__asdf",
      commit = "03df017f1cf4cb1737577d62d56e207fb77320a4",
      remote = "https://gitlab.common-lisp.net/asdf/asdf.git",
      build_file = base_dir + "/build_defs/lisp__asdf.BUILD"
  )

  native.new_local_repository(
      name = "lisp__asdf_tools",
      path = base_dir + "/doc", # dummy directory
      build_file = base_dir + "/build_defs/lisp__asdf_tools.BUILD"
  )

  native.new_git_repository(
      name = "lisp__asdf_encodings",
      commit = "698ea4a16b11dd419d683744ac59b06bee07b853",
      remote = "https://gitlab.common-lisp.net/asdf/asdf-encodings.git",
      build_file = base_dir + "/build_defs/lisp__asdf_encodings.BUILD"
  )

  native.new_git_repository(
      name = "lisp__babel",
      commit = "6aaea300d55dcddedc1d584114605d92249570e5",
      remote = "https://github.com/cl-babel/babel.git",
      build_file = base_dir + "/build_defs/lisp__babel.BUILD"
  )

  native.new_git_repository(
      name = "lisp__bordeaux_threads",
      commit = "9673c70d09f85c7a410a3dd6aad653c2b15219e4",
      remote = "https://github.com/sionescu/bordeaux-threads.git",
      build_file = base_dir + "/build_defs/lisp__bordeaux_threads.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cffi",
      commit = "b26e5b0bd3063f1935294d3446f761c80ee97e1b",
      remote = "https://github.com/cffi/cffi.git",
      build_file = base_dir + "/build_defs/lisp__cffi.BUILD"
  )

  native.new_http_archive(
      name = "lisp__chipz",
      url = "http://www.method-combination.net/lisp/files/chipz.tar.gz",
      sha256 = "50c1862b0ae086bad1c8c3846038c33057d59a57d86c99ad03b5c33db2284068",
      strip_prefix = "chipz_0.8",
      build_file = base_dir + "/build_defs/lisp__chipz.BUILD",
  )

  native.new_git_repository(
      name = "lisp__cl_base64",
      commit = "fc62a5342445d4ec1dd44e95f7dc513473a8c89a",
      remote = "http://git.kpe.io/cl-base64.git",
      build_file = base_dir + "/build_defs/lisp__cl_base64.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cl_fad",
      commit = "51ea56cde5c8d2dafd9a5173e2c0b81d98babc94",
      remote = "https://github.com/edicl/cl-fad.git",
      build_file = base_dir + "/build_defs/lisp__cl_fad.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cl_json",
      commit = "9e4e2c0ce24db9922b296443cc51030df9d39df8",
      #remote = "https://github.com/hankhero/cl-json.git", ## upstream
      ## This includes a patch. A pull request was send to upstream.
      remote = "https://github.com/fare/cl-json.git",
      build_file = base_dir + "/build_defs/lisp__cl_json.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cl_launch",
      commit = "1698221a6ff49126d4f023d1d260f6f8c1cb41bf",
      remote = "https://gitlab.common-lisp.net/xcvb/cl-launch.git",
      build_file = base_dir + "/build_defs/lisp__cl_launch.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cl_ppcre",
      commit = "26aa9b68fb59cc18d216efe0f15d535eb11a2d5a",
      remote = "https://github.com/edicl/cl-ppcre.git",
      build_file = base_dir + "/build_defs/lisp__cl_ppcre.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cl_protobufs",
      commit = "d008be53f8e3e2e3a51eb8a64419b3afc2eebc31",
      remote = "https://gitlab.common-lisp.net/qitab/cl-protobufs.git",
      build_file = base_dir + "/build_defs/lisp__cl_protobufs.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cl_scripting",
      commit = "dd5570ee39310da04df20528f854f4f34b8df3f2",
      remote = "https://github.com/fare/cl-scripting.git",
      build_file = base_dir + "/build_defs/lisp__cl_scripting.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cl_unicode",
      commit = "ecacf5dece1f720aec3f90889e58496fc74e6ef1",
      remote = "https://github.com/edicl/cl-unicode.git",
      build_file = base_dir + "/build_defs/lisp__cl_unicode.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cl_yacc",
      commit = "1334f5469251ffb3f8738a682dc8ee646cb26635",
      remote = "https://github.com/jech/cl-yacc.git",
      build_file = base_dir + "/build_defs/lisp__cl_yacc.BUILD"
  )

  native.new_git_repository(
      name = "lisp__closer_mop",
      commit = "da5462689966d14b281861e7449014f7ea1f51f6",
      remote = "https://github.com/pcostanza/closer-mop.git",
      build_file = base_dir + "/build_defs/lisp__closer_mop.BUILD"
  )

  native.new_git_repository(
      name = "lisp__closure_common",
      commit = "377f8275f2c90a6fbde800a6c690b33fa166865c",
      remote = "git://repo.or.cz/closure-common.git",
      build_file = base_dir + "/build_defs/lisp__closure_common.BUILD"
  )

#  native.new_git_repository(
#      name = "lisp__cxml",
#      commit = "9365c4b93c3e5adc55a6512b3fb453693d06a707",
#      remote = "git://repo.or.cz/cxml.git",
#      build_file = base_dir + "/build_defs/lisp__cxml.BUILD"
#  )

  native.new_git_repository(
      name = "lisp__eos",
      commit = "b0faca83781ead9a588661e37bd47f90362ccd94",
      remote = "https://github.com/adlai/Eos.git",
      build_file = base_dir + "/build_defs/lisp__eos.BUILD"
  )

  native.new_git_repository(
      name = "lisp__fare_mop",
      commit = "538aa94590a0354f382eddd9238934763434af30",
      remote = "https://gitlab.common-lisp.net/frideau/fare-mop.git",
      build_file = base_dir + "/build_defs/lisp__fare_mop.BUILD"
  )

  native.new_git_repository(
      name = "lisp__fare_quasiquote",
      commit = "863ec450f9809171462b7c7ec85c778496a0f3ae",
      remote = "https://gitlab.common-lisp.net/frideau/fare-quasiquote.git",
      build_file = base_dir + "/build_defs/lisp__fare_quasiquote.BUILD"
  )

  native.new_git_repository(
      name = "lisp__fare_utils",
      commit = "1a4f345d7911b403d07a5f300e6006ce3efa4047",
      remote = "https://gitlab.common-lisp.net/frideau/fare-utils.git",
      build_file = base_dir + "/build_defs/lisp__fare_utils.BUILD"
  )

  native.new_git_repository(
      name = "lisp__flexi_streams",
      commit = "688ad7dce44cc836f690056fcfa509ff4d947b3b",
      remote = "https://github.com/edicl/flexi-streams.git",
      build_file = base_dir + "/build_defs/lisp__flexi_streams.BUILD"
  )

  native.local_repository(
      name = "lisp__hello",
      path = base_dir + "/hello",
  )

  native.new_git_repository(
      name = "lisp__hu_dwim_stefil",
      commit = "ab6d1aa8995878a1b66d745dfd0ba021090bbcf9",
      remote = "https://gitlab.common-lisp.net/xcvb/hu.dwim.stefil.git",
      build_file = base_dir + "/build_defs/lisp__hu_dwim_stefil.BUILD"
  )

  native.new_git_repository(
      name = "lisp__inferior_shell",
      commit = "25f0c1c15f38c54272f9f1c81f30797bb7be8d00",
      remote = "https://gitlab.common-lisp.net/qitab/inferior-shell.git",
      build_file = base_dir + "/build_defs/lisp__inferior_shell.BUILD"
  )

  native.new_git_repository(
      name = "lisp__ironclad",
      commit = "e9224500649ddaf17c414fd08fb7b99e906fc2a7",
      remote = "https://github.com/froydnj/ironclad.git",
      build_file = base_dir + "/build_defs/lisp__ironclad.BUILD"
  )

  native.new_git_repository(
      name = "lisp__lisp_invocation",
      commit = "0a55ecc11cf564a13f443dd8916d00449b100636",
      remote = "https://gitlab.common-lisp.net/qitab/lisp-invocation.git",
      build_file = base_dir + "/build_defs/lisp__lisp_invocation.BUILD"
  )

  native.new_git_repository(
      name = "lisp__local_time",
      commit = "fb28d70ba8eb800d5af30fcd33c9d36d6a03e899",
      remote = "https://github.com/dlowe-net/local-time.git",
      build_file = base_dir + "/build_defs/lisp__local_time.BUILD"
  )

  native.new_git_repository(
      name = "lisp__md5",
      commit = "2752a77c70252b2f460961d6a8bff0127d9ac74c",
      remote = "https://github.com/pmai/md5.git",
      build_file = base_dir + "/build_defs/lisp__md5.BUILD"
  )

  native.new_git_repository(
      name = "lisp__mixalot",
      commit = "4b184c882ac8a96971a4756362bfee0a842a6191",
      remote = "https://github.com/ahefner/mixalot.git",
      build_file = base_dir + "/build_defs/lisp__mixalot.BUILD"
  )

  native.new_git_repository(
      name = "lisp__named_readtables",
      commit = "4dfb89fa1af6b305b6492b8af042f5190c11e9fc",
      remote = "https://github.com/melisgl/named-readtables.git",
      build_file = base_dir + "/build_defs/lisp__named_readtables.BUILD"
  )

  native.new_git_repository(
      name = "lisp__nibbles",
      commit = "421c84fb704f3d5945f26ef6eaeb05fcfae41099",
      remote = "https://github.com/froydnj/nibbles.git",
      build_file = base_dir + "/build_defs/lisp__nibbles.BUILD"
  )

  native.new_git_repository(
      name = "lisp__optima",
      commit = "373b245b928c1a5cce91a6cb5bfe5dd77eb36195",
      remote = "https://github.com/m2ym/optima.git",
      build_file = base_dir + "/build_defs/lisp__optima.BUILD"
  )

  native.new_git_repository(
      name = "lisp__parse_number",
      commit = "094efb0f11f729bc684ef0b0f9a17a12569c5b32",
      remote = "https://github.com/sharplispers/parse-number.git",
      build_file = base_dir + "/build_defs/lisp__parse_number.BUILD"
  )

#  native.new_git_repository(
#      name = "lisp__xpath",
#      commit = "7abb91be81d8b18066b37db564862dda34ee4d93",
#      remote = "https://github.com/gonzojive/plexippus-xpath.git",
#      build_file = base_dir + "/build_defs/lisp__xpath.BUILD"
#  )

  native.new_git_repository(
      name = "lisp__poiu",
      commit = "e1ff48ef8b707b9533906e44075dc827c6dedc7e",
      remote = "https://gitlab.common-lisp.net/qitab/poiu.git",
      build_file = base_dir + "/build_defs/lisp__poiu.BUILD"
  )

  native.new_git_repository(
      name = "lisp__ptester",
      commit = "332ed2f9f41fbe329f500089fa1b593bdcfa2d93",
      remote = "http://git.kpe.io/ptester.git",
      build_file = base_dir + "/build_defs/lisp__ptester.BUILD"
  )

  native.new_git_repository(
      name = "lisp__puri",
      commit = "e523dcc81fa3947bed9b6093641eed216fc0ec29",
      remote = "http://git.kpe.io/puri.git",
      build_file = base_dir + "/build_defs/lisp__puri.BUILD"
  )

  native.new_git_repository(
      name = "lisp__slime",
      commit = "85e2e614b3adf6e6295ee07f14ca44db4512a409",
      remote = "https://github.com/slime/slime",
      build_file = base_dir + "/build_defs/lisp__slime.BUILD"
  )

#  native.new_git_repository(
#      name = "lisp__swank_client",
#      commit = "c1d01dd66cbbbe886ee4e38b6f417452e534dc59",
#      remote = "https://github.com/brown/swank-client.git",
#      build_file = base_dir + "/build_defs/lisp__swank_client.BUILD"
#  )

  native.new_git_repository(
      name = "lisp__trivial_backtrace",
      commit = "43ef7d947f4b4de767d0f91f28b50d9c03ad29d6",
      remote = "https://gitlab.common-lisp.net/trivial-backtrace/trivial-backtrace.git",
      build_file = base_dir + "/build_defs/lisp__trivial_backtrace.BUILD"
  )

  native.new_git_repository(
      name = "lisp__trivial_features",
      commit = "c6ebccf5539798ace8cbe277e8ae85c39742a19c",
      remote = "https://github.com/trivial-features/trivial-features.git",
      build_file = base_dir + "/build_defs/lisp__trivial_features.BUILD"
  )

  native.new_git_repository(
      name = "lisp__trivial_garbage",
      commit = "5adca9908632124ca28bd17355ac281352fd97cb",
      remote = "https://github.com/trivial-garbage/trivial-garbage",
      build_file = base_dir + "/build_defs/lisp__trivial_garbage.BUILD"
  )

  native.new_git_repository(
      name = "lisp__trivial_gray_streams",
      commit = "0483ade330508b4b2edeabdb47d16ec9437ee1cb",
      remote = "https://github.com/trivial-gray-streams/trivial-gray-streams.git",
      build_file = base_dir + "/build_defs/lisp__trivial_gray_streams.BUILD"
  )

  native.new_git_repository(
      name = "lisp__usocket",
      commit = "c6f092c767cf89172e0b06a9014d2e4e855b3781",
      remote = "https://github.com/usocket/usocket.git",
      build_file = base_dir + "/build_defs/lisp__usocket.BUILD"
  )

  native.new_git_repository(
      name = "lisp__workout_timer",
      commit = "9ad2f8a0df074e5a1034fd54256cf0b8a9ee6f56",
      remote = "https://gitlab.common-lisp.net/frideau/workout-timer.git",
      build_file = base_dir + "/build_defs/lisp__workout_timer.BUILD"
  )
