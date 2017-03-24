### Setting up your Bazel WORKSPACE for Common Lisp support.
#
# USAGE: You *must*:
# (1) Symlink or copy this edited file to your main Bazel WORKSPACE directly.
# (2) From your main Bazel WORKSPACE file `load("/lisp.WORKSPACE.bzl", "lisp_repositories")`
#   then call the function lisp_repositories() with two arguments:
#   (a) the full Unix path to the bazelisp directory, e.g. "/home/fare/src/google/bazelisp", and
#   (b) the full Unix path to the base of a bootstrap SBCL binary distribution, e.g. "/usr/local"
#     such that bin/sbcl and lib/sbcl/ are under that base directory.
#

def auto_configure_fail(msg):
  """Output failure message when auto configuration fails."""
  red = "\033[0;31m"
  no_color = "\033[0m"
  fail("\n%sAuto-Configuration Error:%s %s\n" % (red, no_color, msg))


def _execute(repository_ctx, command, environment = None):
  """Execute a command, return stdout if succeed and throw an error if it fails."""
  if environment:
    result = repository_ctx.execute(command, environment = environment)
  else:
    result = repository_ctx.execute(command)
  if result.stderr:
    auto_configure_fail(result.stderr)
  else:
    return result.stdout.strip()


def _get_os(repository_ctx):
  """Compute the architecture based on the OS name."""
  return _execute(repository_ctx, ["uname", "-s"]).lower()


def _get_cpu(repository_ctx):
  """Compute the CPU architecture based on the uname results."""
  # Use uname to figure out whether we are on x86_32 or x86_64
  m = _execute(repository_ctx, ["uname", "-m"])
  if m in ["amd64", "x86_64", "x64"]:
    return "x86-64"
  if m in ["x86", "i386", "i686"]:
    return "x86"
  if m in ["power", "ppc64le", "ppc"]:
    return "ppc"
  return "unknown"


def _get_params(repository_ctx):
  """Return the content for the CROSSTOOL file, in a dictionary."""
  return {
      "%{cpu}": _get_cpu(repository_ctx),
      "%{os}": _get_os(repository_ctx)
  }


def _impl(repository_ctx):
  params = _get_params(repository_ctx)
  repository_ctx.file("BUILD")
  repository_ctx.template(
    "config.bzl",
    Label("@lisp__bazel//:build_defs/local_config_lisp.config.bzl.tpl"),
    params)


lisp_autoconf = repository_rule(implementation=_impl, local=True)


def lisp_configure():
  """configuration rules that generate the local_config_lisp external repo."""
  lisp_autoconf(name="local_config_lisp")


def lisp_repositories (base_dir, sbcl_binary_distribution):
  native.local_repository(
      name = "lisp__bazel",
      path = base_dir,
  )

  native.new_local_repository(
      name = "lisp__sbcl_binary_distribution",
      path = sbcl_binary_distribution,
      build_file = base_dir + "/build_defs/lisp__sbcl_binary_distribution.BUILD"
  )

  native.new_http_archive(
      name = "c__zlib",
      url = "http://zlib.net/zlib-1.2.11.tar.gz",
      sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
      strip_prefix = "zlib-1.2.11",
      build_file = base_dir + "/build_defs/c__zlib.BUILD",
  )

  native.new_git_repository(
      name = "lisp__sbcl",
      commit = "00b6786847b4407214a0dc8d55d12ed8b61bf074", # sbcl-1.3.15
      remote = "git://git.code.sourceforge.net/p/sbcl/sbcl",
      build_file = base_dir + "/build_defs/lisp__sbcl.BUILD"
  )


  ### The libraries below are in asciibetical order.

  native.new_git_repository(
      name = "lisp__alexandria",
      commit = "85f82ed88d5fa6e63026038dbb1dad0d6cd5dafe",
      remote = "https://gitlab.common-lisp.net/alexandria/alexandria.git",
      build_file = base_dir + "/build_defs/lisp__alexandria.BUILD"
  )

  native.new_git_repository(
      name = "lisp__asdf",
      commit = "d0eef0700d9272b85475e6654c88e12a96953f08", # 3.2.0.1
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
      commit = "92e6a89486a9469a36b4c0f73f4efc38f4ddeecc",
      remote = "https://github.com/sionescu/bordeaux-threads.git",
      build_file = base_dir + "/build_defs/lisp__bordeaux_threads.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cffi",
      commit = "3563ccc32caa356dab7d9e47086cda318b887625",
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
      commit = "56b0909150d54b2e3aaf2660735702b35558445d",
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
      commit = "1698221a6ff49126d4f023d1d260f6f8c1cb41bf", # 4.1.5
      remote = "https://gitlab.common-lisp.net/xcvb/cl-launch.git",
      build_file = base_dir + "/build_defs/lisp__cl_launch.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cl_ppcre",
      commit = "0f295337d9dec1733cf0389a5a2827292d6a5b0d",
      remote = "https://github.com/edicl/cl-ppcre.git",
      build_file = base_dir + "/build_defs/lisp__cl_ppcre.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cl_protobufs",
      commit = "bb5c018d389ea3b432b20dbeccfdef5beed52872",
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
      commit = "ba6f92f3a2b41c751ddfd9fad1c5a09496d58367",
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
      commit = "cca38c87c7605218ed1138938c3646be153bcd92",
      remote = "https://github.com/pcostanza/closer-mop.git",
      build_file = base_dir + "/build_defs/lisp__closer_mop.BUILD"
  )

  native.new_git_repository(
      name = "lisp__closure_common",
      commit = "377f8275f2c90a6fbde800a6c690b33fa166865c",
      remote = "git://repo.or.cz/closure-common.git",
      build_file = base_dir + "/build_defs/lisp__closure_common.BUILD"
  )

  native.new_git_repository(
      name = "lisp__com_google_base",
      commit = "498fd7224748a1cceaa6127edcedab6e3563aa84",
      remote = "https://github.com/brown/base.git",
      build_file = base_dir + "/build_defs/lisp__com_google_base.BUILD"
  )

  native.new_git_repository(
      name = "lisp__cxml",
      commit = "9365c4b93c3e5adc55a6512b3fb453693d06a707",
      remote = "git://repo.or.cz/cxml.git",
      build_file = base_dir + "/build_defs/lisp__cxml.BUILD"
  )

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
      commit = "c6ef2128a6ac44ba9e9cf0fb34c4ed429b988217",
      remote = "https://gitlab.common-lisp.net/frideau/fare-quasiquote.git",
      build_file = base_dir + "/build_defs/lisp__fare_quasiquote.BUILD"
  )

  native.new_git_repository(
      name = "lisp__fare_utils",
      commit = "66e9c6f1499140bc00ccc22febf2aa528cbb5724",
      remote = "https://gitlab.common-lisp.net/frideau/fare-utils.git",
      build_file = base_dir + "/build_defs/lisp__fare_utils.BUILD"
  )

  native.new_git_repository(
      name = "lisp__flexi_streams",
      commit = "3ce984cf46bac4bdf149a9ccfb6730a638fa11b5",
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
      commit = "e1f6378d75cea9eed243a793efa90cec55e401cb",
      remote = "https://gitlab.common-lisp.net/qitab/inferior-shell.git",
      build_file = base_dir + "/build_defs/lisp__inferior_shell.BUILD"
  )

  native.new_git_repository(
      name = "lisp__ironclad",
      commit = "20d28217e7a9762ca344d46f49cf6b2d99f23494",
      remote = "https://github.com/froydnj/ironclad.git",
      build_file = base_dir + "/build_defs/lisp__ironclad.BUILD"
  )

  native.new_git_repository(
      name = "lisp__lisp_invocation",
      commit = "5134c7354e2c76241731a6c10cb925795ccf2608",
      remote = "https://gitlab.common-lisp.net/qitab/lisp-invocation.git",
      build_file = base_dir + "/build_defs/lisp__lisp_invocation.BUILD"
  )

  native.new_git_repository(
      name = "lisp__local_time",
      commit = "af99a125d826b6a71ff1f110d2bff6b682a4504b",
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
      commit = "84b86d8efb19d0b8814e353238a500097b526bc0",
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
      commit = "f139ae12ed555134b3fe9e2c5bed5836939202ce",
      remote = "https://github.com/sharplispers/parse-number.git",
      build_file = base_dir + "/build_defs/lisp__parse_number.BUILD"
  )

  native.new_git_repository(
      name = "lisp__xpath",
      commit = "7abb91be81d8b18066b37db564862dda34ee4d93",
      remote = "https://github.com/gonzojive/plexippus-xpath.git",
      build_file = base_dir + "/build_defs/lisp__xpath.BUILD"
  )

  native.new_git_repository(
      name = "lisp__poiu",
      commit = "e1ff48ef8b707b9533906e44075dc827c6dedc7e",
      remote = "https://gitlab.common-lisp.net/qitab/poiu.git",
      build_file = base_dir + "/build_defs/lisp__poiu.BUILD"
  )

  native.new_git_repository(
      name = "lisp__ptester",
      commit = "fe69fde54f4bce00ce577feb918796c293fc7253",
      remote = "http://git.kpe.io/ptester.git",
      build_file = base_dir + "/build_defs/lisp__ptester.BUILD"
  )

  native.new_git_repository(
      name = "lisp__puri",
      commit = "b537e93ccfdcbefd8bab075809e4d41b2df779c7",
      remote = "http://git.kpe.io/puri.git",
      build_file = base_dir + "/build_defs/lisp__puri.BUILD"
  )

  native.new_git_repository(
      name = "lisp__slime",
      commit = "38416762c68dfa793f8e4f7c686137ab99b0a18f",
      remote = "https://github.com/slime/slime",
      build_file = base_dir + "/build_defs/lisp__slime.BUILD"
  )

  native.new_git_repository(
      name = "lisp__swank_client",
      commit = "c1d01dd66cbbbe886ee4e38b6f417452e534dc59",
      remote = "https://github.com/brown/swank-client.git",
      build_file = base_dir + "/build_defs/lisp__swank_client.BUILD"
  )

  native.new_git_repository(
      name = "lisp__trivial_backtrace",
      commit = "43ef7d947f4b4de767d0f91f28b50d9c03ad29d6",
      remote = "https://gitlab.common-lisp.net/trivial-backtrace/trivial-backtrace.git",
      build_file = base_dir + "/build_defs/lisp__trivial_backtrace.BUILD"
  )

  native.new_git_repository(
      name = "lisp__trivial_features",
      commit = "29ab1daeb77deb881bf19341cf5f7e41fb246b43",
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
      commit = "2f30276009326a7d785c167ae05914df2cd9ef3e",
      remote = "https://github.com/usocket/usocket.git",
      build_file = base_dir + "/build_defs/lisp__usocket.BUILD"
  )

  native.new_git_repository(
      name = "lisp__workout_timer",
      commit = "bd1be633c70b706e92373cdb123b9ea8fbf13d77",
      remote = "https://gitlab.common-lisp.net/frideau/workout-timer.git",
      build_file = base_dir + "/build_defs/lisp__workout_timer.BUILD"
  )

  lisp_configure()


