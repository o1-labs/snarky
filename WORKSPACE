workspace(name = "snarky")

################ setup ################
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository", "new_git_repository")

http_archive(
    name = "bazel_skylib",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.0.2/bazel-skylib-1.0.2.tar.gz",
        "https://github.com/bazelbuild/bazel-skylib/releases/download/1.0.2/bazel-skylib-1.0.2.tar.gz",
    ],
    sha256 = "97e70364e9249702246c0e9444bccdc4b847bed1eb03c5a3ece4f83dfe6abc44",
)
load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")
bazel_skylib_workspace()

http_archive(
    name = "rules_foreign_cc",
    strip_prefix="rules_foreign_cc-master",
    url = "https://github.com/bazelbuild/rules_foreign_cc/archive/master.zip",
    # sha256 = "3fc764c7084da14cff812ae42327d19c8d6e99379f8b3e310b3213e1d5f0e7e8"
)

load("@rules_foreign_cc//:workspace_definitions.bzl", "rules_foreign_cc_dependencies")
rules_foreign_cc_dependencies()

# rules_python needed by libsnark
http_archive(
    name = "rules_python",
    url = "https://github.com/bazelbuild/rules_python/releases/download/0.0.2/rules_python-0.0.2.tar.gz",
    strip_prefix = "rules_python-0.0.2",
    sha256 = "b5668cde8bb6e3515057ef465a35ad712214962f0b3a314e551204266c7be90c",
)

#################################
#### Bazelized external libs ####

local_repository( name = "libsnark" , path = "src/camlsnark_c/libsnark-caml")
# http_archive(
#     name = "libsnark",
#     urls = ["https://github.com/o1-labs/libsnark/archive/bzl-1.0.tar.gz"],
#     strip_prefix = "libsnark-bzl-1.0",
#     # sha256 = ...
# )

local_repository( name = "libfqfft" , path = "src/camlsnark_c/libsnark-caml/depends/libfqfft")
# http_archive(
#     name = "libfqfft",
#     urls = ["https://github.com/o1-labs/libfqfft/archive/bzl-1.0.tar.gz"],
#     strip_prefix = "libfqfft-bzl-1.0",
#     sha256 = ...
# )

local_repository( name = "libff" , path = "src/camlsnark_c/libsnark-caml/depends/libff")
# http_archive(
#     name = "libff",
#     urls = ["https://github.com/o1-labs/libff/archive/bzl-1.0.tar.gz"],
#     strip_prefix = "libff-bzl-1.0",
#     sha256 = ...
# )

# Used only for bn128, in libff/algebra/curves/bn128/BUILD.bazel, target: @ate_pairing//libzm
local_repository( name = "ate_pairing" , path = "src/camlsnark_c/libsnark-caml/depends/ate-pairing")
# http_archive(
#     name = "ate_pairing",
#     urls = ["https://github.com/o1-labs/ate-pairing/archive/bzl-1.0.tar.gz"],
#     strip_prefix = "ate-pairing-bzl-1.0",
#     sha256 = ...
#     # commit: 8d34a92e92b0c661291dfc177f9e2b61c78597c4
# )

local_repository( name = "xbyak" , path = "src/camlsnark_c/libsnark-caml/depends/xbyak" )
# http_archive(
#     name = "xbyak",
#     urls = ["https://github.com/o1-labs/xbyak/archive/bzl-1.0.tar.gz"],
#     strip_prefix = "xbyak-bzl-1.0",
#     # sha256 = ...
# )

##########################################
######## Non-bazel external repos ########
all_content = """filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])"""

# libsnark-caml:
new_git_repository(
    name = "libsnark-supercop",
    commit = "b04a0ea2c7d7422d74a512ce848e762196f48149",
    remote = "https://github.com/mbbarbosa/libsnark-supercop",
    shallow_since = "1433349878 +0100",
    build_file = "@libsnark//bzl/external/libsnark-supercop:BUILD.bazel"
)

# libfqfft:
http_archive(
    name="gtest",
    url="https://github.com/google/googletest/archive/release-1.10.0.tar.gz",
    sha256="9dc9157a9a1551ec7a7e43daea9a694a0bb5fb8bec81235d8a1e6ef64c716dcb",
    strip_prefix = "googletest-release-1.10.0",
)

## build target: //bzl/external/openmp alias for @libff//bzl/external/openmp
http_archive(
    name="openmp",
    url="https://github.com/llvm/llvm-project/releases/download/llvmorg-10.0.0/openmp-10.0.0.src.tar.xz",
    sha256="3b9ff29a45d0509a1e9667a0feb43538ef402ea8cfc7df3758a01f20df08adfa",
    strip_prefix="openmp-10.0.0.src",
    build_file_content = all_content
)

## build target: //bzl/external/openssl aliased for @libff/bzl/external/openssl
http_archive(
    name="openssl",
    url="https://www.openssl.org/source/openssl-1.1.1g.tar.gz",
    sha256="ddb04774f1e32f0c49751e21b67216ac87852ceb056b75209af2443400636d46",
    strip_prefix="openssl-1.1.1g",
    build_file_content = all_content
)

## build target: //bzl/external/libsodium alias for @libff//bzl/external/libsodium
http_archive(
    name="libsodium",
    type="zip",
    url="https://github.com/jedisct1/libsodium/archive/1.0.18-RELEASE.zip",
    sha256="7728976ead51b0de60bede2421cd2a455c2bff3f1bc0320a1d61e240e693bce9",
    strip_prefix = "libsodium-1.0.18-RELEASE",
    build_file_content = all_content,
)

# not used by snarky; build:  @libff//bzl/external/procps
new_git_repository(
    name = "procps",
    commit = "4090fa711be367a35e689a34c9ba751ad90f6f0d",
    remote = "https://github.com/obazl/procps.git",
    shallow_since = "1588067259 +1000",
    build_file_content = all_content,
)

## build target: //bzl/external/libgmp alias for @ate_pairing//bzl/external/libgmp
http_archive(
    name="libgmp",
    url="https://gmplib.org/download/gmp/gmp-6.2.0.tar.xz",
    sha256="258e6cd51b3fbdfc185c716d55f82c08aff57df0c6fbd143cf6ed561267a1526",
    strip_prefix = "gmp-6.2.0",
    build_file_content = all_content
)

## boost needed by: libsnark, @xbyak//sample:calc
git_repository(
    name = "com_github_nelhage_rules_boost",
    commit = "9f9fb8b2f0213989247c9d5c0e814a8451d18d7f",
    remote = "https://github.com/nelhage/rules_boost",
    shallow_since = "1570056263 -0700",
)
load("@com_github_nelhage_rules_boost//:boost/boost.bzl", "boost_deps")
boost_deps()
