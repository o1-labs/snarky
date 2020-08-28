workspace(name = "libff")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
# load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

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

################################
##  Bazelized external repos  ##

# To use submodules installed in "depends/", use the
# `local_repository` rules; otherwise, use the `http_archive` rules.

# Used only for bn128, in libff/algebra/curves/bn128/BUILD.bazel, target: @ate_pairing//libzm
local_repository( name = "ate_pairing" , path = "depends/ate-pairing")
# http_archive(
#     name = "ate_pairing",
#     urls = ["https://github.com/obazl/ate-pairing/archive/bzl-1.0.tar.gz"],
#     strip_prefix = "ate-pairing-bzl-1.0",
#     sha256 = "e89a6a33eda28e93ae616b57ba5d4693f7b434b4d3407462caaab46a535d35ad"
#     # commit: 8d34a92e92b0c661291dfc177f9e2b61c78597c4
# )

## build target: @xbyak//xbyak
local_repository( name = "xbyak" , path = "depends/xbyak")
# http_archive(
#     name = "xbyak",
#     urls = ["https://github.com/obazl/xbyak/archive/bzl-1.0.tar.gz"],
#     strip_prefix = "xbyak-bzl-1.0",
#     sha256 = "84fc1e7a73ec9077b05516422775ac90086ef45976aaf43f65368529cb71a75d"
#     # commit: 89f7b734cb934518f12cb5836e6aca18f999172a
# )

################################
# Non-bazelized external repos #

all_content = """filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])"""

## build target: //bzl/external/openmp
http_archive(
    name="openmp",
    url="https://github.com/llvm/llvm-project/releases/download/llvmorg-10.0.0/openmp-10.0.0.src.tar.xz",
    sha256="3b9ff29a45d0509a1e9667a0feb43538ef402ea8cfc7df3758a01f20df08adfa",
    strip_prefix="openmp-10.0.0.src",
    build_file_content = all_content
)

## build target: //bzl/external/openssl
http_archive(
    name="openssl",
    url="https://www.openssl.org/source/openssl-1.1.1g.tar.gz",
    sha256="ddb04774f1e32f0c49751e21b67216ac87852ceb056b75209af2443400636d46",
    strip_prefix="openssl-1.1.1g",
    build_file_content = all_content
)

## build target: //bzl/external/libsodium
http_archive(
    name="libsodium",
    type="zip",
    url="https://github.com/jedisct1/libsodium/archive/1.0.18-RELEASE.zip",
    sha256="7728976ead51b0de60bede2421cd2a455c2bff3f1bc0320a1d61e240e693bce9",
    strip_prefix = "libsodium-1.0.18-RELEASE",
    build_file_content = all_content,
)

## CURRENTLY BROKEN: gitlab returns 406 Not Acceptable
## build target: @libff//bzl/external/procps
# http_archive(
#     name="procps",
#     url="https://gitlab.com/procps-ng/procps/-/archive/v3.3.16/procps-v3.3.16.tar.gz",
#     sha256="7f09945e73beac5b12e163a7ee4cae98bcdd9a505163b6a060756f462907ebbc",
#     strip_prefix = "procps-v3.3.16",
#     build_file_content = all_content
# )

## build target: //bzl/external/libgmp alias for @ate_pairing//bzl/external/libgmp
http_archive(
    name="libgmp",
    url="https://gmplib.org/download/gmp/gmp-6.2.0.tar.xz",
    sha256="258e6cd51b3fbdfc185c716d55f82c08aff57df0c6fbd143cf6ed561267a1526",
    strip_prefix = "gmp-6.2.0",
    build_file_content = all_content
)
