load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

# rules_foreign_cc needed by ate-pairing
http_archive(
   name = "rules_foreign_cc",
   strip_prefix = "rules_foreign_cc-master",
   url = "https://github.com/bazelbuild/rules_foreign_cc/archive/master.zip",
)

load("@rules_foreign_cc//:workspace_definitions.bzl", "rules_foreign_cc_dependencies")
rules_foreign_cc_dependencies()

#################################
#### Bazelized external libs ####
local_repository( name = "ate_pairing" , path = "src/camlsnark_c/libsnark-caml/depends/ate-pairing" )
# http_archive(
#     name = "ate_pairing",
#     urls = ["https://github.com/o1-labs/ate-pairing/archive/bzl-1.0.zip"],
#     strip_prefix = "ate-pairing-bzl-1.0",
#     sha256 = ...
# )

local_repository( name = "xbyak" , path = "src/camlsnark_c/libsnark-caml/depends/xbyak" )
# http_archive(
#     name = "xbyak",
#     urls = ["https://github.com/o1-labs/xbyak/archive/bzl-1.0.tar.gz"],
#     strip_prefix = "xbyak-bzl-1.0",
#     # sha256 = ...
# )


## boost needed by: @xbyak//sample:calc
git_repository(
    name = "com_github_nelhage_rules_boost",
    commit = "9f9fb8b2f0213989247c9d5c0e814a8451d18d7f",
    remote = "https://github.com/nelhage/rules_boost",
    shallow_since = "1570056263 -0700",
)
load("@com_github_nelhage_rules_boost//:boost/boost.bzl", "boost_deps")
boost_deps()

##########################################
######## Non-bazel external repos ########
# All non-bazel external repos use build_file = "all_content", but the
# actual build files are in //bzl/external,
# e.g. //bzl/external/libgmp/BUILD.bazel

# Bazel client code deps will thus use labels like @//bzl/external/libgmp

all_content = """filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])"""

## ate-pairing dep: libgmp
http_archive(
    name="libgmp",
    url="https://gmplib.org/download/gmp/gmp-6.2.0.tar.xz",
    sha256="258e6cd51b3fbdfc185c716d55f82c08aff57df0c6fbd143cf6ed561267a1526",
    strip_prefix = "gmp-6.2.0",
    build_file_content = all_content
    # build_file = "@//external:libgmp.BUILD"
)

