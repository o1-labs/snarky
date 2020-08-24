load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

#################################
#### Bazelized external libs ####
local_repository( name = "xbyak" , path = "src/camlsnark_c/libsnark-caml/depends/xbyak" )
# http_archive(
#     name = "xbyak",
#     urls = ["https://github.com/obazl/xbyak/archive/bzl-1.0.tar.gz"],
#     strip_prefix = "xbyak-bzl-1.0",
#     # sha256 = 
# )

##########################################
######## Non-bazel external repos ########

## boost needed by: @xbyak//sample:calc
git_repository(
    name = "com_github_nelhage_rules_boost",
    commit = "9f9fb8b2f0213989247c9d5c0e814a8451d18d7f",
    remote = "https://github.com/nelhage/rules_boost",
    shallow_since = "1570056263 -0700",
)
load("@com_github_nelhage_rules_boost//:boost/boost.bzl", "boost_deps")
boost_deps()
