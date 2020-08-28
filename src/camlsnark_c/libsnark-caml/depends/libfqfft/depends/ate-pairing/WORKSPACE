workspace(name = "ate_pairing")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

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
    name = "rules_cc",
    sha256 = "e75dfb05bc1e89ebbb6696cadb5e455833690009310d9dc5512151c5adb0e4e3",
    strip_prefix = "rules_cc-cfe68f6bc79dea602f2f6a767797f94a5904997f",
    urls = [
        "https://github.com/bazelbuild/rules_cc/archive/cfe68f6bc79dea602f2f6a767797f94a5904997f.zip",
    ],
)

http_archive(
    name = "rules_foreign_cc",
    strip_prefix="rules_foreign_cc-master",
    url = "https://github.com/bazelbuild/rules_foreign_cc/archive/master.zip",
    # sha256 = "55b7c4678b4014be103f0e93eb271858a43493ac7a193ec059289fbdc20b9023",
)
load("@rules_foreign_cc//:workspace_definitions.bzl", "rules_foreign_cc_dependencies")
rules_foreign_cc_dependencies()

################################################################
http_archive(
    name = "xbyak",
    urls = ["https://github.com/o1-labs/xbyak/archive/bzl-1.0.tar.gz"],
    strip_prefix = "xbyak-bzl-1.0",
    # sha256 =
)

################################################################
all_content = """filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])"""

# build target: //bzl/external/libgmp
http_archive(
    name="libgmp",
    url="https://gmplib.org/download/gmp/gmp-6.2.0.tar.xz",
    sha256="258e6cd51b3fbdfc185c716d55f82c08aff57df0c6fbd143cf6ed561267a1526",
    strip_prefix = "gmp-6.2.0",
    build_file_content = all_content
)

################################
# Status:
# swig + pcre, for the java package not yet supported


# http_archive(
#     name = "pcre",
#     urls = [
#         "https://mirror.bazel.build/ftp.pcre.org/pub/pcre/pcre-8.43.tar.gz",
#         "https://ftp.pcre.org/pub/pcre/pcre-8.43.tar.gz",
#     ],
#     sha256 = "0b8e7465dc5e98c757cc3650a20a7843ee4c3edf50aaf60bb33fd879690d2c73",
#     strip_prefix = "pcre-8.43",
#     build_file_content = all_content
# )

# http_archive(
#     name="swig",
#     urls=["http://prdownloads.sourceforge.net/swig/swig-4.0.1.tar.gz"],
#     sha256="7a00b4d0d53ad97a14316135e2d702091cd5f193bb58bcfcd8bc59d41e7887a9",
#     strip_prefix = "swig-4.0.1",
#     build_file_content = all_content
# )
