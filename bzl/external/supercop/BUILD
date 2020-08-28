load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")

cc_library(
    name = "supercop",
    visibility = ["//visibility:public"],
    srcs = glob(["**/*.c", "**/*.s"]),
    hdrs = glob(["**/*.h"]),
    copts = ["-Iexternal/supercop", "-Iexternal/supercop/include"],
    # NOTE: libssl is included in the libfqfft dep, so
    # we reference it that way, but we reference the repo
    # in our WORKSPACE, since Bazel does not yet support
    # transitive workspaces.
    # deps = ["@libfqfft//bzl:openssl"]
)

