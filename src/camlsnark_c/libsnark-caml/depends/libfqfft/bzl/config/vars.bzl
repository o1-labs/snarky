load("@bazel_skylib//lib:selects.bzl", "selects")

# ate_pairing
# CFLAGS_WARN = ["-Wall", "-Wextra", "-Wformat=2",
#                "-Wcast-qual", "-Wcast-align", "-Wwrite-strings",
#                "-Wfloat-equal", "-Wpointer-arith"]

#####################
####    FLAGS    ####
WARNINGS = ["-Wno-unused-variable"]

# compile/link flags
# "${CMAKE_CXX_FLAGS} -std=c++11 -Wall -Wextra -Wfatal-errors -pthread"
# opt flags:    "-ggdb3 -O2 -march=native -mtune=native"

DEBUG_FLAGS = select({
    "//bzl/config:enable_debug": ["-g"],
    "//conditions:default": ["-g0"]
}) + select({
    "//bzl/config:with_verbose": ["-v"],
    "//conditions:default": []
}) + select({
    "//bzl/config:macos_disable_debug": ["-UDEBUG"],
    "//conditions:default": []
})

OPTIMIZE_CXXFLAGS = select({
    "@//bzl/config:enable_optimization": ["-flto", "-fuse-linker-plugin", "-O2"],
    "//conditions:default": []
}) + select({
    "//bzl/config:enable_debug": ["-g"],
    "//conditions:default": ["-g0"]
})

OPTIMIZE_LINKFLAGS = select({
    "//bzl/config:enable_optimization": ["-flto", "-fuse-linker-plugin"],
    "//conditions:default": []
})

CPPFLAGS = DEBUG_FLAGS + WARNINGS
CFLAGS   = []
CXXFLAGS = OPTIMIZE_CXXFLAGS # + ["-lstd=c++14", "-lstdc++"]
LDFLAGS  = []

#######################
####    DEFINES    ####
DDEBUG = select({
    "//bzl/config:enable_debug": ["DEBUG"],
    "//conditions:default": ["NDEBUG"]
})

UDEBUG = select({
    "//bzl/config:macos_no_debug": ["-UDEBUG"],
    "//conditions:default": []
})

DBINARY_OUTPUT = select({
    "@//bzl/config:enable_binary_output": ["BINARY_OUTPUT"],
    "//conditions:default": []
})

# DLIBGMP = select({
#     "@//bzl/config:enable_libgmp" : ["MIE_ATE_USE_GMP"],
#     "//conditions:default": []
# })

DMULTICORE = select({
    "@//bzl/config:with_openmp": ["MULTICORE"],
    "//conditions:default": []
})

#### LIBFF ####
DCURVE = select({
    "@//bzl/config:curve_bn128": ["CURVE_BN128"],
    "@//bzl/config:curve_alt_bn128": ["CURVE_ALT_BN128"],
    "@//bzl/config:curve_edwards": ["CURVE_EDWARDS"],
    "@//bzl/config:curve_mnt4": ["CURVE_MNT4"],
    "@//bzl/config:curve_mnt6": ["CURVE_MNT6"],
    "//conditions:default": ["CURVE_BN128"]
})

# DLOWMEM = select({
#     "@//bzl/config:enable_lowmem": ["LOWMEM"],
#     "//conditions:default": []
# })

# DMONTGOMERY_OUTPUT = select({
#     "@//bzl/config:enable_montgomery_output": ["MONTGOMERY_OUTPUT"],
#     "//conditions:default": []
# })

# DNO_PT_COMPRESSION = select({
#     "@//bzl/config:disable_point_compression": ["NO_PT_COMPRESSION"],
#     "//conditions:default": []
# })

# DPROFILE_OP_COUNTS = select({
#     "@//bzl/config:enable_profile_op_counts": ["PROFILE_OP_COUNTS"],
#     "//conditions:default": []
# })

# DUSE_MIXED_ADDITION = select({
#     "@//bzl/config:enable_mixed_addition": ["USE_MIXED_ADDITION"],
#     "//conditions:default": []
# })

DNO_PROCPS = selects.with_or({
    ("@//bzl/config:disable_procps", "@//bzl/host:macos"): ["NO_PROCPS"],
    "//conditions:default": []
})

DCXX_DEBUG = select({
    "@//bzl/config:enable_cxx_debug": ["_GLIBCXX_DEBUG", "_GLIBCXX_DEBUG_PEDANTIC"],
    "//conditions:default": []
})

# # -DNDEBUG
# #     "${CMAKE_CXX_FLAGS} -flto -fuse-linker-plugin"
#     # "${CMAKE_EXE_LINKER_FLAGS} -flto"

DOPTIMIZE = select({
    "@//bzl/config:enable_optimization": ["NDEBUG"],
    "//conditions:default": []
})

# DUSE_ASM = select({
#     "@//bzl/config:enable_asm": ["USE_ASM"],
#     "//conditions:default": []
# })
