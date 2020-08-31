load("@bazel_skylib//lib:selects.bzl", "selects")

# ate_pairing
# CFLAGS_WARN = ["-Wall", "-Wextra", "-Wformat=2",
#                "-Wcast-qual", "-Wcast-align", "-Wwrite-strings",
#                "-Wfloat-equal", "-Wpointer-arith"]

WARNINGS = ["-Wall", "-Wextra", "-Wno-unused-variable"]

DMULTICORE = select({
    "@//bzl/config:with_openmp": ["MULTICORE"],
    "//conditions:default": []
})

DEBUG_FLAGS = select({
    "//bzl/config:enable_debug": ["-g"],
    "//conditions:default": ["-g0"]
}) + select({
    "//bzl/config:enable_verbose": ["-v"],
    "//conditions:default": []
}) + select({
    "//bzl/config:macos_disable_debug": ["-UDEBUG"],
    "//conditions:default": []
})

OPTIMIZE_CXXFLAGS = select({
    "@//bzl/config:enable_optimization": ["-flto", "-fuse-linker-plugin", "-O2"],
    "//conditions:default": ["-O0"]
})

OPTIMIZE_LINKFLAGS = select({
    "@//bzl/config:enable_optimization": ["-flto", "-fuse-linker-plugin"],
    "//conditions:default": []
})

LINKSTATIC = select({
    "@//bzl/host:linux": True,
    "@//bzl/host:macos": False,
    "//conditions:default": True
})

ALWAYSLINK = select({
    "@//bzl/host:linux": True,
    "@//bzl/host:macos": False,
    "//conditions:default": True
})

CPPFLAGS = ["-Iexternal/libff"] + DEBUG_FLAGS + WARNINGS
CFLAGS   = []
CXXFLAGS = OPTIMIZE_CXXFLAGS
LDFLAGS  = []
#######################
####    DEFINES    ####

DCXX_DEBUG = select({
    "@//bzl/config:linux_cxx_debug": ["_GLIBCXX_DEBUG", "_GLIBCXX_DEBUG_PEDANTIC"],
    "@//bzl/config:macos_cxx_debug": ["_LIBCPP_DEBUG"],
    "//conditions:default": []
})

DDEBUG = select({
    "@//bzl/config:enable_debug": ["DEBUG"],
    "//conditions:default": [] # ["NDEBUG"]
}) + DCXX_DEBUG

DBINARY_OUTPUT = select({
    "//bzl/config:enable_binary_output": ["BINARY_OUTPUT"],
    "//conditions:default": []
})

DCURVE = select({
    "@//bzl/config:enable_curve_bn128": ["CURVE_BN128", "BN_SUPPORT_SNARK"],
    "@//bzl/config:enable_curve_alt_bn128": ["CURVE_ALT_BN128"],
    "@//bzl/config:enable_curve_edwards": ["CURVE_EDWARDS"],
    "@//bzl/config:enable_curve_mnt4": ["CURVE_MNT4"],
    "@//bzl/config:enable_curve_mnt6": ["CURVE_MNT6"],
}, no_match_error = "ERROR: no curve specified; try --//:curve=<curve>")


DLOWMEM = select({
    "//bzl/config:enable_lowmem": ["LOWMEM"],
    "//conditions:default": []
})

DMONTGOMERY_OUTPUT = select({
    "//bzl/config:enable_montgomery_output": ["MONTGOMERY_OUTPUT"],
    "//conditions:default": []
})

DNO_PT_COMPRESSION = select({
    "//bzl/config:disable_pt_compression": ["NO_PT_COMPRESSION"],
    "//conditions:default": []
})

DPROFILE_OP_COUNTS = select({
    "//bzl/config:enable_profile_op_counts": ["PROFILE_OP_COUNTS"],
    "//conditions:default": []
})

DUSE_MIXED_ADDITION = select({
    "//bzl/config:enable_mixed_addition": ["USE_MIXED_ADDITION"],
    "//conditions:default": []
})

DNO_PROCPS = selects.with_or({
    ("//bzl/config:without_procps",
     "@//bzl/host:macos"): ["NO_PROCPS"],
    "//conditions:default": []
})

DOPTIMIZE = select({
    "//bzl/config:enable_optimization": ["NDEBUG"],
    "//conditions:default": []
})

DUSE_ASM = select({
    "//bzl/config:enable_asm": ["USE_ASM"],
    "//conditions:default": []
})
