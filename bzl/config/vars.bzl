load("@bazel_skylib//lib:selects.bzl", "selects")

#####################
####    FLAGS    ####
WARNINGS = ["-Wall", "-Wextra", "-Wfatal-errors",
            "-Wno-unused-variable",
            "-Wno-unused-parameter"
]

DEBUG_FLAGS = select({
    "//bzl/config:enable_debug": ["-g"],
    "//conditions:default": ["-ggdb3"]
}) + select({
    "//bzl/config:macos_disable_debug": ["-UDEBUG"],
    "//conditions:default": []
})

OPTIMIZE_CXXFLAGS = select({
    "@//bzl/config:enable_optimization": ["-flto", "-fuse-linker-plugin", "-O2"],
    "//conditions:default": ["-O2"]
}) + select({
    "@//bzl/config:enable_debug": ["-g"],
    "//conditions:default": ["-g0"]
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

CPPFLAGS = [
    "-std=c++14",
    "-lstdc++",
    # "-Xpreprocessor", "-fopenmp",
    "-fPIC",
] + DEBUG_FLAGS + WARNINGS
CFLAGS   = []
CXXFLAGS = OPTIMIZE_CXXFLAGS
LDFLAGS  = []

#######################
####    DEFINES    ####
DDEBUG = select({
    "//bzl/config:enable_debug": ["DEBUG", "FOOBAR"],
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

DLIBGMP = select({
    "@//bzl/config:enable_libgmp" : ["MIE_ATE_USE_GMP"],
    "//conditions:default": []
})

DMULTICORE = select({
    "@//bzl/config:enable_multicore": ["MULTICORE"],
    "//conditions:default": []
})

#### LIBFF ####
DCURVE = select({
    "@//bzl/config:enable_curve_bn128": ["CURVE_BN128"],
    "@//bzl/config:enable_curve_alt_bn128": ["CURVE_ALT_BN128"],
    "@//bzl/config:enable_curve_edwards": ["CURVE_EDWARDS"],
    "@//bzl/config:enable_curve_mnt4": ["CURVE_MNT4"],
    "@//bzl/config:enable_curve_mnt6": ["CURVE_MNT6"],
}, no_match_error = "ERROR: no curve specified; try --//:curve=<curve>")

DLOWMEM = select({
    "@//bzl/config:enable_lowmem": ["LOWMEM"],
    "//conditions:default": []
})

DMONTGOMERY_OUTPUT = select({
    "@//bzl/config:enable_montgomery_output": ["MONTGOMERY_OUTPUT"],
    "//conditions:default": []
})

DNO_PT_COMPRESSION = select({
    "@//bzl/config:disable_point_compression": ["NO_PT_COMPRESSION"],
    "//conditions:default": []
})

DPROFILE_OP_COUNTS = select({
    "@//bzl/config:enable_profile_op_counts": ["PROFILE_OP_COUNTS"],
    "//conditions:default": []
})

DUSE_MIXED_ADDITION = select({
    "@//bzl/config:enable_mixed_addition": ["USE_MIXED_ADDITION"],
    "//conditions:default": []
})

DCXX_DEBUG = select({
    "@//bzl/config:enable_cxx_debug": ["_GLIBCXX_DEBUG", "_GLIBCXX_DEBUG_PEDANTIC"],
    "//conditions:default": []
})

DOPTIMIZE = select({
    "@//bzl/config:enable_optimization": ["NDEBUG"],
    "//conditions:default": []
})

DUSE_ASM = select({
    "@//bzl/config:enable_asm": ["USE_ASM"],
    "//conditions:default": []
})
