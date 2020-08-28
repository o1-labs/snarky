load("@bazel_skylib//lib:selects.bzl", "selects")

# ate_pairing
# CFLAGS_WARN = ["-Wall", "-Wextra", "-Wformat=2",
#                "-Wcast-qual", "-Wcast-align", "-Wwrite-strings",
#                "-Wfloat-equal", "-Wpointer-arith"]

###############################
####    -I SEARCH PATHS    ####

# -Iexternal ensures < > paths will work for imported repos
HDR_SEARCH_PATHS = ["-I.", "-Iexternal"]

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
    "//bzl/config:enable_verbose": ["-v"],
    "//conditions:default": []
}) + select({
    "//bzl/config:macos_disable_debug": ["-UDEBUG"],
    "//conditions:default": []
})

OPTIMIZE_CXXFLAGS = select({
    "//bzl/config:enable_optimization": ["-flto", "-fuse-linker-plugin", "-O2"],
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
CXXFLAGS = OPTIMIZE_CXXFLAGS # + ["-lstd=c++14", "-lstdc++", "-D_LIBCXX_DEPRECATION_WARNINGS "]
# CMAKE_CXX_FLAGS = ["-std=c++11",
# CXX_FLAGS = ["-std=c++11",
#              "-Wall", "-Wextra", "-Wfatal-errors",
#              "-Wno-unused-variables"
# ]

# OPT_FLAGS = ["-ggdb3", "-O2", "-march=native", "-mtune=native"]

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

# DBINARY_OUTPUT = select({
#     "//bzl/config:enable_binary_output": ["BINARY_OUTPUT"],
#     "//conditions:default": []
# })

DMULTICORE = select({
    "//bzl/config:with_openmp": ["MULTICORE"],
    "//conditions:default": []
})

#### LIBFF ####
DCURVE = select({
    # "//bzl/config:enable_curve_bn128": ["CURVE_BN128"],
    # "//bzl/config:enable_curve_alt_bn128": ["CURVE_ALT_BN128"],
    # "//bzl/config:enable_curve_edwards": ["CURVE_EDWARDS"],
    # "//bzl/config:enable_curve_mnt4": ["CURVE_MNT4"],
    # "//bzl/config:enable_curve_mnt6": ["CURVE_MNT6"],
    "//conditions:default": ["CURVE_BN128"]
})

# DPROFILE_OP_COUNTS = select({
#     "//bzl/config:enable_profile_op_counts": ["PROFILE_OP_COUNTS"],
#     "//conditions:default": []
# })

DPROF_DOUBLE = select({
    "//bzl/config:enable_prof_double": ["PROF_DOUBLE"],
    "//conditions:default": []
})

# DPROFILING = DPROFILE_OP_COUNTS + DPROF_DOUBLE

DCXX_DEBUG = select({
    "//bzl/config:enable_cxx_debug": ["_GLIBCXX_DEBUG", "_GLIBCXX_DEBUG_PEDANTIC"],
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
