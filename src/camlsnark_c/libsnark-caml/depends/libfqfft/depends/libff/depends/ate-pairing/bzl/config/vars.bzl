load("@bazel_skylib//lib:selects.bzl", "selects")

# CFLAGS = -fPIC -O3 -fomit-frame-pointer -DNDEBUG -msse2 -mfpmath=sse -march=native
# ifeq ($(DBG),on)
# CFLAGS += -O0 -g3 -UNDEBUG
# LDFLAGS += -g3
# endif

CFLAGS_BASE = [
    "-std=c++14 -lstdc++ -fPIC", "-fomit-frame-pointer", "-msse2", "-mfpmath=sse",  "-march=native",
] + select({
    "//bzl/config:enable_debug" : ["-O0", "-g3"],
    "//conditions:default" : ["-O3", "-g0"]
}) + select({
    "//bzl/config:macos_disable_debug": ["-UDEBUG"],
    "//conditions:default" : []
}) + select({
    "//bzl/config:enable_verbose" : ["-v"],
    "//conditions:default" : []
}) + select({
    # if needed, test for 32 bit
    "//conditions:default" : ["-m64"]
})
# + select({
#     "//:vuint_bit_len_1024" : ["-DMIE_ZM_VUINT_BIT_LEN=1024"],
#     "//conditions:default": ["-DMIE_ZM_VUINT_BIT_LEN=576"],
# })

# CFLAGS_WARN=-Wall -Wextra -Wformat=2 -Wcast-qual -Wcast-align -Wwrite-strings -Wfloat-equal -Wpointer-arith #-Wswitch-enum -Wstrict-aliasing=2

CFLAGS_WARN = [
    "-Wall",
    "-Wextra",
    "-Wformat=2",
    "-Wcast-qual",
    "-Wcast-align",
    "-Wwrite-strings",
    "-Wfloat-equal",
    "-Wpointer-arith",
]

CFLAGS = CFLAGS_BASE + CFLAGS_WARN

# CFLAGS_ALWAYS = -D_FILE_OFFSET_BITS=64 -DMIE_ATE_USE_GMP
DEFINES = [
    "_FILE_OFFSET_BITS=64"  # , "MIE_ATE_USE_GMP"
] + select({
    "//bzl/config:with_libgmp" : ["MIE_ATE_USE_GMP"],
    "//conditions:default": []
})

DDEBUG = select({
    # clang:  -fdebug-macro
    "//bzl/config:enable_debug" : ["DEBUG"],
    "//conditions:default": ["NDEBUG"]
})

SNARK = select({
    "@//:enable_snark" : ["BN_SUPPORT_SNARK=1"],
    "//conditions:default": []
})

# ifneq ($(VUINT_BIT_LEN),)
# CFLAGS += -D"MIE_ZM_VUINT_BIT_LEN=$(VUINT_BIT_LEN)"
# endif


LINK_STATIC_ONLY = select({
    "//bzl/host:linux": True,
    "//bzl/host:macos": False,
    "//conditions:default": True
})
