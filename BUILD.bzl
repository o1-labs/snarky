GLOBAL_ALWAYS_LINK = select({
    "@//:enable_always_link": ["-linkall"],
    "//conditions:default": [],
})

GLOBAL_DEBUG = select({
    "@//:enable_debug": ["-g"],
    "//conditions:default": [],
})

GLOBAL_THREADS = select({
    "@//:enable_threads": ["-thread"],
    "//conditions:default": [],
})

GLOBAL_VERBOSE = select({
    "@//:enable_verbose": ["-verbose"],
    "//conditions:default": [],
})

# DEFAULT_WARNINGS = ["-w", "+a-4-6-7-9-27-29-32..42-44-45-48-50-60"]
# WARNINGS = ["-w", "@a-4-29-40-41-42-44-45-48-58-59-60"]
# MODULE_WARNINGS = ["-w", "@1..3@5..28@30..39@43@46..47@49..57@61..62-40"]

GLOBAL_DEFAULT_OPTS    = ["-strict-formats", "-short-paths", "-keep-locs"]

GLOBAL_OPTS            = GLOBAL_DEFAULT_OPTS + GLOBAL_THREADS + GLOBAL_ALWAYS_LINK + GLOBAL_VERBOSE + GLOBAL_DEBUG
GLOBAL_ARCHIVE_OPTS    = GLOBAL_OPTS
GLOBAL_EXECUTABLE_OPTS = GLOBAL_OPTS
GLOBAL_INTERFACE_OPTS  = GLOBAL_OPTS
GLOBAL_MODULE_OPTS     = GLOBAL_OPTS + [
    "-w", "-49",      # ignore Warning 49: no cmi file was found in path for module x
    "-no-alias-deps", # lazy linking
    "-opaque"         #  do not generate cross-module optimization information
]
GLOBAL_NS_MODULE_OPTS  = GLOBAL_OPTS

################################################################
REPO_ALWAYS_LINK = select({
    "//:enable_always_link": ["-linkall"],
    "//conditions:default": [],
})

REPO_DEBUG = select({
    "//:enable_debug": ["-g"],
    "//conditions:default": [],
})

REPO_THREADS = select({
    "//:enable_threads": ["-thread"],
    "//conditions:default": [],
})

REPO_VERBOSE = select({
    "//:enable_verbose": ["-verbose"],
    "//conditions:default": [],
})

REPO_OPTS            = REPO_THREADS + REPO_ALWAYS_LINK + REPO_VERBOSE + REPO_DEBUG
REPO_ARCHIVE_OPTS    = REPO_OPTS
REPO_EXECUTABLE_OPTS = REPO_OPTS
REPO_INTERFACE_OPTS  = REPO_OPTS
REPO_MODULE_OPTS     = REPO_OPTS + [
    "-w", "-49",      # ignore Warning 49: no cmi file was found in path for module x
    "-no-alias-deps", # lazy linking
    "-opaque"         #  do not generate cross-module optimization information
]
REPO_NS_MODULE_OPTS  = REPO_OPTS
