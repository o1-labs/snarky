ALWAYS_LINK = select({
    "//bzl/config:enable_always_link": ["-linkall"],
    "//conditions:default": [],
})

DEBUG = select({
    "//bzl/config:enable_debug": ["-g"],
    "//conditions:default": [],
})

THREADS = select({
    "//bzl/config:enable_threads": ["-thread"],
    "//conditions:default": [],
})

VERBOSE = select({
    "//bzl/config:enable_verbose": ["-verbose"],
    "//conditions:default": [],
})

GLOBAL_OPTS = THREADS + ALWAYS_LINK + VERBOSE + DEBUG

## rule options
WARNINGS = ["-w", "@a-4-29-40-41-42-44-45-48-58-59-60"]
MODULE_WARNINGS = ["-w", "@1..3@5..28@30..39@43@46..47@49..57@61..62-40"]
OPTS = ["-strict-formats", "-short-paths", "-keep-locs"]

GLOBAL_ARCHIVE_OPTS = WARNINGS + OPTS + GLOBAL_OPTS
GLOBAL_EXECUTABLE_OPTS = WARNINGS + OPTS + GLOBAL_OPTS
GLOBAL_INTERFACE_OPTS = WARNINGS + OPTS + GLOBAL_OPTS
GLOBAL_MODULE_OPTS = WARNINGS + OPTS + GLOBAL_OPTS + [
    "-w", "-49", # ignore Warning 49: no cmi file was found in path for module x
    "-no-alias-deps", # lazy linking
    "-opaque"         #  do not generate cross-module optimization information
]
GLOBAL_NS_MODULE_OPTS = WARNINGS + OPTS + GLOBAL_OPTS

PPX_ARCHIVE_OPTS = []
PPX_EXECUTABLE_OPTS = []
PPX_MODULE_OPTS = []

################################################################
## FIXME
GLOBAL_CLI_OPTS = GLOBAL_OPTS
ARCHIVE_OPTS = WARNINGS + OPTS + GLOBAL_OPTS
EXECUTABLE_OPTS = WARNINGS + OPTS + GLOBAL_OPTS
INTERFACE_OPTS = WARNINGS + OPTS + GLOBAL_OPTS
MODULE_OPTS = WARNINGS + OPTS + GLOBAL_OPTS + [
    "-w", "-49", # ignore Warning 49: no cmi file was found in path for module x
    "-no-alias-deps", # lazy linking
    "-opaque"         #  do not generate cross-module optimization information
]
NS_MODULE_OPTS = WARNINGS + OPTS + GLOBAL_OPTS

