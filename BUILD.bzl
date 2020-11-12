REPO_ALWAYS_LINK = select({
    "@//:enable_always_link": ["-linkall"],
    "//conditions:default": [],
})

REPO_DEBUG = select({
    "@//:enable_debug": ["-g"],
    "//conditions:default": [],
})

REPO_THREADS = select({
    "@//:enable_threads": ["-thread"],
    "//conditions:default": [],
})

REPO_VERBOSE = select({
    "@//:enable_verbose": ["-verbose"],
    "//conditions:default": [],
})

## FIXME: settle on defaults
# DEFAULT_WARNINGS = ["-w", "+a-4-6-7-9-27-29-32..42-44-45-48-50-60"]
# WARNINGS = ["-w", "@a-4-29-40-41-42-44-45-48-58-59-60"]
# MODULE_WARNINGS = ["-w", "@1..3@5..28@30..39@43@46..47@49..57@61..62-40"]

REPO_DEFAULT_OPTS    = ["-strict-formats", "-short-paths", "-keep-locs"]

REPO_OPTS            = REPO_DEFAULT_OPTS + REPO_THREADS + REPO_ALWAYS_LINK + REPO_VERBOSE + REPO_DEBUG
REPO_ARCHIVE_OPTS    = REPO_OPTS
REPO_EXECUTABLE_OPTS = REPO_OPTS
REPO_INTERFACE_OPTS  = REPO_OPTS
REPO_MODULE_OPTS     = REPO_OPTS
REPO_NS_MODULE_OPTS  = REPO_OPTS

REPO_PPX_ARCHIVE_OPTS    = REPO_OPTS
REPO_PPX_EXECUTABLE_OPTS = REPO_OPTS + ["-linkall", "-thread", "-predicates", "ppx_driver"]
REPO_PPX_INTERFACE_OPTS  = REPO_OPTS
REPO_PPX_MODULE_OPTS     = REPO_OPTS
