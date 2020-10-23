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
REPO_MODULE_OPTS     = REPO_OPTS
REPO_NS_MODULE_OPTS  = REPO_OPTS

REPO_PPX_ARCHIVE_OPTS    = REPO_OPTS
REPO_PPX_EXECUTABLE_OPTS = REPO_OPTS
REPO_PPX_INTERFACE_OPTS  = REPO_OPTS
REPO_PPX_MODULE_OPTS     = REPO_OPTS
