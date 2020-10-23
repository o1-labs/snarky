EXAMPLES_OPTS = select({
    "@//bzl/host:linux": ["-cclib", "-lstdc++"],
    "@//bzl/host:macos": []
}, no_match_error = "Snarky EXAMPLES_OPTS: unsupported platform.  Linux or MacOS only.")

