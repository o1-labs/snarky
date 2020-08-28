# xbyak: working with bazel

## using xbyak as dependency

To use `xbyak` in a non-bazel project, follow the instructions in the main readme file.

To use `xbyak` in a Bazel project, add the following to the WORKSPACE file:

```
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
http_archive(
    name = "xbyak",
    urls = ["https://github.com/o1-labs/xbyak.git"]
    # or use a url for a commit or release
    # strip_prefix = ...
    # sha256 = ...
)
```

The dependency label is `@xbyak//xbyak`, e.g. `deps =
["@xbyak//xbyak"]`. If the header files are not found, add
`"-Iexternal/xbyak"` to `copts`.

For an example, see [ate-pairing](https://github.com/o1-labs/ate-pairing/blob/snarky/BUILD.bazel)

## tests

Clone the repo, then list the test targets: `$ bazel query test:all`

Run the test suite: `$ bazel test test`

All tests succeed on linux, test:jmp fails on MacOS.

You can also run the samples:

```
$ bazel query sample:all
$ bazel run sample:<target>
```

where <target> is one of those listed by the sample query.

