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

## build configuration options

* `--//:snark` - use CurveSNARK (see main readme file for
  explanation). Default: False, use CurveFp254BNb.

* `--/:with_libgmp` - compile libzm with libgmp support.  Default: True.

When using `xbyak` as a dependency in a Bazelized project, you may
copy the `bool_flag` and `config_setting` definitions from the `xbyak`
root `BUILD.bazel` file to your project's root `BUILD.bazel` file.
This will allow the user to configure builds using e.g.
`--//:with_libgmp` instead of `--@xbyak//:with_libgmp`. It also allows
you to override the defaults. See
[ate-pairing](https://github.com/o1-labs/ate-pairing/blob/snarky/BUILD.bazel)
and [libff](https://github.com/o1-labs/libff/blob/snarky/BUILD.bazel)
for examples. The latter overrides `enable_snark` to default True.

## tests

Clone the repo, then:

```
$ bazel test test
```

All tests succeed on linux, test:jmp fails on MacOS.

You can also run the samples:

```
$ bazel run sample:<target>
```

where <target> = bf | calc | jmp_table | memfunc | quantize | static_buf | test | test_util | toyvm

## queries

List all test targets:

* ` bazel query "kind(.*_test, //...:*)"`

List all rules in package `sample`:

* ` bazel query "sample:all"`

