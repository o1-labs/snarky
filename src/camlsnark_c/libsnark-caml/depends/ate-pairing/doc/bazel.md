# ate-pairing: working with bazel

## using ate-pairing as dependency

To use `ate-pairing` in a non-bazel project, follow the instructions in the main readme file.

To use `ate-pairing` in a Bazel project, add the following to the WORKSPACE file:

```
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
http_archive(
    name = "ate-pairing",
    urls = ["https://github.com/o1-labs/ate-pairing.git"]
    # or use a url for a commit or release
    # strip_prefix = ...
    # sha256 = ...
)
```

`ate-pairing` is the name of the repo; the build product is `libzm.a`
and `libzm.so`. The Bazel dependency label is `@ate-pairing//libzm`, e.g.
`deps = ["@ate-pairing//libzm"]`. If the header files are not
found, add `"-Iexternal/libzm"` to `copts`.

For an example, see [ate-pairing](https://github.com/o1-labs/ate-pairing/blob/snarky/BUILD.bazel)

## build configuration options

* `--//:snark` - use CurveSNARK from the `xbyak` library. (see main
  [readme file of xbyak](https://github.com/o1-labs/xbyak) for
  explanation). Default: False, use CurveFp254BNb.

* `--/:with_libgmp` - compile libzm with libgmp support.  Default: True.

When using `ate-pairing` as a dependency in a Bazelized project, you may
copy the `bool_flag` and `config_setting` definitions from the `ate-pairing`
root `BUILD.bazel` file to your project's root `BUILD.bazel` file.
This will allow the user to configure builds using e.g.
`--//:with_libgmp` instead of `--@ate-pairing//:with_libgmp`. It also allows
you to override the defaults. See
[libff](https://github.com/o1-labs/libff/blob/snarky/BUILD.bazel)
for examples. It overrides `enable_snark` to default True.

## tests

Clone the repo, then list the test targets:

```
$ bazel query test:all
```

The test suites are `test:CurveSNARK` and `test:CurveNoSNARK`. Both
should succeed on both Linux and MacOS.

`xbyak` is a dependency of `ate-pairing`; you can run its tests and samples too:

```
$ bazel query @xbyak/test:all
$ bazel test @xbyak//test
$ bazel query @xbyak//sample:all
$ bazel run @xbyak//sample:<target>
```

where <target> is one of those listed by the sample query.

## queries

List all test targets:

* ` bazel query "kind(.*_test, //...:*)"`

List all rules in package `sample`:

* ` bazel query "sample:all"`

