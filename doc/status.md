# Bazel build status

**WARNING** This is for the deprecated C/C++ backend. Not up to date.

## app/reformat-snarky

`$ bazel build app/reformat-snarky:reformat.exe` succeeds and runs.

## bench

`$ bazel build bench:main.exe` succeeds. Run fails.

## Examples

* examples/election:election_main builds and runs
* examples/anonvote build fails, with 'No implementation' for Digestif, see below.
* examples/election_gpu untested
* examples/elliptic_curve_operations builds and runs
* examples/imperative_test: builds and runs
* examples/merkle_update builds and runs
* examples/sfbw: all builds fail with 'No implementation' for Digestif, see below
* examples/tutorial builds and runs

## jstest

`$ bazel build jstest/src:run_snarky.exe` fails with 'No implementation' for Digestif, see below.

## meja

Not yet supported.


## Errors

`Invalid_argument Filename.chop_extension` example:

```
$ ./bazel-bin/examples/imperative_test/imperative_test
Uncaught exception:

  (Invalid_argument Filename.chop_extension)

Raised at file "stdlib.ml", line 34, characters 20-45
Called from file "src/toplevel.ml", line 5, characters 11-74
```

This comes from linking in toplevel.ml


No implementation for Digestif:

```
Error: No implementations provided for the following modules:
         Digestif referenced from bazel-out/darwin-fastbuild/bin/snarky_curve/Snarky_curve.cmx
```

You need to provide an implementation, either `digestif.c` or `digestif.ocaml`.
