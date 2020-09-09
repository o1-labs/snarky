
ToC:

* [Working with Bazel](#working_with_bazel)
  * [Building and Querying](#build_query)
  * [Debugging and Troubleshooting](#debugging)
  * [Configuration](#config)
  * [Known Problems](#known_problems)
  * [Cleaning](#cleaning)
* [Notes on Bazel Build Structure](#build_structure)
  * [Libraries: embedded, submodularized, bazelized](#libs)
  * [Embedded deps](#embedded)
  * [Submodules](#submodules)
  * [External Repos](#externals)
  * [Specific Structures](#specifics)

# Prerequisites

(See the main docs)

## Linux

* procps:
  * sudo apt install:
    * libtool-bin
    * autopoint

* Graphviz

# <a name="working_with_bazel">Working with Bazel</a>

Prerequisite reading: [Workspaces, packages, and targets](https://docs.bazel.build/versions/master/build-ref.html#packages_targets)

## <a name="build_query">Building and Querying</a>

Bazel build commands are simple: `$ bazel build //my/pkg:target`.
Usually the `//` can be omitted, and often the target name is the same
as the last segment of the packge string, so you will do something
like `$ bazel build foo/bar`.

Query commands are a little more complicated. You can find examples
[here](https://docs.bazel.build/versions/master/query-how-to.html);
see also the
[reference](https://docs.bazel.build/versions/master/query.html). Here
are some queries you may find useful (replace '//test' with a target,
e.g. '@libff/libff`):

List all deps of a target, filtering out everything else

* `bazel query "filter (//test, deps(//test/...))"`

This will also show file deps.  To filter those out, showing only rules:

* `bazel query "kind(rule, filter (test, deps(test/...)))"`

But the filter is not needed. This gives the same result:

* `bazel query "kind(rule, //test/...)"`

This is the query to use to show only build rules in a package.

We can limit the output to specific rule types.  First, show all rule types:

* `bazel query "kind(rule, //test/...)" --output label_kind:`

Now narrow the output:

* `bazel query 'kind("cc_bin.* rule", //test/...)'`

We can combine predicates:

```
 bazel query "deps(//ate-pairing/...)" --output label_kind
 bazel query "kind(rule, ate-pairing)"
 bazel query "kind (rule, deps(//ate-pairing/...))" --output label_kind | sort
```

The last excludes source file targets.

Samples/examples are defined using `cc_binary` rules; to list them:

* `bazel query 'kind("cc_binary rule", @libsnark//libsnark/...:all)'`

Tests are defined using either `cc_test`, `python_test`, or `test_suite`.

Finally, you can pipe the output to [GraphViz](https://graphviz.org/)
to create a nice SVG picture of your build structure; see [Visualize
your
build](https://blog.bazel.build/2015/06/17/visualize-your-build.html)
for some simple examples.

## <a name="debugging">Debugging and Troubleshooting</a>

Bazel has its own work area and keeps track of lots of info; you can
see some of it by running `$ bazel info`. Often you will be interested
in `execution_root`, which is where you can find, for example, the
external repos Bazel has downloaded. If Bazel complains about missing
files, for example, you can run a `find` command on that directory
(don't forget the `-L` flag, since Bazel uses lots of symlinks) to see
where it is.

Of particular interest is `command_log`. Bazel always saves commands
and their outputs to this file (overwriting it). You may find it
convenient to define an alias for quick browsing; see
`bzl/tools/aliaslog` for a little script you can `source` to define an
alias `bl` that uses `less` to browse the command_log.

To get more verbose output from Bazel put the following in your
user.bazelrc file (see [.bazelrc](https://docs.bazel.build/versions/master/best-practices.html#bazelrc)):

```
build --subcommands=pretty_print
build --verbose_failures
build --sandbox_debug
```

Build rules may contain macros loaded from .bzl files. You can see how
they expand:

```
$ bazel query --output=build //pkg/path:target
```

If you try a Dune build and you get an unexpected error like:

```
$ dune build
Error: Too many opam files for package "reformat-snarky":
```

then you probably just need to run `$ bazel clean` before running dune.

If you get strange link errors, try buiding with `--spawn_strategy=local`.

#### <a name="config">Configuration</a>

Each dependency has its own set of build flags.  You can set them
individually; for example, to build with `debug` and `verbose` set for
libff but not for other deps, pass `--@libff//:debug --@libff//:verbose`:

`$ bazel test @libsnark//test --@libff//:debug --@libff/:verbose`

Note that this :verbose switch enables C/C++ verbosity (compiler
switch `-v`), and has no effect on Bazel's verbosity.

To list available flags, run a query for the repo, e.g. `$ bazel query @libff//:all`.

The convention followed is: "build setting" flags are declared in the
root BUILD.bazel file; the corresponding config_settings are defined
in `//bzl/config/BUILD.bazel`, and they are used to set command-line
parameters in `//bzl/config/vars.bzl`.

### <a name="known_problems">Known Problems</a>

The following do not compile:

* libsnark/gadgetlib1/gadgets/verifiers/test/test_r1cs_gg_ppzksnark_verifier_gadget.cpp
  * cmake target: gadgetlib1_r1cs_se_ppzksnark_verifier_gadget_test
  * bazel target: @libsnark//libsnark/gadgetlib1/gadgets/verifiers/test:gg_ppzksnark
  * errors:
    * error: no member named 'gamma_ABC_g1' in 'libsnark::r1cs_gg_ppzksnark_verification_key<libff::mnt6_pp>'
    * error: no member named 'gamma_ABC_g1' in 'libsnark::r1cs_gg_ppzksnark_verification_key<libff::mnt6_pp>'
    * etc.

* @libsnark//libsnark/gadgetlib2/tests:protoboard
-----------------------------------------------------------------------------
Running main() from gmock_main.cc
[==========] Running 4 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 4 tests from gadgetLib2
[ RUN      ] gadgetLib2.R1P_enforceBooleanity
[       OK ] gadgetLib2.R1P_enforceBooleanity (1 ms)
[ RUN      ] gadgetLib2.Protoboard_unpackedWordAssignmentEqualsValue_R1P
[       OK ] gadgetLib2.Protoboard_unpackedWordAssignmentEqualsValue_R1P (0 ms)
[ RUN      ] gadgetLib2.Protoboard_multipackedWordAssignmentEqualsValue_R1P
ERROR:  R1P multipacked size mismatch (In file external/libsnark/libsnark/gadgetlib2/protoboard.cpp line 162.)

  (stack trace not available on this platform)
[       OK ] gadgetLib2.Protoboard_multipackedWordAssignmentEqualsValue_R1P (0 ms)
[ RUN      ] gadgetLib2.Protoboard_dualWordAssignmentEqualsValue_R1P
[       OK ] gadgetLib2.Protoboard_dualWordAssignmentEqualsValue_R1P (1 ms)
[----------] 4 tests from gadgetLib2 (2 ms total)

[----------] Global test environment tear-down
[==========] 4 tests from 1 test suite ran. (2 ms total)
[  PASSED  ] 4 tests.

That's on MacOS; the error message is slightly different on Linux.


* @libsnark//libsnark/gadgetlib2/tests:integration

* @libfqfft//libfqfft/tests:evaluation_domain

* @libfqfft//libfqfft/tests
  * all fail with --//:with_openmp

#### <a name="cleaning">Cleaning</a>

Normally you never need to run `bazel clean`, since Bazel keeps track
of everything. In some cases, e.g. when you delete or rename a source
file, a built artifact may be left over in Bazel's work areas; Bazel
does not seem to automatically remove such stuff, which can lead to
puzzling problems. So if you're seeing unexpected stuff try `$ bazel
clean` just to be sure.  The downside to this is of course that it forces a complete rebuild, which may be time-consuming.

You may need to clean occasionally if you are working on the Bazel
code itself, or if you are using `--override-repository` to work with
different external repos. If you are testing external repos, run `$
bazel clean --expunge` to wipe out everything and force externals to
be refetched. See [Cleaning build
outputs](https://docs.bazel.build/versions/master/user-manual.html#clean)
for more information.


## Developing Dependencies

You can also work with Bazel workspaces/repositories "in parallel", so
to speak, and in fact this is routine, especially when you are
developing libraries. Bazel makes it very easy to work simultaneously
on different parts of chains of dependencies.  The procedure is simple:

* put each repository in its own directory
* use one of Bazel's reference mechanisms to express local dependencies.

Then any changes you make are immediately and automatically picked up
by the depending part.

Bazel supports three ways of referring to a repository. The first,
mentioned above, is to use a network-enabled repository rule in the
WORKSPACE file. The second is to use a `local_repository` rule in the
WORKSPACE file. This allows you to associate a workspace name with a
path in the local file system. Since the WORKSPACE file is under
version control, you only want to use this method for embedded
repositories (since they too will be under version control in the same
project.) The third way is to pass a command line
`--override_repository` parameter. This functions just like the
`local_repository` rule, except that it only accepts absolute paths.
You can avoid lots of typing by putting such parameters in a file
named `user.bazelrc` located in the root directory of your main
workspace, and putting `try-import user.bazelrc` in file `.bazelrc` in
the same directory.

Here is an example of how one might work on snarky and its
depenencies. Create a working directory (let's call it snarky-dev),
and clone snarky and all the dependencies you want to work on into that
repo.  So you have:

```
$ tree -d -L 1
snark-dev
├── ate-pairing
├── libff
├── libfqfft
├── libsnark
├── snarky
└── xbyak
```

Now put the following in `snarky-dev/snarky/.bazelrc`: `try-import
user.bazelrc`, and put the following in
`snarky-dev/snarky/user.bazelrc` (replacing /PATH/TO appropriately):

```
build --override_repository=libsnark=/PATH/TO/snarky-dev/libsnark
query --override_repository=libsnark=/PATH/TO/snarky-dev/libsnark

build --override_repository=libfqfft=/PATH/TO/snarky-dev/libfqfft
query --override_repository=libfqfft=/PATH/TO/snarky-dev/libfqfft

build --override_repository=libff=/PATH/TO/snarky-dev/libff
query --override_repository=libff=/PATH/TO/snarky-dev/libff

build --override_repository=ate_pairing=/PATH/TO/snarky-dev/ate-pairing
query --override_repository=ate_pairing=/PATH/TO/snarky-dev/ate-pairing

build --override_repository=xbyak=/PATH/TO/snarky-dev/xbyak
query --override_repository=xbyak=/PATH/TO/snarky-dev/xbyak

```

Now whenever you run `$ bazel build` or `$ bazel query` from
`snarky-dev/snarky`, any changes you have made to your local
dependency copies will be picked up. Once you have finished your edits
and pushed them to the origin server, comment out these overrides to
resume use of the repository rules in WORKSPACE.

## <a name="build_structure">Notes on the Bazel Build Structure</a>

## <a name="libs">Libraries: embedded, submodularized, bazelized</a>

`snarky` depends on the following chain of library dependencies:

* [libsnark](https://github.com/scipr-lab/libsnark) ([obazl fork](https://github.com/obazl/libsnark))
* [libfqfft](https://github.com/scipr-lab/libfqfft) ([obazl fork](https://github.com/obazl/libfqfft))
* [libff](https://github.com/scipr-lab/libff) ([obazl fork](https://github.com/obazl/libff))
* [ate-pairing](https://github.com/herumi/ate-pairing) ([obazl fork](https://github.com/obazl/ate-pairing))
* [xbyak](https://github.com/herumi/xbyak) ([obazl fork](https://github.com/obazl/xbyak))

All of these are currently directly embedded in the codebase, but they
are derived from separate github repositories. Each of the upstream
repositories has been forked to [obazl](https://github.com/obazl),
where Bazel development takes place.

The primary goal here is to add Bazel support, but a related goal is
to decouple snarky and its dependencies. This would have the obvious
effect of making it easier to keep snarky in sync with the upstream
repositories, but it also would make the build structure more clear
and easier to work with.

There are three steps to this. First is to add Bazel support to
`snarky` as-is. Second is to migrate to a modularize build structure
that uses git submodules for the dependencies. This will involve some
minor code reorganization; the result will support both Bazel and the
legacy Dune/Cmake-base build. Finally, the optional third step would
be to move to a pure Bazel build, under which all dependencies would
be supported as Bazel external repositories, and the (embedded)
submodules (and Dune/Opam files) could be removed.

#### <a name="embedded">Embedded deps</a>

The basic build structure under Bazel is the workspace. Workspaces are
determined by WORKSPACE files; the subtree under a directory
containing a WORKSPACE file is in the workspace so determined.
However, such a subtree may contain other WORKSPACE files, each of
which will determine a distinct Bazel workspace. Workspaces are
opaque; Bazel does not allow build rules from one workspace to reach
into another. Instead, it requires that build targets be exposed via
workspace-qualified _labels_. Such labels start with `@myrepo`, e.g.
`@myrepo//my/awesome/pkg:mytarget`.  This makes intuitive sense for
external repos that are downloaded (that is, specified by a
_repository rule_ in a WORKSPACE file, such as `http_archive` or
`git_repository`), but the same goes for workspaces embedded in
another workspace: the embedding workspace can only depend on
resources available under such workspace-qualified labels. In other
words, if workspace `snarky` contains workspace `xbyak` (as is the
case here), and it wants to build a target in that workspace, it
cannot just use file paths to reach into that part of the tree; it
must use one of the labels `xbyak` exposes. For example, we can run
the `xbyak` test suite from the `snarky` root directory like so:

```
snarky $ bazel test @xbyak//test
```

But the following will fail, since it uses the path of xbyak within
the snarky directory structure:

```
snarky $ bazel test snarky/src/camlsnark_c/libsnark-caml/depends/xbyak/test
```

On the other hand, `xbyak` is a workspace in its own right, so we can
also do the following:

```
snarky $ cd src/camlsnark_c/libsnark-caml/depends/xbyak/test
snarky/src/camlsnark_c/libsnark-caml/depends/xbyak/test $ bazel build //test
```

In this case, Bazel will use the nearest WORKSPACE file to determine
the effective workspace; that file is `snarky/src/camlsnark_c/libsnark-caml/depends/xbyak/WORKSPACE`

#### <a name="submodules">submodules</a>

From Bazel's perspective, there is no difference between embedded code
and a submodule.

#### <a name="externals">External repos</a>

If submodules work - that is, the code is maintained in an external
repo that is incorporated as git submodule - then transitionaing to
Bazel external repos is very simple. Just add to the WORKSPACE the
repository rules that will download the external repos. At that point
the submodules would no longer be in use and so could be removed.

For offline work you can prefetch remote external repos. See [Offline
builds](https://docs.bazel.build/versions/master/external.html#offline-builds)
for details.

### <a name="specifics">Specific Structures</a>

### src/camlsnark_c/libsnark-c/caml

The targets in this package correspond to the following:

  * src/camlsnark_c/snark_caml_bn128
  * src/camlsnark_c/snark_caml_common
  * src/camlsnark_c/snark_caml_mnt298
  * src/camlsnark_c/snark_caml_mnt753

To list the targets:

```
$ bazel query @libsnark//caml:all
@libsnark//caml:snark_caml_mnt753
@libsnark//caml:snark_caml_mnt298
@libsnark//caml:snark_caml_bn128
@libsnark//caml:snark_caml
@libsnark//caml:gen_sources
```

The first three targets listed above build .a libs corresponding to
those built by the `snark_caml_*` dune files. The `:snark_caml` target
combines them all as a single library. Note that
`src/camlsnark_c/snark_caml_common` does not need a distinct target;
what it builds is built by `@libff//libff/common`.

The `:gen_sources` target corresponds to
`src/camlsnark_c/libsnark-caml/caml/copy_over.sh`. It generates some
of the source files in `libsnark-caml/caml`. Under a pure Bazel build
system, it would allow all of those files to be removed from source
control.
