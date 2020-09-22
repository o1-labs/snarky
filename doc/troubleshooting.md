# troubleshooting

## incompatible assumptions

Make sure both ml and mli files use the same ppx!

Error: Files bazel-out/darwin-fastbuild/bin/src/base/Snarky_backendless__Pedersen.cmx
       and bazel-out/darwin-fastbuild/bin/interval_union/Interval_union.cmx
       make inconsistent assumptions over interface Interval_union

This was caused by a missing ppx_exe attribute in the ocaml_inteface
rule for interval_union.mli.

### linker errors and warnings

"ld warning: directory not found -L/opt/local/lib"

Evidently this is caused by the zarith package. It can be ignored, or
you can silence it by creating the directory.

### the incredibly annoying "expected in flat namespace" error

This is a MacOS annoyance.  Example:

$ ./bazel-bin/examples/election/election
dyld: Symbol not found: __ZN5libff10mnt4753_G117wnaf_window_tableE
  Referenced from: bazel-out/darwin-fastbuild/bin/external/libsnark/caml/libsnark_caml.so
  Expected in: flat namespace
 in bazel-out/darwin-fastbuild/bin/external/libsnark/caml/libsnark_caml.so

Make sure you've compiled everything with the same language (C++14)
and the same C++ standard lib (libc++ on MacOS, libstdc++ on Linux).
Including external repos. Check the name mangling (is 'extern "C"'
involved?).  For your cc_deps, make sure the default is set to dynamic linking.


### dlsym linking

Uncaught exception:

  Dl.DL_error("dlsym(RTLD_DEFAULT, camlsnark_string_to_char_pointer): symbol not found")

Raised at file "src/ctypes-foreign-base/dl.ml", line 42, characters 20-44
Called from file "src/ctypes-foreign-base/ctypes_foreign_basis.ml", line 47, characters 19-47
Re-raised at file "src/ctypes-foreign-base/ctypes_foreign_basis.ml", line 49, characters 50-59
Called from file "src/camlsnark_c/cpp_string/snarky_cpp_string.ml", line 9, characters 2-70

### Illegal char

File "bazel-out/darwin-fastbuild/bin/src/camlsnark_c/Snarky_libsnark_bindings__Libsnark_ffi_bindings.ml", line 1498, characters 0-1:
Error: Illegal character (\000)

This is a generated file. No idea how it ends up with binary content.
Fix is to run the build again. You build it separately: `$ bazel build
src/camlsnark_c:libsnark_ffi_bindings_ml`. But that shows what it's
like before PPX processing.
