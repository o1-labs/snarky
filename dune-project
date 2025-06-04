(lang dune 2.0)
(using menhir 2.0)

(name snarky)
(version 0.1)

(generate_opam_files true)

(license MIT)
(authors "O(1) Labs, LLC <opensource@o1labs.org>")
(maintainers "opensource@o1labs.org")
(source (github o1-labs/snarky))
(homepage "https://github.com/o1labs/snarky")
(bug_reports "https://github.com/o1labs/snarky/issues")

(package
 (name bitstring_lib)
 (synopsis "A helper library for bitstrings in snarky")
 (description "A helper library for bitstrings in snarky")
 (depends
  bisect_ppx
  (core (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
  ppx_deriving
  ppx_jane
  tuple_lib))

(package
 (name fold_lib)
 (synopsis "A library for fold functions")
 (description "A library for fold functions")
 (depends
  (bisect_ppx :with-test)
  (core (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
  ppx_deriving
  ppx_jane))

(package
 (name group_map)
 (synopsis "A library for elliptic curve operations")
 (description "A library for elliptic curve operations")
 (depends
  (core_kernel (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
  snarkette))

(package
 (name h_list)
 (synopsis "A heterogeneous list library")
 (description "A heterogeneous list library")
 (depends
  (base (>= v0.14.3))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
  (ppxlib (>= 0.25.0))))

(package
 (name interval_union)
 (synopsis "A library for interval unions")
 (description "A library for interval unions")
 (depends
  (alcotest :with-test)
  (bisect_ppx :with-test)
  (core_kernel (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
  ppx_deriving
  ppx_jane))

(package
 (name ppx_snarky)
 (synopsis "PPX extension for snarky")
 (description "PPX extension for snarky")
 (depends
  (core (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
  ppxlib
  ppx_sexp_conv))

(package
 (name snarkette)
 (synopsis "A portable cryptography library")
 (description "A portable cryptography library")
 (depends
  (bisect_ppx :with-test)
  bignum
  (core_kernel (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  fold_lib
  (ocaml (>= 4.07.0))
  ppx_deriving
  ppx_deriving_yojson
  ppx_jane
  tuple_lib))

(package
 (name snarky)
 (synopsis "A snarks DSL")
 (description "A snarks DSL")
 (depends
  (alcotest :with-test)
  base64
  bignum
  bitstring_lib
  (bisect_ppx (>= 2.0.0))
  (core_kernel (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  fold_lib
  h_list
  interval_union
  (ocaml (>= 4.07.0))
  (ppx_deriving (>= 5.0))
  (ppx_jane (>= v0.14))
  ppx_snarky
  tuple_lib
  (yojson (and (>= 1.7.0) (< 1.8.0))))
 (depopts snarky_cuda))

(package
 (name snarky_bench)
 (synopsis "Benchmarking library for snarky")
 (description "Benchmarking library for snarky")
 (depends
  (core (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
   ppx_bench
   ppx_jane
   snarky))

(package
 (name snarky_curve)
 (synopsis "Elliptic curve library for snarky")
 (description "Elliptic curve library for snarky")
 (depends
  (bisect_ppx :with-test)
  (core_kernel (and (>= v0.14) (< v0.15)))
   digestif
  (dune (and :build (>= 2.0)))
   group_map
  (ocaml (>= 4.07.0))
   ppx_deriving
   ppx_jane
   snarky))

(package
 (name snarky_integer)
 (synopsis "Integer library for snarky")
 (description "Integer library for snarky")
 (depends
  (bisect_ppx :with-test)
  (core_kernel (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
   ppx_deriving
   ppx_jane
   snarky))

(package
 (name snarky_signature)
 (synopsis "Signature library for snarky")
 (description "Signature library for snarky")
 (depends
  (bisect_ppx :with-test)
  (core_kernel (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
  ppx_deriving
  ppx_jane
  snarky))

(package
 (name sponge)
 (synopsis "Sponge construction for cryptographic hashing")
 (description "Sponge construction for cryptographic hashing")
 (depends
  (bisect_ppx :with-test)
  (core_kernel (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
  ppx_deriving
  ppx_jane
  snarky))

(package
 (name tuple_lib)
 (synopsis "A library for working with tuples")
 (description "A library for working with tuples")
 (depends
  (bisect_ppx :with-test)
  (core_kernel (and (>= v0.14) (< v0.15)))
  (dune (and :build (>= 2.0)))
  (ocaml (>= 4.07.0))
  ppx_deriving
  ppx_jane))
