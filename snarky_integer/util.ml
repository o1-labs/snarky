open Core_kernel
open Snarky_backendless
open Snark
module B = Bigint

let bigint_to_field (type f field_var) ~m:((module M) : (f, field_var) m) =
  let open M in
  Fn.compose Bigint.to_field Bigint.of_bignum_bigint

let bigint_of_field (type f field_var) ~m:((module M) : (f, field_var) m) =
  let open M in
  Fn.compose Bigint.to_bignum_bigint Bigint.of_field
