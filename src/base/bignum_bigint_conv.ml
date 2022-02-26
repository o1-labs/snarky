module Bignum_bigint = Bigint

module Make (Bigint : Snarky_intf.Bigint_intf.Convertible) :
  Snarky_intf.Bigint_intf.Bignum_bigint_conv with type t := Bigint.t = struct
  open Bigint

  let of_bignum_bigint n = of_decimal_string (Bignum_bigint.to_string n)

  let to_bignum_bigint n =
    let size_in_bits = length_in_bytes * 8 in
    let rec go i two_to_the_i acc =
      if i = size_in_bits then acc
      else
        let acc' =
          if test_bit n i then Bignum_bigint.(acc + two_to_the_i) else acc
        in
        go (i + 1) Bignum_bigint.(two_to_the_i + two_to_the_i) acc'
    in
    go 0 Bignum_bigint.one Bignum_bigint.zero
end
