module Bignum_bigint = Bigint

let rec extended_euclidean a b =
  let open Bignum_bigint in
  if equal b zero then (a, one, zero)
  else
    match extended_euclidean b (a % b) with
    | d, x, y ->
        (d, y, x - Bignum_bigint.(a / b * y))

let inv_no_mod x p =
  let _, a, _b = extended_euclidean x p in
  a

module Serialize = struct
  let t_of_sexp = Bignum_bigint.t_of_sexp

  let sexp_of_t = Bignum_bigint.sexp_of_t

  let bin_size_t = Bignum_bigint.bin_size_t

  let bin_write_t = Bignum_bigint.bin_write_t

  let bin_read_t = Bignum_bigint.bin_read_t

  let __bin_read_t__ = Bignum_bigint.__bin_read_t__

  let bin_shape_t = Bignum_bigint.bin_shape_t

  let bin_writer_t = Bignum_bigint.bin_writer_t

  let bin_reader_t = Bignum_bigint.bin_reader_t

  let bin_t = Bignum_bigint.bin_t

  let of_string = Bignum_bigint.of_string

  let to_string = Bignum_bigint.to_string
end

module Fp (P : sig
  val order : Bignum_bigint.t
end) : Snarky_intf.Field.S = struct
  type t = Bignum_bigint.t

  include Serialize

  let of_int = Bignum_bigint.of_int

  let one = of_int 1

  let zero = of_int 0

  let add x y = Bignum_bigint.(x + (y % P.order))

  let sub x y = Bignum_bigint.(x - (y % P.order))

  let mul x y = Bignum_bigint.(x * y % P.order)

  let inv x = Bignum_bigint.(inv_no_mod x P.order % P.order)

  let square x = mul x x

  let equal x y = Bignum_bigint.(equal (x - (y % P.order)) (of_int 0))

  let is_square =
    let euler = Bignum_bigint.((P.order - of_int 1) / of_int 2) in
    fun x -> Bignum_bigint.(equal (x ** euler) one)

  let sqrt =
    failwith "sqrt not implemented, not possible for arbitrary finite fields"

  let size_in_bits = Bignum_bigint.to_zarith_bigint P.order |> Z.log2

  let print x = to_string x |> print_endline

  let random _ = Bignum_bigint.(random P.order)

  module Vector = struct
    open Core

    type elt = t

    type t = elt Deque.t

    let create () = Deque.create ()

    let get = Deque.get

    let emplace_back = Deque.enqueue_back

    let length = Deque.length
  end
end

module Bigint : Snarky_intf.Bigint_intf.S = struct
  include Bignum_bigint

  let of_int = Bignum_bigint.of_int

  let to_int = Bignum_bigint.to_int

  let of_string = Bignum_bigint.of_string

  let to_string = Bignum_bigint.to_string
end