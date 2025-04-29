open Core
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
  val characteristic : Bignum_bigint.t
end) : Snarky_intf.Field.S with type t = Bignum_bigint.t = struct
  type t = Bignum_bigint.t

  include Serialize

  let of_int n = Bignum_bigint.(of_int n % P.characteristic)

  let one = of_int 1

  let zero = of_int 0

  let add x y = Bignum_bigint.((x + y) % P.characteristic)

  let sub x y = Bignum_bigint.((x - y) % P.characteristic)

  let mul x y = Bignum_bigint.(x * y % P.characteristic)

  let inv x = Bignum_bigint.(inv_no_mod x P.characteristic % P.characteristic)

  let square x = mul x x

  let equal x y = Bignum_bigint.(equal ((x - y) % P.characteristic) (of_int 0))

  let is_square =
    let euler = Bignum_bigint.((P.characteristic - of_int 1) / of_int 2) in
    fun x -> Bignum_bigint.(equal ((x ** euler) % P.characteristic) (of_int 1))

  let sqrt _ =
    failwith "sqrt not implemented, not possible for arbitrary finite fields"

  let size_in_bits =
    Bignum_bigint.to_zarith_bigint P.characteristic |> Z.numbits

  let print x = to_string x |> print_endline

  let random _ = Bignum_bigint.(random P.characteristic)

  module Vector = struct
    type elt = t

    type t = elt Deque.t

    let create () = Deque.create ()

    let get = Deque.get

    let emplace_back = Deque.enqueue_back

    let length = Deque.length
  end
end

module Biginteger (P : sig
  val characteristic : Bignum_bigint.t
end) :
  Snarky_intf.Bigint_intf.Extended
    with type t = Bignum_bigint.t
     and type field := Bignum_bigint.t = struct
  type t = Bignum_bigint.t

  include Serialize

  let of_field x = x

  let test_bit x = Z.testbit (Bignum_bigint.to_zarith_bigint x)

  let to_field x = Bignum_bigint.(x % P.characteristic)

  let compare = Bignum_bigint.compare

  let of_numeral x ~base =
    Z.of_string_base base x |> Bignum_bigint.of_zarith_bigint

  let of_decimal_string x =
    Bignum_bigint.(of_numeral ~base:10 x % P.characteristic)

  let length_in_bytes =
    let max = Bignum_bigint.(P.characteristic - of_int 1) in
    Bignum_bigint.to_zarith_bigint max |> Z.numbits |> fun x -> x / 8

  let of_data _ = failwith "Biginteger.of_data not implemented"
end

module Backend (P : sig
  val characteristic : Bignum_bigint.t
end) : Snarky.Backend_intf.S = struct
  module Field = Fp (P)
  module Bigint = Biginteger (P)

  let field_size = P.characteristic

  module Cvar = Snarky.Cvar.Make (Field)

  module Constraint = struct
    type t =
      | Boolean of Field.t Snarky.Cvar.t
      | Equal of Field.t Snarky.Cvar.t * Field.t Snarky.Cvar.t
      | Square of Field.t Snarky.Cvar.t * Field.t Snarky.Cvar.t
      | R1CS of
          Field.t Snarky.Cvar.t * Field.t Snarky.Cvar.t * Field.t Snarky.Cvar.t
    [@@deriving sexp]

    let boolean x = Boolean x

    let equal x y = Equal (x, y)

    let r1cs x y z = R1CS (x, y, z)

    let square x y = Square (x, y)

    let eval (t : t) (f : Field.t Snarky.Cvar.t -> Field.t) =
      match t with
      | Boolean x ->
          f x == Field.one
      | Equal (x, y) ->
          f x == f y
      | Square (x, y) ->
          Field.mul (f x) (f x) == f y
      | R1CS (x, y, z) ->
          Field.mul (f x) (f y) == f z

    let log_constraint (t : t) (f : Field.t Snarky.Cvar.t -> Field.t) : string =
      match t with
      | Boolean x ->
          Printf.sprintf "Boolean(%s)" (Field.to_string (f x))
      | Equal (x, y) ->
          Printf.sprintf "Equal(%s, %s)"
            (f x |> Field.to_string)
            (f y |> Field.to_string)
      | Square (x, y) ->
          Printf.sprintf "Square(%s, %s)"
            (f x |> Field.to_string)
            (f y |> Field.to_string)
      | R1CS (x, y, z) ->
          Printf.sprintf "R1CS(%s, %s, %s)"
            (f x |> Field.to_string)
            (f y |> Field.to_string)
            (f z |> Field.to_string)
  end

  module R1CS_constraint_system = struct
    module Field = Field

    type constraint_ = Constraint.t

    type t =
      { public_input_size : int Core_kernel.Set_once.t
      ; auxiliary_input_size : int Core_kernel.Set_once.t
      ; constraints : constraint_ Deque.t
      }

    let create () =
      { public_input_size = Core_kernel.Set_once.create ()
      ; auxiliary_input_size = Core_kernel.Set_once.create ()
      ; constraints = Deque.create ()
      }

    let finalize _ = ()

    let add_constraint t c = Deque.enqueue_back t.constraints c

    let digest _ = failwith "R1CSSystem.digest not implemented"

    let set_primary_input_size t size =
      Core_kernel.Set_once.set_exn t.public_input_size [%here] size

    let set_auxiliary_input_size t size =
      Core_kernel.Set_once.set_exn t.auxiliary_input_size [%here] size

    let get_public_input_size t = t.public_input_size

    let get_rows_len t = Deque.length t.constraints
  end

  module Run_state = Snarky.Run_state.Make (struct
    type field = Field.t

    type constraint_ = Constraint.t
  end)
end

module Snark (P : sig
  val characteristic : Bignum_bigint.t
end) =
struct
  module Backend = Backend (P)
  include Snarky.Snark.Make (Backend)
end
