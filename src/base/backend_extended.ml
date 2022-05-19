open Core_kernel
module Bignum_bigint = Bigint

(** Yojson-compatible JSON type. *)
type 'a json =
  [> `String of string
  | `Assoc of (string * 'a json) list
  | `List of 'a json list ]
  as
  'a

module type S = sig
  module Field : Snarky_intf.Field.Full

  module Bigint : sig
    include Snarky_intf.Bigint_intf.Extended with type field := Field.t

    val of_bignum_bigint : Bignum_bigint.t -> t

    val to_bignum_bigint : t -> Bignum_bigint.t
  end

  module Var : sig
    type t [@@deriving sexp]

    include Comparable.S with type t := t

    val index : t -> int

    val create : int -> t
  end

  module Cvar : sig
    type t = Field.t Cvar.t [@@deriving sexp]

    val length : t -> int

    module Unsafe : sig
      val of_index : int -> t
    end

    val eval :
      [`Return_values_will_be_mutated of int -> Field.t] -> t -> Field.t

    val constant : Field.t -> t

    val to_constant_and_terms : t -> Field.t option * (Field.t * Var.t) list

    val add : t -> t -> t

    val negate : t -> t

    val scale : t -> Field.t -> t

    val sub : t -> t -> t

    val linear_combination : (Field.t * t) list -> t

    val sum : t list -> t

    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val ( * ) : Field.t -> t -> t

    val var_indices : t -> int list

    val to_constant : t -> Field.t option
  end

  module R1CS_constraint_system :
    Backend_intf.Constraint_system_intf with module Field := Field

  module Constraint : sig
    type t = (Cvar.t, Field.t) Constraint.t [@@deriving sexp]

    type 'k with_constraint_args = ?label:string -> 'k

    val boolean : (Cvar.t -> t) with_constraint_args

    val equal : (Cvar.t -> Cvar.t -> t) with_constraint_args

    val r1cs : (Cvar.t -> Cvar.t -> Cvar.t -> t) with_constraint_args

    val square : (Cvar.t -> Cvar.t -> t) with_constraint_args

    val annotation : t -> string

    val eval :
         (Cvar.t, Field.t) Constraint.basic_with_annotation list
      -> (Cvar.t -> Field.t)
      -> bool
  end

  module Proving_key : sig
    type t [@@deriving bin_io]

    val is_initialized : t -> [`Yes | `No of R1CS_constraint_system.t]

    val set_constraint_system : t -> R1CS_constraint_system.t -> unit

    include Stringable.S with type t := t
  end

  module Verification_key : sig
    type t [@@deriving bin_io]

    include Stringable.S with type t := t
  end

  module Keypair : sig
    type t = {pk: Proving_key.t; vk: Verification_key.t} [@@deriving bin_io]

    val create : pk:Proving_key.t -> vk:Verification_key.t -> t

    val pk : t -> Proving_key.t

    val vk : t -> Verification_key.t

    val generate : R1CS_constraint_system.t -> t
  end
end

module Make (Backend : Backend_intf.S) :
  S
  with type Field.t = Backend.Field.t
   and type Field.Vector.t = Backend.Field.Vector.t
   and type Bigint.t = Backend.Bigint.R.t
   and type Proving_key.t = Backend.Proving_key.t
   and type Verification_key.t = Backend.Verification_key.t
   and type Var.t = Backend.Var.t
   and type R1CS_constraint_system.t = Backend.R1CS_constraint_system.t =
struct
  open Backend

  module Bigint = struct
    include Bigint.R

    let of_bignum_bigint n = of_decimal_string (Bignum_bigint.to_string n)

    let to_bignum_bigint n =
      let rec go i two_to_the_i acc =
        if i = Field.size_in_bits then acc
        else
          let acc' =
            if test_bit n i then Bignum_bigint.(acc + two_to_the_i) else acc
          in
          go (i + 1) Bignum_bigint.(two_to_the_i + two_to_the_i) acc'
      in
      go 0 Bignum_bigint.one Bignum_bigint.zero
  end

  module Verification_key = struct
    include Verification_key
    include Binable.Of_stringable_without_uuid (Verification_key)
  end

  module Proving_key = Proving_key

  module Keypair = struct
    type t = {pk: Proving_key.t; vk: Verification_key.t}
    [@@deriving fields, bin_io]

    let create = Fields.create

    let of_backend_keypair kp = {pk= Keypair.pk kp; vk= Keypair.vk kp}

    let generate = Fn.compose of_backend_keypair Backend.Keypair.create
  end

  module Var = struct
    module T = struct
      include Backend.Var

      let compare x y = Int.compare (index x) (index y)

      let t_of_sexp _ = failwith "Var.t_of_sexp"

      let sexp_of_t v =
        Sexp.(List [Atom "var"; Atom (Int.to_string (index v))])
    end

    include T
    include Comparable.Make (T)
  end

  module Field = struct
    include Field

    let size = Bigint.to_bignum_bigint Backend.field_size

    let inv x = if equal x zero then failwith "Field.inv: zero" else inv x

    (* TODO: Optimize *)
    let div x y = mul x (inv y)

    let negate x = sub zero x

    let unpack x =
      let n = Bigint.of_field x in
      List.init size_in_bits ~f:(fun i -> Bigint.test_bit n i)

    let project_reference =
      let rec go x acc = function
        | [] ->
            acc
        | b :: bs ->
            go (Field.add x x) (if b then Field.add acc x else acc) bs
      in
      fun bs -> go Field.one Field.zero bs

    let _project bs =
      (* todo: 32-bit and ARM support. basically this code needs to always match the loop in the C++ of_data implementation. *)
      assert (Sys.word_size = 64 && not Sys.big_endian) ;
      let module R = Backend.Bigint.R in
      let chunks_of n xs =
        List.groupi ~break:(fun i _ _ -> Int.equal (i mod n) 0) xs
      in
      let chunks64 = chunks_of 64 bs in
      let z = Char.of_int_exn 0 in
      let arr = Bigstring.init (8 * R.length_in_bytes) ~f:(fun _ -> z) in
      List.(
        iteri ~f:(fun i elt ->
            Bigstring.set_int64_t_le arr ~pos:(i * 8)
              Int64.(
                foldi ~init:zero
                  ~f:(fun i acc el ->
                    acc + if el then shift_left one i else zero )
                  elt) ))
        chunks64 ;
      Backend.Bigint.R.(of_data arr ~bitcount:(List.length bs) |> to_field)

    let project = project_reference

    let compare t1 t2 = Bigint.(compare (of_field t1) (of_field t2))

    let hash_fold_t s x =
      Bignum_bigint.hash_fold_t s Bigint.(to_bignum_bigint (of_field x))

    let hash = Hash.of_fold hash_fold_t

    let to_bignum_bigint = Fn.compose Bigint.to_bignum_bigint Bigint.of_field

    let of_bignum_bigint = Fn.compose Bigint.to_field Bigint.of_bignum_bigint

    let sexp_of_t = Fn.compose Bignum_bigint.sexp_of_t to_bignum_bigint

    let t_of_sexp = Fn.compose of_bignum_bigint Bignum_bigint.t_of_sexp

    let to_string = Fn.compose Bignum_bigint.to_string to_bignum_bigint

    let of_string = Fn.compose of_bignum_bigint Bignum_bigint.of_string

    let%test_unit "project correctness" =
      Quickcheck.test
        Quickcheck.Generator.(
          small_positive_int >>= fun x -> list_with_length x bool)
        ~f:(fun bs ->
          [%test_eq: string]
            (project bs |> to_string)
            (project_reference bs |> to_string) )

    let ( + ) = add

    let ( * ) = mul

    let ( - ) = sub

    let ( / ) = div
  end

  module Cvar = struct
    include Cvar.Make (Field) (Var)

    let var_indices t =
      let _, terms = to_constant_and_terms t in
      List.map ~f:(fun (_, v) -> Var.index v) terms

    let to_constant : t -> Field.t option = function
      | Constant x ->
          Some x
      | _ ->
          None
  end

  module Constraint = struct
    open Constraint
    include Constraint.T

    type 'k with_constraint_args = ?label:string -> 'k

    type t = (Cvar.t, Field.t) Constraint.t [@@deriving sexp]

    let m = (module Field : Snarky_intf.Field.S with type t = Field.t)

    let eval t get_value =
      List.for_all t ~f:(fun {basic; _} ->
          Constraint.Basic.eval m get_value basic )
  end

  module R1CS_constraint_system = R1CS_constraint_system
end
