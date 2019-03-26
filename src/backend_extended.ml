open Core_kernel

module Bignum_bigint = Bigint

module type S = sig
  module Field : sig
    type t [@@deriving bin_io, sexp, hash, compare]

    include Field_intf.Extended with type t := t
  end

  module Bigint : sig
    include Bigint_intf.Extended with type field := Field.t

    val of_bignum_bigint : Bignum_bigint.t -> t

    val to_bignum_bigint : t -> Bignum_bigint.t
  end

  val field_size : Bigint.t

  module Var : sig
    type t = Field.t Backend_types.Var.t [@@deriving sexp]

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

  val eval : (int -> Field.t) -> t -> Field.t

  val constant : Field.t -> t

  val to_constant_and_terms : t -> Field.t option * (Field.t * Var.t) list

  val add : t -> t -> t

  val scale : t -> Field.t -> t

  val sub : t -> t -> t

  val linear_combination : (Field.t * t) list -> t

  val sum : t list -> t

  module Infix : sig
    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val ( * ) : Field.t -> t -> t
  end
  end

  module Linear_combination : sig
    type t = Field.t Backend_types.Linear_combination.t

    val of_constant : Field.t option -> t

    val of_var : Cvar.t -> t

    val of_field : Field.t -> t

    val zero : t
  end

  module R1CS_constraint : sig
    type t = Field.t Backend_types.R1CS_constraint.t

    val create :
      Linear_combination.t -> Linear_combination.t -> Linear_combination.t -> t

    val set_is_square : t -> bool -> unit
  end

  module Proving_key : sig
    type t = Field.t Backend_types.Proving_key.t [@@deriving bin_io]

    include Stringable.S with type t := t

    val to_bigstring : t -> Bigstring.t

    val of_bigstring : Bigstring.t -> t
  end

  module Verification_key : sig
    type t = Field.t Backend_types.Verification_key.t [@@deriving bin_io]

    include Stringable.S with type t := t

    val to_bigstring : t -> Bigstring.t

    val of_bigstring : Bigstring.t -> t
  end

  module Proof : sig
    type t = Field.t Backend_types.Proof.t

    include Stringable.S with type t := t

    val create :
      Proving_key.t -> primary:Field.Vector.t -> auxiliary:Field.Vector.t -> t

    val verify : t -> Verification_key.t -> Field.Vector.t -> bool
  end

  module R1CS_constraint_system : sig
    type t = Field.t Backend_types.R1CS_constraint_system.t

    val create : unit -> t

    val report_statistics : t -> unit

    val add_constraint : t -> R1CS_constraint.t -> unit

    val add_constraint_with_annotation :
      t -> R1CS_constraint.t -> string -> unit

    val set_primary_input_size : t -> int -> unit

    val set_auxiliary_input_size : t -> int -> unit

    val get_primary_input_size : t -> int

    val get_auxiliary_input_size : t -> int

    val check_exn : t -> unit

    val is_satisfied :
         t
      -> primary_input:Field.Vector.t
      -> auxiliary_input:Field.Vector.t
      -> bool

    val digest : t -> Md5.t
  end

  module Keypair : sig
    type t = {pk: Proving_key.t; vk: Verification_key.t} [@@deriving bin_io]

    val create : pk:Proving_key.t -> vk:Verification_key.t -> t

    val pk : t -> Proving_key.t

    val vk : t -> Verification_key.t

    val generate : R1CS_constraint_system.t -> t
  end
end

module Make (Backend : Backend_intf.S) : S = struct
  open Backend

  type field = Field.t

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

  let field_size = field_size

  module Proof = Proof

  module Verification_key = struct
    include Verification_key
    include Binable.Of_stringable (Verification_key)
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
        | [] -> acc
        | b :: bs -> go (Field.add x x) (if b then Field.add acc x else acc) bs
      in
      fun bs -> go Field.one Field.zero bs

    let project bs =
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

    module Infix = struct
      let ( + ) = add

      let ( * ) = mul

      let ( - ) = sub

      let ( / ) = div
    end
  end

  module Cvar = struct
    include Cvar.Make (Field) (Var)

    let var_indices t =
      let _, terms = to_constant_and_terms t in
      List.map ~f:(fun (_, v) -> Var.index v) terms

    let to_constant : t -> Field.t option = function
      | Constant x -> Some x
      | _ -> None
  end

  module Linear_combination = struct
    type t = Linear_combination.t

    let of_constant = function
      | None -> Linear_combination.create ()
      | Some c -> Linear_combination.of_field c

    let of_var (cv : Cvar.t) =
      let constant, terms = Cvar.to_constant_and_terms cv in
      let t = of_constant constant in
      List.iter terms ~f:(fun (c, v) -> Linear_combination.add_term t c v) ;
      t

    let of_field = Backend.Linear_combination.of_field

    let zero = of_field Field.zero
  end

  module R1CS_constraint = R1CS_constraint

  module R1CS_constraint_system = R1CS_constraint_system
end
