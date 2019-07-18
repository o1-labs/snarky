open Core_kernel
open Ctypes
open Bindings_base
open Backend_types

module Group_coefficients (Fq : Foreign_intf) = struct
  module type Bound = sig
    include Foreign_types

    val a : (unit -> Fq.t return) result

    val b : (unit -> Fq.t return) result
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F

    let mk_coeff name = foreign name (void @-> returning Fq.typ)

    let a = mk_coeff (with_prefix P.prefix "coeff_a")

    let b = mk_coeff (with_prefix P.prefix "coeff_b")
  end
end

module Window_table
    (G : Foreign_intf)
    (Scalar_field : Foreign_intf)
    (Scalar : Foreign_intf)
    (V : Foreign_intf) =
struct
  module type Bound = sig
    include Foreign_types

    val create : (G.t -> V.t return) result

    val scale : (V.t -> Scalar.t -> G.t return) result
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F

    let func_name = with_prefix P.prefix

    let create =
      foreign (func_name "create_window_table") (G.typ @-> returning V.typ)

    let scale =
      foreign
        (func_name "window_scalar_mul")
        (V.typ @-> Scalar.typ @-> returning G.typ)
  end
end

module Group
    (Field : Foreign_intf)
    (Bigint_r : Foreign_intf)
    (Fq : Foreign_intf) =
struct
  module type Bound = sig
    include Foreign_types

    type t

    val typ : t Ctypes.typ

    val zero : (unit -> t return) result

    val one : (unit -> t return) result

    val delete : (t -> unit return) result

    val print : (t -> unit return) result

    val random : (unit -> t return) result

    val double : (t -> t return) result

    val negate : (t -> t return) result

    val add : (t -> t -> t return) result

    val scale : (Bigint_r.t -> t -> t return) result

    val scale_field : (Field.t -> t -> t return) result

    val equal : (t -> t -> bool return) result

    val of_affine : (Fq.t -> Fq.t -> t return) result

    val is_zero : (t -> bool return) result

    val to_affine' : (t -> unit return) result

    val x : (t -> Fq.t return) result

    val y : (t -> Fq.t return) result

    val prefix : string

    module Vector :
      Vector.Bound
      with type 'a result = 'a result
       and type 'a return = 'a return
       and type elt = t
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F
    include P
    include Make_foreign (F) (P)

    let zero = foreign (func_name "zero") (void @-> returning typ)

    let one = foreign (func_name "one") (void @-> returning typ)

    let delete = foreign (func_name "delete") (typ @-> returning void)

    let print = foreign (func_name "print") (typ @-> returning void)

    let random = foreign (func_name "random") (void @-> returning typ)

    let double = foreign (func_name "double") (typ @-> returning typ)

    let negate = foreign (func_name "negate") (typ @-> returning typ)

    let add = foreign (func_name "add") (typ @-> typ @-> returning typ)

    let scale =
      foreign (func_name "scale") (Bigint_r.typ @-> typ @-> returning typ)

    let scale_field =
      foreign (func_name "scale_field") (Field.typ @-> typ @-> returning typ)

    let equal = foreign (func_name "equal") (typ @-> typ @-> returning bool)

    let of_affine =
      foreign (func_name "of_coords") (Fq.typ @-> Fq.typ @-> returning typ)

    let is_zero = foreign (func_name "is_zero") (typ @-> returning bool)

    let to_affine' =
      foreign (func_name "to_affine_coordinates") (typ @-> returning void)

    let x = foreign (func_name "x") (typ @-> returning Fq.typ)

    let y = foreign (func_name "y") (typ @-> returning Fq.typ)

    module Vector =
      Vector.Bind
        (F)
        (struct
          type nonrec t = t

          let typ = typ

          let prefix = with_prefix prefix "vector"
        end)
  end
end

module Field = struct
  module type Bound = sig
    include Foreign_types

    type t

    val typ : t typ

    val func_name : string -> string

    val size_in_bits : (unit -> int return) result

    val delete : (t -> unit return) result

    val print : (t -> unit return) result

    val random : (unit -> t return) result

    val square : (t -> t return) result

    val is_square : (t -> bool return) result

    val sqrt : (t -> t return) result

    val of_int : (Signed.Long.t -> t return) result

    val add : (t -> t -> t return) result

    val inv : (t -> t return) result

    val mul : (t -> t -> t return) result

    val sub : (t -> t -> t return) result

    module Mutable : sig
      val add : (t -> t -> unit return) result

      val sub : (t -> t -> unit return) result

      val mul : (t -> t -> unit return) result

      val copy : (t -> t -> unit return) result
    end

    val equal : (t -> t -> bool return) result

    module Vector :
      Vector.Bound
      with type 'a result = 'a result
       and type 'a return = 'a return
       and type elt = t

    val montgomery_representation : (t -> char Ctypes_static.ptr return) result
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          include Foreign_intf

          val func_name : string -> string

          val outer_prefix : string
      end) :
    Bound
    with type 'a return = 'a F.return
     and type 'a result = 'a F.result
     and type t = P.t = struct
    include F
    include P

    let size_in_bits =
      foreign (func_name "size_in_bits") (void @-> returning int)

    let delete = foreign (func_name "delete") (typ @-> returning void)

    let print = foreign (func_name "print") (typ @-> returning void)

    let random = foreign (func_name "random") (void @-> returning typ)

    let square = foreign (func_name "square") (typ @-> returning typ)

    let is_square = foreign (func_name "is_square") (typ @-> returning bool)

    let sqrt = foreign (func_name "sqrt") (typ @-> returning typ)

    let of_int = foreign (func_name "of_int") (long @-> returning typ)

    let add = foreign (func_name "add") (typ @-> typ @-> returning typ)

    let inv = foreign (func_name "inv") (typ @-> returning typ)

    let mul = foreign (func_name "mul") (typ @-> typ @-> returning typ)

    let sub = foreign (func_name "sub") (typ @-> typ @-> returning typ)

    module Mutable = struct
      let make name =
        foreign (func_name ("mut_" ^ name)) (typ @-> typ @-> returning void)

      let add = make "add"

      let sub = make "sub"

      let mul = make "mul"

      let copy = foreign (func_name "copy") (typ @-> typ @-> returning void)
    end

    let equal = foreign (func_name "equal") (typ @-> typ @-> returning bool)

    let montgomery_representation =
      foreign
        (func_name "montgomery_representation")
        (typ @-> returning (ptr char))

    module Vector =
      Vector.Bind
        (F)
        (struct
          type nonrec t = t

          let typ = typ

          let prefix = with_prefix outer_prefix "field_vector"
        end)
  end
end

(* NOTE: This isn't functored over Field0 because Common is field-agnostic.
   Probably the relationship should be inverted, and Field should get
   to/of_bigint instead.
*)
module Bigint = struct
  module Common = struct
    module type Bound = sig
      include Foreign_types

      type t

      val typ : t typ

      val func_name : string -> string

      val delete : (t -> unit return) result

      val test_bit : (t -> int -> bool return) result

      val find_wnaf :
        (Unsigned.size_t -> t -> Signed.Long.t Vector.t return) result
    end

    module Bind
        (F : Ctypes.FOREIGN) (P : sig
            val prefix : string

            val outer_prefix : string
        end) :
      Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
    struct
      include F

      include Make_foreign
                (F)
                (struct
                  let prefix =
                    with_prefix (with_prefix P.outer_prefix "bigint") P.prefix
                end)

      let test_bit =
        foreign (func_name "test_bit") (typ @-> int @-> returning bool)

      let find_wnaf =
        foreign (func_name "find_wnaf")
          (size_t @-> typ @-> returning Vector.typ)
    end
  end

  module R = struct
    module type Bound = sig
      include Common.Bound

      type field

      val div : (t -> t -> t return) result

      val of_numeral : (string -> int -> int -> t return) result

      val of_decimal_string : (string -> t return) result

      val compare : (t -> t -> int return) result

      val of_field : (field -> t return) result

      val num_limbs : (unit -> int return) result

      val bytes_per_limb : (unit -> int return) result

      val to_data : (t -> char Ctypes_static.ptr return) result

      val of_data : (char Ctypes_static.ptr -> t return) result

      val to_field : (t -> field return) result
    end

    module Bind
        (F : Ctypes.FOREIGN) (P : sig
            val prefix : string
        end)
        (Field0 : Foreign_intf) :
      Bound
      with type 'a return = 'a F.return
       and type 'a result = 'a F.result
       and type field := Field0.t = struct
      open F

      include Common.Bind
                (F)
                (struct
                  let outer_prefix = P.prefix

                  let prefix = "r"
                end)

      let div = foreign (func_name "div") (typ @-> typ @-> returning typ)

      let of_numeral =
        foreign (func_name "of_numeral")
          (string @-> int @-> int @-> returning typ)

      let of_decimal_string =
        foreign (func_name "of_decimal_string") (string @-> returning typ)

      let compare =
        foreign (func_name "compare") (typ @-> typ @-> returning int)

      let of_field =
        foreign (func_name "of_field") (Field0.typ @-> returning typ)

      let num_limbs = foreign (func_name "num_limbs") (void @-> returning int)

      let bytes_per_limb =
        foreign (func_name "bytes_per_limb") (void @-> returning int)

      let to_data = foreign (func_name "to_data") (typ @-> returning (ptr char))

      let of_data = foreign (func_name "of_data") (ptr char @-> returning typ)

      let to_field =
        foreign (func_name "to_field") (typ @-> returning Field0.typ)
    end
  end

  module type Bound = sig
    include Foreign_types

    type field

    module R :
      R.Bound
      with type 'a return = 'a return
       and type 'a result = 'a result
       and type field := field

    module Q :
      Common.Bound
      with type 'a return = 'a return
       and type 'a result = 'a result
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end)
      (Field0 : Foreign_intf) :
    Bound
    with type 'a return = 'a F.return
     and type 'a result = 'a F.result
     and type field := Field0.t = struct
    include F
    module R = R.Bind (F) (P) (Field0)

    module Q =
      Common.Bind
        (F)
        (struct
          let prefix = "q"

          let outer_prefix = P.prefix
        end)
  end
end

module Var (Field0 : Foreign_intf) = struct
  module type Bound = sig
    include Foreign_types

    type t = Field0.t Backend_types.Var.t

    val typ : t Ctypes.typ

    val delete : (t -> unit return) result

    val index : (t -> Unsigned.size_t return) result

    val create : (int -> t return) result
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F

    include Var.Make
              (F)
              (struct
                let prefix = with_prefix P.prefix "var"

                type field = Field0.t
              end)

    let create = foreign (func_name "create") (int @-> returning typ)

    let index = foreign (func_name "index") (typ @-> returning size_t)
  end
end

module Linear_combination (Field : Foreign_intf) (Var : Foreign_intf) = struct
  module Term = struct
    module type Bound = sig
      include Foreign_types

      type t = Field.t Linear_combination.Term.t

      val typ : t typ

      val delete : (t -> unit return) result

      val create : (Field.t -> Var.t -> t return) result

      val coeff : (t -> Field.t return) result

      val index : (t -> int return) result

      module Vector :
        Vector.Bound
        with type 'a result = 'a result
         and type 'a return = 'a return
         and type elt = t
    end

    module Bind
        (F : Ctypes.FOREIGN) (P : sig
            val prefix : string
        end) :
      Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
    struct
      include F

      let prefix = with_prefix P.prefix "term"

      include Linear_combination.Term.Make
                (F)
                (struct
                  let prefix = prefix

                  type field = Field.t
                end)

      let create =
        foreign (func_name "create") (Field.typ @-> Var.typ @-> returning typ)

      let coeff = foreign (func_name "coeff") (typ @-> returning Field.typ)

      let index = foreign (func_name "index") (typ @-> returning int)

      module Vector =
        Vector.Bind
          (F)
          (struct
            type nonrec t = t

            let typ = typ

            let prefix = with_prefix prefix "vector"
          end)
    end
  end

  module type Bound = sig
    include Foreign_types

    type t = Field.t Backend_types.Linear_combination.t

    val typ : t typ

    val delete : (t -> unit return) result

    module Term :
      Term.Bound with type 'a result = 'a result and type 'a return = 'a return

    module Vector :
      Vector.Bound
      with type 'a result = 'a result
       and type 'a return = 'a return
       and type elt = t

    val print : (t -> unit return) result

    val create : (unit -> t return) result

    val of_var : (Var.t -> t return) result

    val of_int : (int -> t return) result

    val add_term : (t -> Field.t -> Var.t -> unit return) result

    val terms : (t -> Term.Vector.t return) result

    val of_field : (Field.t -> t return) result
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F

    let prefix = with_prefix P.prefix "linear_combination"

    include Linear_combination.Make
              (F)
              (struct
                let prefix = prefix

                type field = Field.t
              end)

    module Term =
      Term.Bind
        (F)
        (struct
          let prefix = prefix
        end)

    module Vector =
      Vector.Bind
        (F)
        (struct
          type nonrec t = t

          let typ = typ

          let prefix = with_prefix prefix "vector"
        end)

    let print = foreign (func_name "print") (typ @-> returning void)

    (*
    let substitute =
      foreign (func_name "substitute")
        (typ @-> Var.typ @-> Term.Vector.typ @-> returning void)
    ;; *)

    let create = foreign (func_name "create") (void @-> returning typ)

    let of_var = foreign (func_name "of_var") (Var.typ @-> returning typ)

    let of_int = foreign (func_name "of_int") (int @-> returning typ)

    let add_term =
      foreign (func_name "add_term")
        (typ @-> Field.typ @-> Var.typ @-> returning void)

    let terms = foreign (func_name "terms") (typ @-> returning Term.Vector.typ)

    let of_field = foreign (func_name "of_field") (Field.typ @-> returning typ)
  end
end

module R1CS_constraint
    (Field : Foreign_intf)
    (Linear_combination : Foreign_intf) =
struct
  module type Bound = sig
    include Foreign_types

    type t = Field.t Backend_types.R1CS_constraint.t

    val typ : t Ctypes.typ

    val delete : (t -> unit return) result

    val create :
      (   Linear_combination.t
       -> Linear_combination.t
       -> Linear_combination.t
       -> t return)
      result

    val set_is_square : (t -> bool -> unit return) result

    val a : (t -> Linear_combination.t return) result

    val b : (t -> Linear_combination.t return) result

    val c : (t -> Linear_combination.t return) result
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F

    include R1CS_constraint.Make
              (F)
              (struct
                let prefix = with_prefix P.prefix "r1cs_constraint"

                type field = Field.t
              end)

    let create =
      foreign (func_name "create")
        ( Linear_combination.typ @-> Linear_combination.typ
        @-> Linear_combination.typ @-> returning typ )

    let set_is_square =
      foreign (func_name "set_is_square") (typ @-> bool @-> returning void)

    let a = foreign (func_name "a") (typ @-> returning Linear_combination.typ)

    let b = foreign (func_name "b") (typ @-> returning Linear_combination.typ)

    let c = foreign (func_name "c") (typ @-> returning Linear_combination.typ)
  end
end

module R1CS_constraint_system
    (Field : Foreign_intf)
    (Field_vector : Foreign_intf)
    (R1CS_constraint : Foreign_intf) =
struct
  module type Bound = sig
    include Foreign_types

    type t = Field.t Backend_types.R1CS_constraint_system.t

    val typ : t typ

    val func_name : string -> string

    val delete : (t -> unit return) result

    val report_statistics : (t -> unit return) result

    val swap_AB_if_beneficial : (t -> unit return) result

    val check : (t -> bool return) result

    val create : (unit -> t return) result

    val clear : (t -> unit return) result

    val add_constraint : (t -> R1CS_constraint.t -> unit return) result

    val add_constraint_with_annotation :
      (t -> R1CS_constraint.t -> string -> unit return) result

    val set_primary_input_size : (t -> int -> unit return) result

    val set_auxiliary_input_size : (t -> int -> unit return) result

    val get_primary_input_size : (t -> int return) result

    val get_auxiliary_input_size : (t -> int return) result

    val is_satisfied :
      (t -> Field_vector.t -> Field_vector.t -> bool return) result

    val digest : (t -> Cpp_string.t return) result

    val num_constraints : (t -> int return) result
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F

    include R1CS_constraint_system.Make
              (F)
              (struct
                let prefix = with_prefix P.prefix "r1cs_constraint_system"

                type field = Field.t
              end)

    let report_statistics =
      foreign (func_name "report_statistics") (typ @-> returning void)

    let swap_AB_if_beneficial =
      foreign (func_name "swap_AB_if_beneficial") (typ @-> returning void)

    let check = foreign (func_name "check") (typ @-> returning bool)

    let create = foreign (func_name "create") (void @-> returning typ)

    let clear = foreign (func_name "clear") (typ @-> returning void)

    let num_constraints =
      foreign (func_name "num_constraints") (typ @-> returning int)

    let add_constraint =
      foreign
        (func_name "add_constraint")
        (typ @-> R1CS_constraint.typ @-> returning void)

    let add_constraint_with_annotation =
      foreign
        (func_name "add_constraint_with_annotation")
        (typ @-> R1CS_constraint.typ @-> string @-> returning void)

    let set_primary_input_size =
      foreign
        (func_name "set_primary_input_size")
        (typ @-> int @-> returning void)

    let set_auxiliary_input_size =
      foreign
        (func_name "set_auxiliary_input_size")
        (typ @-> int @-> returning void)

    let get_primary_input_size =
      foreign (func_name "get_primary_input_size") (typ @-> returning int)

    let get_auxiliary_input_size =
      foreign (func_name "get_auxiliary_input_size") (typ @-> returning int)

    let is_satisfied =
      foreign (func_name "is_satisfied")
        ( typ @-> Field_vector.typ @-> Field_vector.typ
        @-> returning Ctypes.bool )

    let digest = foreign (func_name "digest") (typ @-> returning Cpp_string.typ)
  end
end

module Common = struct
  module type Bound = sig
    include Foreign_types

    val prefix : string

    val init : (unit -> unit return) result

    module Field0 : sig
      type t

      val typ : t typ

      val delete : (t -> unit return) result

      val func_name : string -> string
    end

    module Bigint :
      Bigint.Bound
      with type 'a return = 'a return
       and type 'a result = 'a result
       and type field := Field0.t

    module Field :
      Field.Bound
      with type 'a return = 'a return
       and type 'a result = 'a result
       and type t = Field0.t

    module Var : sig
      module T : module type of Var (Field0)

      include
        T.Bound with type 'a return = 'a return and type 'a result = 'a result
    end

    module Linear_combination : sig
      module T : module type of Linear_combination (Field) (Var)

      include
        T.Bound with type 'a return = 'a return and type 'a result = 'a result
    end

    module R1CS_constraint : sig
      module T : module type of R1CS_constraint (Field) (Linear_combination)

      include
        T.Bound with type 'a return = 'a return and type 'a result = 'a result
    end

    module R1CS_constraint_system : sig
      module T :
          module type of
            R1CS_constraint_system (Field0) (Field.Vector) (R1CS_constraint)

      include
        T.Bound with type 'a return = 'a return and type 'a result = 'a result
    end

    val field_size : (unit -> Bigint.R.t return) result

    val domain_size : (int -> int return) result
  end

  module Bind (P : sig
    val prefix : string
  end)
  (F : Ctypes.FOREIGN) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F

    let prefix = P.prefix

    let init =
      foreign
        (with_prefix P.prefix "init_public_params")
        (void @-> returning void)

    let domain_size =
      foreign (with_prefix prefix "domain_size") (int @-> returning int)

    module Field0 =
      Make_foreign
        (F)
        (struct
          let prefix = with_prefix prefix "field"
        end)

    module Bigint = Bigint.Bind (F) (P) (Field0)

    module Field =
      Field.Bind
        (F)
        (struct
          include Field0

          let outer_prefix = P.prefix
        end)

    module Var = struct
      module T = Var (Field0)
      include T.Bind (F) (P)
    end

    module Linear_combination = struct
      module T = Linear_combination (Field) (Var)
      include T.Bind (F) (P)
    end

    module R1CS_constraint = struct
      module T = R1CS_constraint (Field) (Linear_combination)
      include T.Bind (F) (P)
    end

    module R1CS_constraint_system = struct
      module T =
        R1CS_constraint_system (Field) (Field.Vector) (R1CS_constraint)
      include T.Bind (F) (P)
    end

    let field_size =
      foreign
        (with_prefix P.prefix "field_size")
        (void @-> returning Bigint.R.typ)
  end
end
