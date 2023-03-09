module Cvar0 = Cvar
module Runner = Checked_runner

val set_eval_constraints : bool -> unit

module Make : functor
  (Backend : Backend_extended.S)
  (Checked : Checked_intf.Extended with type field = Backend.Field.t)
  (As_prover : As_prover0.Extended with type field := Backend.Field.t)
  (Runner : Runner.S
              with module Types := Checked.Types
              with type field := Backend.Field.t
               and type cvar := Backend.Cvar.t
               and type constr := Backend.Constraint.t option
               and type r1cs := Backend.R1CS_constraint_system.t)
  -> sig
  val equal :
       Checked.field Cvar0.t
    -> Checked.field Cvar0.t
    -> Checked.field Cvar0.t Boolean.t Checked.t

  val if_ :
       Checked.field Cvar0.t Boolean.t
    -> then_:Checked.field Cvar0.t
    -> else_:Checked.field Cvar0.t
    -> (Checked.field Cvar0.t, Checked.field) Checked.Types.Checked.t

  module Typ2 : sig
    type ('var, 'value, 'aux, 'field, 'checked) typ' =
          ('var, 'value, 'aux, 'field, 'checked) Types.Typ.typ' =
      { var_to_fields : 'var -> 'field Cvar0.t array * 'aux
      ; var_of_fields : 'field Cvar0.t array * 'aux -> 'var
      ; value_to_fields : 'value -> 'field array * 'aux
      ; value_of_fields : 'field array * 'aux -> 'value
      ; size_in_field_elements : int
      ; constraint_system_auxiliary : unit -> 'aux
      ; check : 'var -> 'checked
      }

    type ('var, 'value, 'field, 'checked) typ =
          ('var, 'value, 'field, 'checked) Types.Typ.typ =
      | Typ :
          ('var, 'value, 'aux, 'field, 'checked) typ'
          -> ('var, 'value, 'field, 'checked) typ

    module T : sig
      type ('var, 'value, 'field) t =
        ( 'var
        , 'value
        , 'field
        , (unit, 'field) Snarky_backendless__.Checked_intf.Unextend(Checked).t
        )
        typ

      type ('var, 'value, 'field) typ = ('var, 'value, 'field) t

      module type S = sig
        type field

        module Var : sig
          type t

          val size_in_field_elements : int

          val to_field_elements : t -> field Cvar0.t array

          val of_field_elements : field Cvar0.t array -> t

          val check :
               t
            -> ( unit
               , field )
               Snarky_backendless__.Checked_intf.Unextend(Checked).t
        end

        module Value : sig
          type t

          val size_in_field_elements : int

          val to_field_elements : t -> field array

          val of_field_elements : field array -> t
        end
      end

      val unit : unit -> (unit, unit, 'field) t
    end

    type ('var, 'value) t = ('var, 'value, Checked.field) T.t

    val unit : (unit, unit) t
  end

  module Boolean : sig
    type var = Checked.field Cvar0.t Boolean.t

    type value = bool

    val true_ : var

    val false_ : var

    val not : var -> var

    val if_ :
         Checked.field Cvar0.t Boolean.t
      -> then_:var
      -> else_:var
      -> ( Checked.field Cvar0.t Boolean.t
         , Checked.field )
         Checked.Types.Checked.t

    val _and_for_square_constraint_systems :
         var
      -> var
      -> ( Checked.field Cvar0.t Boolean.t
         , Checked.field )
         Checked.Types.Checked.t

    val ( && ) : var -> var -> var Checked.t

    val ( &&& ) : var -> var -> var Checked.t

    val ( || ) : var -> var -> (var, Checked.field) Checked.Types.Checked.t

    val ( ||| ) : var -> var -> (var, Checked.field) Checked.Types.Checked.t

    val any : var list -> (var, Checked.field) Checked.Types.Checked.t

    val all : var list -> var Checked.t

    val to_constant : var -> bool option

    val var_of_value : bool -> var

    val typ : (var, value) Typ2.t

    val typ_unchecked : (var, value) Typ2.t

    val ( lxor ) : var -> var -> (var, Checked.field) Checked.Types.Checked.t

    module Array : sig
      val num_true : var array -> Checked.field Cvar0.t

      val any : var array -> (var, Checked.field) Checked.Types.Checked.t

      val all : var array -> var Checked.t

      module Assert : sig
        val any : var array -> (unit, Checked.field) Checked.Types.Checked.t

        val all : var array -> (unit, Checked.field) Checked.Types.Checked.t
      end
    end

    val equal : var -> var -> (var, Checked.field) Checked.Types.Checked.t

    val of_field :
         Backend.Cvar.t
      -> (Backend.Cvar.t Boolean.t, Checked.field) Checked.Types.Checked.t

    module Unsafe : sig
      val of_cvar : Checked.field Cvar0.t -> var
    end

    module Assert : sig
      val ( = ) : var -> var -> (unit, Checked.field) Checked.Types.Checked.t

      val is_true : var -> (unit, Checked.field) Checked.Types.Checked.t

      val any : var list -> (unit, Checked.field) Checked.Types.Checked.t

      val all : var list -> (unit, Checked.field) Checked.Types.Checked.t

      val exactly_one :
        var list -> (unit, Checked.field) Checked.Types.Checked.t
    end

    module Expr : sig
      type t = Var of var | And of t list | Or of t list | Not of t

      val eval : t -> (var, Checked.field) Checked.Types.Checked.t

      val assert_ : t -> (unit, Checked.field) Checked.Types.Checked.t

      val ( ! ) : var -> t

      val ( && ) : t -> t -> t

      val ( &&& ) : t -> t -> t

      val ( || ) : t -> t -> t

      val ( ||| ) : t -> t -> t

      val not : t -> t

      val any : t list -> t

      val all : t list -> t
    end
  end

  val mul :
       ?label:string
    -> Checked.field Cvar0.t
    -> Checked.field Cvar0.t
    -> (Checked.field Cvar0.t, Checked.field) Checked.Types.Checked.t

  val square :
       ?label:string
    -> Checked.field Cvar0.t
    -> (Checked.field Cvar0.t, Checked.field) Checked.Types.Checked.t

  val div :
       ?label:string
    -> Checked.field Cvar0.t
    -> Checked.field Cvar0.t
    -> (Checked.field Cvar0.t, Checked.field) Checked.Types.Checked.t

  val inv :
       ?label:string
    -> Checked.field Cvar0.t
    -> (Checked.field Cvar0.t, Checked.field) Checked.Types.Checked.t

  val assert_non_zero :
    Checked.field Cvar0.t -> (unit, Checked.field) Checked.Types.Checked.t

  val equal_vars :
    Checked.field Cvar0.t -> (Checked.field * Checked.field) As_prover.t

  val equal_constraints :
       Checked.field Cvar0.t
    -> Checked.field Cvar0.t
    -> Checked.field Cvar0.t
    -> (unit, Checked.field) Checked.Types.Checked.t
end
