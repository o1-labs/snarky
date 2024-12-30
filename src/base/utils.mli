module Cvar0 = Cvar
module Runner = Checked_runner

val set_eval_constraints : bool -> unit

module Make : functor
  (Backend : Backend_extended.S)
  (Types : Types.Types
             with type field = Backend.Field.t
              and type field_var = Backend.Cvar.t)
  (Checked : Checked_intf.Extended
               with module Types := Types
               with type field = Backend.Field.t
                and type run_state = Backend.Run_state.t)
  (As_prover : As_prover_intf.Basic
                 with type field := Backend.Field.t
                 with module Types := Types)
  (Typ : Snark_intf.Typ_intf
           with type field := Backend.Field.t
            and type field_var := Backend.Cvar.t
            and type 'field checked_unit := unit Types.Checked.t
            and type ('var, 'value, 'aux) typ' :=
             ('var, 'value, 'aux) Types.Typ.typ'
            and type ('var, 'value) typ := ('var, 'value) Types.Typ.typ)
  (Runner : Runner.S
              with module Types := Types
              with type field := Backend.Field.t
               and type cvar := Backend.Cvar.t
               and type constr := Backend.Constraint.t option
               and type r1cs := Backend.R1CS_constraint_system.t
               and type run_state = Backend.Run_state.t)
  -> sig
  val equal :
       Checked.field Cvar0.t
    -> Checked.field Cvar0.t
    -> Checked.field Cvar0.t Boolean.t Checked.t

  val if_ :
       Checked.field Cvar0.t Boolean.t
    -> then_:Checked.field Cvar0.t
    -> else_:Checked.field Cvar0.t
    -> Checked.field Cvar0.t Types.Checked.t

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
      -> Checked.field Cvar0.t Boolean.t Types.Checked.t

    val _and_for_square_constraint_systems :
      var -> var -> Checked.field Cvar0.t Boolean.t Types.Checked.t

    val ( && ) : var -> var -> var Checked.t

    val ( &&& ) : var -> var -> var Checked.t

    val ( || ) : var -> var -> var Types.Checked.t

    val ( ||| ) : var -> var -> var Types.Checked.t

    val any : var list -> var Types.Checked.t

    val all : var list -> var Checked.t

    val to_constant : var -> bool option

    val var_of_value : bool -> var

    val typ : (var, value) Typ.t

    val typ_unchecked : (var, value) Typ.t

    val ( lxor ) : var -> var -> var Types.Checked.t

    module Array : sig
      val num_true : var array -> Checked.field Cvar0.t

      val any : var array -> var Types.Checked.t

      val all : var array -> var Checked.t

      module Assert : sig
        val any : var array -> unit Types.Checked.t

        val all : var array -> unit Types.Checked.t
      end
    end

    val equal : var -> var -> var Types.Checked.t

    val of_field : Backend.Cvar.t -> Backend.Cvar.t Boolean.t Types.Checked.t

    module Unsafe : sig
      val of_cvar : Checked.field Cvar0.t -> var
    end

    module Assert : sig
      val ( = ) : var -> var -> unit Types.Checked.t

      val is_true : var -> unit Types.Checked.t

      val any : var list -> unit Types.Checked.t

      val all : var list -> unit Types.Checked.t

      val exactly_one : var list -> unit Types.Checked.t
    end

    module Expr : sig
      type t = Var of var | And of t list | Or of t list | Not of t

      val eval : t -> var Types.Checked.t

      val assert_ : t -> unit Types.Checked.t

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
    -> Checked.field Cvar0.t Types.Checked.t

  val square :
       ?label:string
    -> Checked.field Cvar0.t
    -> Checked.field Cvar0.t Types.Checked.t

  val div :
       ?label:string
    -> Checked.field Cvar0.t
    -> Checked.field Cvar0.t
    -> Checked.field Cvar0.t Types.Checked.t

  val inv :
       ?label:string
    -> Checked.field Cvar0.t
    -> Checked.field Cvar0.t Types.Checked.t

  val assert_non_zero : Checked.field Cvar0.t -> unit Types.Checked.t

  val equal_vars :
    Checked.field Cvar0.t -> (Checked.field * Checked.field) As_prover.t

  val equal_constraints :
       Checked.field Cvar0.t
    -> Checked.field Cvar0.t
    -> Checked.field Cvar0.t
    -> unit Types.Checked.t
end
