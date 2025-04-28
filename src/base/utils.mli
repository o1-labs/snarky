module Runner = Checked_runner

val set_eval_constraints : bool -> unit

module Make : functor
  (Backend : Backend_intf.S)
  (Types : Types.Types
             with type field = Backend.Field.t
              and type field_var = Backend.Cvar.t)
  (Checked : Checked_intf.Extended
               with module Types := Types
               with type run_state = Backend.Run_state.t
               with type constraint_ = Backend.Constraint.t)
  (As_prover : As_prover_intf.S with module Types := Types)
  (Typ : Snark_intf.Typ_intf
           with type field := Backend.Field.t
            and type field_var := Backend.Cvar.t
            and type 'field checked_unit := unit Types.Checked.t
            and type ('var, 'value, 'aux) typ' :=
             ('var, 'value, 'aux) Types.Typ.typ'
            and type ('var, 'value) typ := ('var, 'value) Types.Typ.typ)
  (Runner : Runner.S
              with module Types := Types
              with type constr := Backend.Constraint.t option
               and type r1cs := Backend.R1CS_constraint_system.t
               and type run_state = Backend.Run_state.t)
  -> sig
  val equal :
    Types.field_var -> Types.field_var -> Types.field_var Boolean.t Checked.t

  val if_ :
       Types.field_var Boolean.t
    -> then_:Types.field_var
    -> else_:Types.field_var
    -> Types.field_var Types.Checked.t

  module Boolean : sig
    type var = Types.field_var Boolean.t

    type value = bool

    val true_ : var

    val false_ : var

    val not : var -> var

    val if_ :
         Types.field_var Boolean.t
      -> then_:var
      -> else_:var
      -> Types.field_var Boolean.t Types.Checked.t

    val _and_for_square_constraint_systems :
      var -> var -> Types.field_var Boolean.t Types.Checked.t

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
      val num_true : var array -> Types.field_var

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
      val of_cvar : Types.field_var -> var
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
    -> Types.field_var
    -> Types.field_var
    -> Types.field_var Types.Checked.t

  val square :
    ?label:string -> Types.field_var -> Types.field_var Types.Checked.t

  val div :
       ?label:string
    -> Types.field_var
    -> Types.field_var
    -> Types.field_var Types.Checked.t

  val inv : ?label:string -> Types.field_var -> Types.field_var Types.Checked.t

  val assert_non_zero : Types.field_var -> unit Types.Checked.t

  val equal_vars : Types.field_var -> (Types.field * Types.field) As_prover.t

  val equal_constraints :
       Types.field_var
    -> Types.field_var
    -> Types.field_var
    -> unit Types.Checked.t
end
