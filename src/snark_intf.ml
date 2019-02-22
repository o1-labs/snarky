module Bignum_bigint = Bigint
open Core_kernel
module Constraint0 = Constraint
module Boolean0 = Boolean

(** The base interface to Snarky. *)
module type Basic = sig
  (** The {!module:Backend_intf.S.Proving_key} module from the backend. *)
  module Proving_key : sig
    type t [@@deriving bin_io]

    val to_string : t -> string

    val of_string : string -> t

    val to_bigstring : t -> Bigstring.t

    val of_bigstring : Bigstring.t -> t
  end

  (** The {!module:Backend_intf.S.Verification_key} module from the backend. *)
  module Verification_key : sig
    type t [@@deriving bin_io]

    val to_string : t -> string

    val of_string : string -> t

    val to_bigstring : t -> Bigstring.t

    val of_bigstring : Bigstring.t -> t
  end

  (** The rank-1 constraint system used by this instance. See
      {!module:Backend_intf.S.R1CS_constraint_system}. *)
  module R1CS_constraint_system : sig
    type t

    val digest : t -> Md5.t
  end

  (** Managing and generating pairs of keys {!type:Proving_key.t} and
      {!type:Verification_key.t}. *)
  module Keypair : sig
    type t [@@deriving bin_io]

    val create : pk:Proving_key.t -> vk:Verification_key.t -> t

    val pk : t -> Proving_key.t

    val vk : t -> Verification_key.t

    val generate : R1CS_constraint_system.t -> t
  end

  (** Variables in the R1CS. *)
  module Var : sig
    include Comparable.S

    val create : int -> t
  end

  (** The finite field over which the R1CS operates. *)
  type field

  module Bigint : sig
    include Bigint_intf.Extended with type field := field

    val of_bignum_bigint : Bignum_bigint.t -> t

    val to_bignum_bigint : t -> Bignum_bigint.t
  end

  (** Rank-1 constraints over {!type:Var.t}s. *)
  module rec Constraint : sig
    type t = Field.Var.t Constraint0.t

    type 'k with_constraint_args = ?label:string -> 'k

    val boolean : (Field.Var.t -> t) with_constraint_args

    val equal : (Field.Var.t -> Field.Var.t -> t) with_constraint_args

    val r1cs :
      (Field.Var.t -> Field.Var.t -> Field.Var.t -> t) with_constraint_args

    val square : (Field.Var.t -> Field.Var.t -> t) with_constraint_args
  end
  
  (** The data specification for checked computations. *)
  and Data_spec : sig
    (** A list of {!type:Typ.t} values, describing the inputs to a checked computation.
        The type [('r_var, 'r_value, 'k_var, 'k_value) t] represents
        - ['k_value] is the OCaml type of the computation
        - ['r_value] is the OCaml type of the result
        - ['k_var] is the type of the computation within the R1CS
        - ['k_value] is the type of the result within the R1CS. *)
    type ('r_var, 'r_value, 'k_var, 'k_value) t =
      | ( :: ) :
          ('var, 'value) Typ.t * ('r_var, 'r_value, 'k_var, 'k_value) t
          -> ('r_var, 'r_value, 'var -> 'k_var, 'value -> 'k_value) t
      | [] : ('r_var, 'r_value, 'r_var, 'r_value) t

    val size : (_, _, _, _) t -> int
  end
  
  (** Mappings from OCaml types to R1CS variables and constraints. *)
  and Typ : sig
    module Store : sig
      include
        Monad.S with type 'a t = ('a, Field.t, Field.Var.t) Typ_monads.Store.t

      val store : field -> Field.Var.t t
    end

    module Alloc : sig
      include Monad.S with type 'a t = ('a, Field.Var.t) Typ_monads.Alloc.t

      val alloc : Field.Var.t t
    end

    module Read : sig
      include
        Monad.S with type 'a t = ('a, Field.t, Field.Var.t) Typ_monads.Read.t

      val read : Field.Var.t -> field t
    end

    type ('var, 'value) t = ('var, 'value, Field.t) Types.Typ.t

    (** Accessors for {!type:Types.Typ.t} fields: *)

    val store : ('var, 'value) t -> 'value -> 'var Store.t

    val read : ('var, 'value) t -> 'var -> 'value Read.t

    val alloc : ('var, 'value) t -> 'var Alloc.t

    val check : ('var, 'value) t -> 'var -> (unit, _) Checked.t

    (** Basic instances: *)

    val unit : (unit, unit) t

    val field : (Field.Var.t, field) t

    (** Common constructors: *)

    val tuple2 :
         ('var1, 'value1) t
      -> ('var2, 'value2) t
      -> ('var1 * 'var2, 'value1 * 'value2) t

    val ( * ) :
         ('var1, 'value1) t
      -> ('var2, 'value2) t
      -> ('var1 * 'var2, 'value1 * 'value2) t
    (** synonym for tuple2 *)

    val tuple3 :
         ('var1, 'value1) t
      -> ('var2, 'value2) t
      -> ('var3, 'value3) t
      -> ('var1 * 'var2 * 'var3, 'value1 * 'value2 * 'value3) t

    val list : length:int -> ('var, 'value) t -> ('var list, 'value list) t

    val array : length:int -> ('var, 'value) t -> ('var array, 'value array) t

    val hlist :
         (unit, unit, 'k_var, 'k_value) Data_spec.t
      -> ((unit, 'k_var) H_list.t, (unit, 'k_value) H_list.t) t
    (** Unpack a {!type:Data_spec.t} list to a {!type:t}. The return value relates
        a polymorphic list of OCaml types to a polymorphic list of R1CS types. *)

    (** Convert relationships over
        {{:https://en.wikipedia.org/wiki/Isomorphism}isomorphic} types: *)

    val transport :
         ('var, 'value1) t
      -> there:('value2 -> 'value1)
      -> back:('value1 -> 'value2)
      -> ('var, 'value2) t

    val transport_var :
         ('var1, 'value) t
      -> there:('var2 -> 'var1)
      -> back:('var1 -> 'var2)
      -> ('var2, 'value) t

    val of_hlistable :
         (unit, unit, 'k_var, 'k_value) Data_spec.t
      -> var_to_hlist:('var -> (unit, 'k_var) H_list.t)
      -> var_of_hlist:((unit, 'k_var) H_list.t -> 'var)
      -> value_to_hlist:('value -> (unit, 'k_value) H_list.t)
      -> value_of_hlist:((unit, 'k_value) H_list.t -> 'value)
      -> ('var, 'value) t

    module Of_traversable (T : Traversable.S) : sig
      val typ :
        template:unit T.t -> ('var, 'value) t -> ('var T.t, 'value T.t) t
    end
  end
  
  (** Representation of booleans within a field.

      This representation ties the value of [true] to {!val:Field.one} and
      [false] to {!val:Field.zero}, adding a check in {!val:Boolean.typ} to
      ensure that these are the only vales. *)
  and Boolean : sig
    type var = Field.Var.t Boolean0.t

    type value = bool

    val true_ : var

    val false_ : var

    val if_ : var -> then_:var -> else_:var -> (var, _) Checked.t

    val not : var -> var

    val ( && ) : var -> var -> (var, _) Checked.t

    val ( || ) : var -> var -> (var, _) Checked.t

    val ( lxor ) : var -> var -> (var, _) Checked.t

    val any : var list -> (var, _) Checked.t

    val all : var list -> (var, _) Checked.t

    val of_field : Field.Var.t -> (var, _) Checked.t
    (** Convert a value in a field to a boolean, adding checks to the R1CS that
       it is a valid boolean value. *)

    val var_of_value : value -> var

    val typ : (var, value) Typ.t
    (** The relationship between {!val:var} and {!val:value}, with a check that
        the value is valid (ie. {!val:Field.zero} or {!val:Field.one}). *)

    val typ_unchecked : (var, value) Typ.t
    (** {!val:typ} without a validity check for the underlying field value. *)

    val equal : var -> var -> (var, _) Checked.t

    module Expr : sig
      (** Expression trees. *)
      type t

      val ( ! ) : var -> t

      val ( && ) : t -> t -> t

      val ( || ) : t -> t -> t

      val any : t list -> t

      val all : t list -> t

      val not : t -> t

      val eval : t -> (var, _) Checked.t
      (** Evaluate the expression tree. *)

      val assert_ : t -> (unit, _) Checked.t
    end

    module Unsafe : sig
      val of_cvar : Field.Var.t -> var
    end

    module Assert : sig
      val ( = ) : Boolean.var -> Boolean.var -> (unit, _) Checked.t

      val is_true : Boolean.var -> (unit, _) Checked.t

      val any : var list -> (unit, _) Checked.t

      val all : var list -> (unit, _) Checked.t

      val exactly_one : var list -> (unit, _) Checked.t
    end
  end
  
  (** Checked computations.

      These are the values used to generate an R1CS for a computation. *)
  and Checked : sig
    (** [('ret, 'state) t] represents a computation ['state -> 'ret] that can
        be compiled into an R1CS.

        We form a
        {{:https://en.wikipedia.org/wiki/Monad_(functional_programming)}monad}
        over this type, which allows us to work inside the checked function to
        do further checked computations. For example (using
        {{:https://github.com/janestreet/ppx_let}monad let-syntax}):
{[
let multiply3 (x : Field.Var.t) (y : Field.Var.t) (z : Field.Var.t)
  : (Field.Var.t, _) Checked.t =
  open Checked.Let_syntax in
  let%bind x_times_y = Field.Checked.mul x y in
  Field.Checked.mul x_times_y z
]}
    *)
    include
      Monad.S2
      with type ('a, 's) t = ('a, 's, Field.t) Types.Checked.t

    module List :
      Monad_sequence.S
      with type ('a, 's) monad := ('a, 's) t
       and type 'a t = 'a list
       and type boolean := Boolean.var

    (** [Choose_preimage] is the request issued by
        {!val:Field.Checked.choose_preimage_var} before falling back to its
        default implementation. You can respond to this request to override the
        default behaviour.
        
        See {!module:Request} for more information on requests. *)
    type _ Request.t += Choose_preimage : field * int -> bool list Request.t
  end
  
  and Field : sig
    (** The finite field over which the R1CS operates. *)
    type t = field [@@deriving bin_io, sexp, hash, compare, eq]

    val gen : t Core_kernel.Quickcheck.Generator.t
    (** A generator for Quickcheck tests. *)

    include Field_intf.Extended with type t := t

    include Stringable.S with type t := t

    val size : Bignum_bigint.t

    val unpack : t -> bool list
    (** Convert a field element into its constituent bits. *)

    val project : bool list -> t
    (** Convert a list of bits into a field element. *)

    val project_reference : bool list -> t
    (** [project], but slow. Exposed for benchmarks. *)

    type var' = Var.t

    module Var : sig
      type t = field Cvar.t

      val length : t -> int
      (** For debug purposes *)

      val var_indices : t -> int list

      val to_constant_and_terms : t -> field option * (field * Var.t) list
      (** Convert a {!type:t} value to its constituent constant and a list of
          scaled R1CS variables. *)

      val constant : field -> t

      val to_constant : t -> field option

      val linear_combination : (field * t) list -> t

      val sum : t list -> t

      val add : t -> t -> t

      val sub : t -> t -> t

      val scale : t -> field -> t

      val project : Boolean.var list -> t

      val pack : Boolean.var list -> t
    end

    module Checked : sig
      val mul : Var.t -> Var.t -> (Var.t, _) Checked.t

      val square : Var.t -> (Var.t, _) Checked.t

      val div : Var.t -> Var.t -> (Var.t, _) Checked.t

      val inv : Var.t -> (Var.t, _) Checked.t

      val equal : Var.t -> Var.t -> (Boolean.var, 's) Checked.t

      val unpack : Var.t -> length:int -> (Boolean.var list, _) Checked.t

      val unpack_flagged :
           Var.t
        -> length:int
        -> (Boolean.var list * [`Success of Boolean.var], _) Checked.t

      val unpack_full :
        Var.t -> (Boolean.var Bitstring_lib.Bitstring.Lsb_first.t, _) Checked.t

      val choose_preimage_var :
        Var.t -> length:int -> (Boolean.var list, _) Checked.t

      type comparison_result = {less: Boolean.var; less_or_equal: Boolean.var}

      val compare :
        bit_length:int -> Var.t -> Var.t -> (comparison_result, _) Checked.t

      val if_ :
        Boolean.var -> then_:Var.t -> else_:Var.t -> (Var.t, _) Checked.t

      module Infix : sig
        val ( + ) : Var.t -> Var.t -> Var.t

        val ( - ) : Var.t -> Var.t -> Var.t

        val ( * ) : field -> Var.t -> Var.t
      end

      module Unsafe : sig
        val of_index : int -> Var.t
      end

      module Assert : sig
        val lte : bit_length:int -> Var.t -> Var.t -> (unit, _) Checked.t

        val gte : bit_length:int -> Var.t -> Var.t -> (unit, _) Checked.t

        val lt : bit_length:int -> Var.t -> Var.t -> (unit, _) Checked.t

        val gt : bit_length:int -> Var.t -> Var.t -> (unit, _) Checked.t

        val not_equal : Var.t -> Var.t -> (unit, _) Checked.t

        val equal : Var.t -> Var.t -> (unit, _) Checked.t

        val non_zero : Var.t -> (unit, _) Checked.t
      end
    end

    val typ : (Var.t, t) Typ.t
  end

  include Monad.Syntax2 with type ('a, 's) t := ('a, 's) Checked.t

  module Proof : sig
    type t

    include Stringable.S with type t := t
  end

  module Bitstring_checked : sig
    type t = Boolean.var list

    val equal : t -> t -> (Boolean.var, _) Checked.t

    val lt_value :
         Boolean.var Bitstring_lib.Bitstring.Msb_first.t
      -> bool Bitstring_lib.Bitstring.Msb_first.t
      -> (Boolean.var, _) Checked.t

    module Assert : sig
      val equal : t -> t -> (unit, _) Checked.t
    end
  end

  module As_prover : sig
    (** An [('a, 'prover_state) t] value uses the current ['prover_state] to
        generate a value of type ['a], and update the ['prover_state] as
        necessary, within a checked computation.
        
        This type specialises the {!type:As_prover.t} type for the backend's
        particular field and variable type. *)
    type ('a, 'prover_state) t

    type ('a, 'prover_state) as_prover = ('a, 'prover_state) t

    (** Mutable references for use by the prover in a checked computation. *)
    module Ref : sig
      (** A mutable reference to an ['a] value, which may be used in checked
          computations. *)
      type 'a t

      val create :
        ('a, 'prover_state) as_prover -> ('a t, 'prover_state) Checked.t

      val get : 'a t -> ('a, _) as_prover

      val set : 'a t -> 'a -> (unit, _) as_prover
    end

    include Monad.S2 with type ('a, 's) t := ('a, 's) t

    val map2 : ('a, 's) t -> ('b, 's) t -> f:('a -> 'b -> 'c) -> ('c, 's) t

    val read_var : Field.Var.t -> (field, 'prover_state) t

    val get_state : ('prover_state, 'prover_state) t

    val set_state : 'prover_state -> (unit, 'prover_state) t

    val modify_state :
      ('prover_state -> 'prover_state) -> (unit, 'prover_state) t

    val read : ('var, 'value) Typ.t -> 'var -> ('value, 'prover_state) t
  end

  module Handle : sig
    type ('var, 'value) t = {var: 'var; value: 'value option}

    val value : (_, 'value) t -> ('value, _) As_prover.t

    val var : ('var, _) t -> 'var
  end

  val assert_ : ?label:string -> Constraint.t -> (unit, 's) Checked.t

  val assert_all : ?label:string -> Constraint.t list -> (unit, 's) Checked.t

  val assert_r1cs :
       ?label:string
    -> Field.Var.t
    -> Field.Var.t
    -> Field.Var.t
    -> (unit, _) Checked.t

  val assert_square :
    ?label:string -> Field.Var.t -> Field.Var.t -> (unit, _) Checked.t

  val as_prover : (unit, 's) As_prover.t -> (unit, 's) Checked.t

  val with_state :
       ?and_then:('s1 -> (unit, 's) As_prover.t)
    -> ('s1, 's) As_prover.t
    -> ('a, 's1) Checked.t
    -> ('a, 's) Checked.t

  val next_auxiliary : (int, 's) Checked.t

  val request_witness :
       ('var, 'value) Typ.t
    -> ('value Request.t, 's) As_prover.t
    -> ('var, 's) Checked.t

  val perform : (unit Request.t, 's) As_prover.t -> (unit, 's) Checked.t

  val request :
       ?such_that:('var -> (unit, 's) Checked.t)
    -> ('var, 'value) Typ.t
    -> 'value Request.t
    -> ('var, 's) Checked.t
  (** TODO: Come up with a better name for this in relation to the above *)

  val exists :
       ?request:('value Request.t, 's) As_prover.t
    -> ?compute:('value, 's) As_prover.t
    -> ('var, 'value) Typ.t
    -> ('var, 's) Checked.t

  type response = Request.response

  val unhandled : response

  type request = Request.request =
    | With :
        { request: 'a Request.t
        ; respond: 'a Request.Response.t -> response }
        -> request

  module Handler : sig
    type t = request -> response
  end

  val handle : ('a, 's) Checked.t -> Handler.t -> ('a, 's) Checked.t

  val with_label : string -> ('a, 's) Checked.t -> ('a, 's) Checked.t

  val constraint_system :
       exposing:((unit, 's) Checked.t, _, 'k_var, _) Data_spec.t
    -> 'k_var
    -> R1CS_constraint_system.t

  val generate_keypair :
       exposing:((unit, 's) Checked.t, _, 'k_var, _) Data_spec.t
    -> 'k_var
    -> Keypair.t

  val conv :
       ('r_var -> 'r_value)
    -> ('r_var, 'r_value, 'k_var, 'k_value) Data_spec.t
    -> 'k_var
    -> 'k_value

  val prove :
       Proving_key.t
    -> ((unit, 's) Checked.t, Proof.t, 'k_var, 'k_value) Data_spec.t
    -> 's
    -> 'k_var
    -> 'k_value

  val verify :
       Proof.t
    -> Verification_key.t
    -> (_, bool, _, 'k_value) Data_spec.t
    -> 'k_value

  val run_unchecked : ('a, 's) Checked.t -> 's -> 's * 'a

  val run_and_check :
    (('a, 's) As_prover.t, 's) Checked.t -> 's -> ('s * 'a) Or_error.t

  val check : ('a, 's) Checked.t -> 's -> bool

  val constraint_count :
    ?log:(?start:bool -> string -> int -> unit) -> (_, _) Checked.t -> int

  module Test : sig
    val checked_to_unchecked :
         ('vin, 'valin) Typ.t
      -> ('vout, 'valout) Typ.t
      -> ('vin -> ('vout, unit) Checked.t)
      -> 'valin
      -> 'valout

    val test_equal :
         ?sexp_of_t:('valout -> Sexp.t)
      -> ?equal:('valout -> 'valout -> bool)
      -> ('vin, 'valin) Typ.t
      -> ('vout, 'valout) Typ.t
      -> ('vin -> ('vout, unit) Checked.t)
      -> ('valin -> 'valout)
      -> 'valin
      -> unit
  end
end

module type S = sig
  include Basic

  module Number :
    Number_intf.S
    with type ('a, 'b) checked := ('a, 'b) Checked.t
     and type field := field
     and type field_var := Field.Var.t
     and type bool_var := Boolean.var

  module Enumerable (M : sig
    type t [@@deriving enum]
  end) :
    Enumerable_intf.S
    with type ('a, 'b) checked := ('a, 'b) Checked.t
     and type ('a, 'b) typ := ('a, 'b) Typ.t
     and type bool_var := Boolean.var
     and type var = Field.Var.t
     and type t := M.t
end
