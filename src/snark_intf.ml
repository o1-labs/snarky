module Bignum_bigint = Bigint
open Core_kernel
module Constraint0 = Constraint
module Boolean0 = Boolean
module Typ0 = Typ

(** Yojson-compatible JSON type. *)
type 'a json =
  [> `String of string
  | `Assoc of (string * 'a json) list
  | `List of 'a json list ]
  as
  'a

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

  (** The finite field over which the R1CS operates. *)
  type field

  (** The rank-1 constraint system used by this instance. See
      {!module:Backend_intf.S.R1CS_constraint_system}. *)
  module R1CS_constraint_system : sig
    type t

    val digest : t -> Md5.t

    val constraints : t -> field Cvar.t Constraint0.t
    (** Extract the constraints from the constraint system. *)

    val to_json : t -> 'a json
    (** Convert a basic constraint into a JSON representation.

        This representation is compatible with the Yojson library, which can be
        used to print JSON to the screen, write it to a file, etc.
    *)
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

  module Bigint : sig
    include Bigint_intf.Extended with type field := field

    val of_bignum_bigint : Bignum_bigint.t -> t

    val to_bignum_bigint : t -> Bignum_bigint.t
  end

  (** Rank-1 constraints over {!type:Var.t}s. *)
  module rec Constraint : sig
    (** The type of constraints.
        In the proof system, every constraint is a rank-1 constraint; that is,
        the constraint takes the form [a * b = c] for some [a], [b] and [c]
        which are made up of some linear combination of {!type:Field.Var.t}s.

        For example, a constraint could be [(w + 2*x) * (y + z) = a + b], where
        [w], [x], [y], [z], [a], and [b] are field variables.
        Note that a linear combination is the result of adding together some of
        these variables, each multiplied by a field constant ({!type:Field.t});
        any time we want to multiply our *variables*, we need to add a new
        rank-1 constraint.
    *)
    type t = Field.Var.t Constraint0.t

    type 'k with_constraint_args = ?label:string -> 'k

    val boolean : (Field.Var.t -> t) with_constraint_args
    (** A constraint that asserts that the field variable is a boolean: either
        {!val:Field.zero} or {!val:Field.one}.
    *)

    val equal : (Field.Var.t -> Field.Var.t -> t) with_constraint_args
    (** A constraint that asserts that the field variable arguments are equal.
    *)

    val r1cs :
      (Field.Var.t -> Field.Var.t -> Field.Var.t -> t) with_constraint_args
    (** A bare rank-1 constraint. *)

    val square : (Field.Var.t -> Field.Var.t -> t) with_constraint_args
    (** A constraint that asserts that the first variable squares to the
        second, ie. [square x y] => [x*x = y] within the field.
    *)

    val basic_to_json : Field.Var.t Constraint0.basic -> 'a json
    (** Convert a basic constraint into a JSON representation.

        This representation is compatible with the Yojson library, which can be
        used to print JSON to the screen, write it to a file, etc.
    *)

    val to_json : t -> 'a json
    (** Convert a constraint into a JSON representation.

        This representation is compatible with the Yojson library, which can be
        used to print JSON to the screen, write it to a file, etc.
    *)
  end

  (** The data specification for checked computations. *)
  and Data_spec : sig
    (** A list of {!type:Typ.t} values, describing the inputs to a checked
        computation. The type [('r_var, 'r_value, 'k_var, 'k_value) t]
        represents
        - ['k_value] is the OCaml type of the computation
        - ['r_value] is the OCaml type of the result
        - ['k_var] is the type of the computation within the R1CS
        - ['k_value] is the type of the result within the R1CS.

        This functions the same as OCaml's default list type:
{[
  Data_spec.[typ1; typ2; typ3]

  Data_spec.(typ1 :: typs)

  let open Data_spec in
  [typ1; typ2; typ3; typ4; typ5]

  let open Data_spec in
  typ1 :: typ2 :: typs

]}
        all function as you would expect.
    *)
    type ('r_var, 'r_value, 'k_var, 'k_value) t =
      ('r_var, 'r_value, 'k_var, 'k_value, field) Typ0.Data_spec.t

    val size : (_, _, _, _) t -> int
    (** [size [typ1; ...; typn]] returns the number of {!type:Var.t} variables
        allocated by allocating [typ1], followed by [typ2], etc. *)

    include module type of Typ0.Data_spec0
  end

  (** Mappings from OCaml types to R1CS variables and constraints. *)
  and Typ : sig
    module Store : sig
      (** A ['value Store.t] value describes storing {!type:Field.t}s in
          variables for use in the R1CS. It is a monad, which lets us combine
          these variables to create more complex values.

          For example, we can store a 3-tuple of values by writing
{[
  let store3 (store_a : 'a Store.t) (store_b : 'b Store.t)
    (store_c : 'b Store.t) : ('a, 'b, 'c) Store.t =
    let open Store in
    let%map a = store_a
    and b = store_b
    and c = store_c
    in
    (a, b, c)
]}
      *)
      include Monad_let.S with type 'a t = ('a, Field.t) Typ_monads.Store.t

      val store : field -> Field.Var.t t
      (** Store a single field element for the R1CS, and return the variable
          that refers to it. *)
    end

    module Alloc : sig
      (** A ['value Alloc.t] describes allocating variables for the R1CS to use
          without explicitly giving the values to store in them. This is
          analogous to {!type:Store.t}.

          The main use of this is in generating the constraint system and
          generating the {!type:Keypair.t} with {!val:generate_keypair}; we
          can't know yet what values we will want to store in the variables,
          but we still want to know what constraints they will have to satisfy.
      *)
      include Monad_let.S with type 'a t = ('a, Field.t) Typ_monads.Alloc.t

      val alloc : Field.Var.t t
      (** Allocate a variable in the R1CS that can hold a single field element.
      *)
    end

    module Read : sig
      (** A ['value Read.t] describes reading values back out of the R1CS
          variables, so that the prover can use them in {!module:As_prover}
          blocks. It is a monad, which lets us combine these values to
          create more complex ones.

          For example, we can create a record containing the results of some
          reads by writing
{[
  type ('a, 'b) t = {a : 'a; b : 'b}

  let read_t (read_a : 'a Read.t) (read_b : 'b Read.t) : ('a, 'b) t =
    let open Read in
    let%map a = read_a
    and b = read_b
    in
    {a; b}
]}
      *)
      include Monad_let.S with type 'a t = ('a, Field.t) Typ_monads.Read.t

      val read : Field.Var.t -> field t
      (** Read the contents of a single R1CS variable. *)
    end

    (** The type [('var, 'value) t] describes a mapping from the OCaml type
        ['value] to a type representing the value using R1CS variables
        (['var]).
        This description includes
        - a {!type:Store.t} for storing ['value]s as ['var]s
        - a {!type:Alloc.t} for creating a ['var] when we don't know what values
          it should contain yet
        - a {!type:Read.t} for reading the contents of the ['var] back out as a
          ['value] in {!module:As_prover} blocks
        - a {!type:Checked.t} for asserting constraints on the ['var] -- for
          example, that a [Boolean.t] is either a {!val:Field.zero} or a
          {!val:Field.one}.
    *)
    type ('var, 'value) t =
      ('var, 'value, Field.t, (unit, unit) Checked.t) Types.Typ.t

    (** Accessors for {!type:Types.Typ.t} fields: *)

    val store : ('var, 'value) t -> 'value -> 'var Store.t
    (** [store typ x] stores [x] as a ['var] according to the description given
        by [typ].
    *)

    val read : ('var, 'value) t -> 'var -> 'value Read.t
    (** [read typ x] reads [x] as a ['value] according to the description given
    by [typ].
    *)

    val alloc : ('var, 'value) t -> 'var Alloc.t
    (** [alloc typ] allocates the R1CS variables necessary to represent a
        ['value] and creates a ['var] from them, according to the description
        given by [typ].
    *)

    val check : ('var, 'value) t -> 'var -> (unit, _) Checked.t
    (** [check typ x] runs a checked computation to generate the constraints
        described by [typ] that [x] should satisfy.
    *)

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
    (** synonym for {!val:tuple2} *)

    val tuple3 :
         ('var1, 'value1) t
      -> ('var2, 'value2) t
      -> ('var3, 'value3) t
      -> ('var1 * 'var2 * 'var3, 'value1 * 'value2 * 'value3) t

    val list : length:int -> ('var, 'value) t -> ('var list, 'value list) t
    (** [list ~length typ] describes how to convert between a ['value list] and
        a ['var list], given a description of how to convert between a ['value]
        and a ['var].

        [length] must be the length of the lists that are converted. This value
        must be constant for every use; otherwise the constraint system may use
        a different number of variables depending on the data given.

        Passing a list of the wrong length throws an error.
    *)

    val array : length:int -> ('var, 'value) t -> ('var array, 'value array) t
    (** [array ~length typ] describes how to convert between a ['value array]
        and a ['var array], given a description of how to convert between a
        ['value] and a ['var].

        [length] must be the length of the arrays that are converted. This
        value must be constant for every use; otherwise the constraint system
        may use a different number of variables depending on the data given.

        Passing an array of the wrong length throws an error.
    *)

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
    (** A specialised version of {!val:transport}/{!val:transport_var} that
        describes the relationship between ['var] and ['value] in terms of a
        {!type:Data_spec.t}.
    *)

    module Of_traversable (T : Traversable.S) : sig
      val typ :
        template:unit T.t -> ('var, 'value) t -> ('var T.t, 'value T.t) t
    end

    include module type of Types.Typ.T

    val int : (int, int) t

    val string : (string, string) t

    val char : (char, char) t

    val big_int : (Big_int.big_int, Big_int.big_int) t
  end

  (** Representation of booleans within a field.

      This representation ties the value of [true] to {!val:Field.one} and
      [false] to {!val:Field.zero}, adding a check in {!val:Boolean.typ} to
      ensure that these are the only vales. *)
  and Boolean : sig
    (** The type that stores booleans as R1CS variables. *)
    type var = Field.Var.t Boolean0.t

    type value = bool

    val to_field : var -> Field.Var.t

    val true_ : var
    (** An R1CS variable containing {!val:Field.one}, representing [true]. *)

    val false_ : var
    (** An R1CS variable containing {!val:Field.zero}, representing [false]. *)

    val if_ : var -> then_:var -> else_:var -> (var, _) Checked.t
    (** [if_ b ~then_ ~else_] returns [then_] if [b] is true, or [else_]
        otherwise.
    *)

    val not : var -> var
    (** Negate a boolean value *)

    val ( && ) : var -> var -> (var, _) Checked.t
    (** Boolean and *)

    val ( || ) : var -> var -> (var, _) Checked.t
    (** Boolean or *)

    val ( lxor ) : var -> var -> (var, _) Checked.t
    (** Boolean xor (exclusive-or) *)

    val any : var list -> (var, _) Checked.t
    (** Returns [true] if any value in the list is true, false otherwise. *)

    val all : var list -> (var, _) Checked.t
    (** Returns [true] if all value in the list are true, false otherwise. *)

    val of_field : Field.Var.t -> (var, _) Checked.t
    (** Convert a value in a field to a boolean, adding checks to the R1CS that
       it is a valid boolean value. *)

    val var_of_value : value -> var
    (** Convert an OCaml [bool] into a R1CS variable representing the same
        value. *)

    val typ : (var, value) Typ.t
    (** The relationship between {!val:var} and {!val:value}, with a check that
        the value is valid (ie. {!val:Field.zero} or {!val:Field.one}). *)

    val typ_unchecked : (var, value) Typ.t
    (** {!val:typ} without a validity check for the underlying field value. *)

    val equal : var -> var -> (var, _) Checked.t

    (** Build trees representing boolean expressions. *)
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

      val equal : Boolean.var -> Boolean.var -> (unit, _) Checked.t

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

    type 'prover_state run_state = ('prover_state, Field.t) Run_state.t

    include
      Monad_let.S2 with type ('a, 's) t = ('a, 's, Field.t) Types.Checked.t

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
    (** The finite field over which the R1CS operates.
        Values may be between 0 and {!val:size}. *)
    type t = field [@@deriving bin_io, sexp, hash, compare]

    val gen : t Core_kernel.Quickcheck.Generator.t
    (** A generator for Quickcheck tests. *)

    include Field_intf.Extended with type t := t

    include Stringable.S with type t := t

    val size : Bignum_bigint.t
    (** The number at which values in the field wrap back around to 0. *)

    val unpack : t -> bool list
    (** Convert a field element into its constituent bits. *)

    val project : bool list -> t
    (** Convert a list of bits into a field element. This is the inverse of
        unpack.
    *)

    val project_reference : bool list -> t
    (** [project], but slow. Exposed for benchmarks. *)

    type var' = Var.t

    module Var : sig
      (** The type that stores booleans as R1CS variables. *)
      type t = field Cvar.t

      val length : t -> int
      (** For debug purposes *)

      val var_indices : t -> int list

      val to_constant_and_terms : t -> field option * (field * Var.t) list
      (** Convert a {!type:t} value to its constituent constant and a list of
          scaled R1CS variables. *)

      val constant : field -> t
      (** [constant x] creates a new R1CS variable containing the constant
          field element [x]. *)

      val to_constant : t -> field option
      (** [to_constant x] returns [Some f] if x holds only the constant field
          element [f]. Otherwise, it returns [None].
      *)

      val linear_combination : (field * t) list -> t
      (** [linear_combination [(f1, x1);...;(fn, xn)]] returns the result of
          calculating [f1 * x1 + f2 * x2 + ... + fn * xn].
          This does not add a new constraint; see {!type:Constraint.t} for more
          information.
      *)

      val sum : t list -> t
      (** [sum l] returns the sum of all R1CS variables in [l].

          If the result would be greater than or equal to {!val:Field.size}
          then the value will overflow to be less than {!val:Field.size}.
      *)

      val add : t -> t -> t
      (** [add x y] returns the result of adding the R1CS variables [x] and
          [y].

          If the result would be greater than or equal to {!val:Field.size}
          then the value will overflow to be less than {!val:Field.size}.
      *)

      val sub : t -> t -> t
      (** [sub x y] returns the result of subtracting the R1CS variables [x]
          and [y].

          If the result would be less than 0 then the value will underflow
          to be between 0 and {!val:Field.size}.
      *)

      val scale : t -> field -> t
      (** [scale x f] returns the result of multiplying the R1CS variable [x]
          by the constant field element [f].

          If the result would be greater than or equal to {!val:Field.size}
          then the value will overflow to be less than {!val:Field.size}.
      *)

      val project : Boolean.var list -> t
      (** Convert a list of bits into a field element.

          [project [b1;...;bn] = b1 + 2*b2 + 4*b3 + ... + 2^(n-1) * bn]

          If the result would be greater than or equal to {!val:Field.size}
          then the value will overflow to be less than {!val:Field.size}.
      *)

      val pack : Boolean.var list -> t
      (** Convert a list of bits into a field element.

          [pack [b1;...;bn] = b1 + 2*b2 + 4*b3 + ... + 2^(n-1) * bn]

          This will raise an assertion error if the length of the list is not
          strictly less than number of bits in {!val:Field.size}.

          Use [project] if you know that the list represents a value less than
          {!val:Field.size} but where the number of bits may be the maximum, or
          where overflow is appropriate.
      *)
    end

    module Checked : sig
      val mul : Var.t -> Var.t -> (Var.t, _) Checked.t
      (** [mul x y] returns the result of multiplying the R1CS variables [x]
          and [y].

          If the result would be greater than or equal to {!val:Field.size}
          then the value will overflow to be less than {!val:Field.size}.
      *)

      val square : Var.t -> (Var.t, _) Checked.t
      (** [square x] returns the result of multiplying the R1CS variables [x]
          by itself.

          If the result would be greater than or equal to {!val:Field.size}
          then the value will overflow to be less than {!val:Field.size}.
      *)

      val div : Var.t -> Var.t -> (Var.t, _) Checked.t
      (** [div x y] returns the result of dividing the R1CS variable [x] by
          [y].

          If [x] is not an integer multiple of [y], the result could be any
          value; it is equivalent to computing [mul x (inv y)].

          If [y] is 0, this raises a [Failure].
      *)

      val inv : Var.t -> (Var.t, _) Checked.t
      (** [inv x] returns the value such that [mul x (inv x) = 1].

          If [x] is 0, this raises a [Failure].
      *)

      val equal : Var.t -> Var.t -> (Boolean.var, 's) Checked.t
      (** [equal x y] returns a R1CS variable containing the value [true] if
          the R1CS variables [x] and [y] are equal, or [false] otherwise.
      *)

      val unpack : Var.t -> length:int -> (Boolean.var list, _) Checked.t
      (** [unpack x ~length] returns a list of R1CS variables containing the
          [length] lowest bits of [x]. If [length] is greater than the number
          of bits in {!val:Field.size} then this raises a [Failure].

          For example,
          - [unpack 8 ~length:4 = [0; 0; 0; 1]]
          - [unpack 9 ~length:3 = [1; 0; 0]]
          - [unpack 9 ~length:5 = [1; 0; 0; 1; 0]]
      *)

      val unpack_flagged :
           Var.t
        -> length:int
        -> (Boolean.var list * [`Success of Boolean.var], _) Checked.t
      (** [unpack x ~length = (unpack x ~length, `Success success)], where
          [success] is an R1CS variable containing [true] if the returned bits
          represent [x], and [false] otherwise.

          If [length] is greater than the number of bits in {!val:Field.size}
          then this raises a [Failure].
      *)

      val unpack_full :
        Var.t -> (Boolean.var Bitstring_lib.Bitstring.Lsb_first.t, _) Checked.t
      (** [unpack x ~length] returns a list of R1CS variables containing the
          bits of [x].
      *)

      val choose_preimage_var :
        Var.t -> length:int -> (Boolean.var list, _) Checked.t
      (** [unpack x ~length] returns a list of R1CS variables containing the
          [length] lowest bits of [x].
      *)

      (** The type of results from checked comparisons, stored as boolean R1CS
          variables.
      *)
      type comparison_result = {less: Boolean.var; less_or_equal: Boolean.var}

      val compare :
        bit_length:int -> Var.t -> Var.t -> (comparison_result, _) Checked.t
      (** [compare ~bit_length x y] compares the [bit_length] lowest bits of
          [x] and [y].

          This requires converting the R1CS variables [x] and [y] into a list
          of bits.
      *)

      val if_ :
        Boolean.var -> then_:Var.t -> else_:Var.t -> (Var.t, _) Checked.t
      (** [if_ b ~then_ ~else_] returns [then_] if [b] is true, or [else_]
          otherwise.
      *)

      (** Infix notations for the basic field operations. *)

      val ( + ) : Var.t -> Var.t -> Var.t

      val ( - ) : Var.t -> Var.t -> Var.t

      val ( * ) : field -> Var.t -> Var.t

      module Unsafe : sig
        val of_index : int -> Var.t
      end

      (** Assertions *)
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
    (** Describes how to convert between {!type:t} and {!type:Var.t} values. *)
  end

  type boolean = Boolean.var

  module Let_syntax :
    Monad_let.Syntax2 with type ('a, 's) t := ('a, 's) Checked.t

  (** Zero-knowledge proofs generated from checked computations. *)
  module Proof : sig
    type t

    (** The type of messages that can be associated with a proof. *)
    type message

    include Binable.S with type t := t
  end

  (** Utility functions for dealing with lists of bits in the R1CS. *)
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

  (** Code that can be run by the prover only, using 'superpowers' like looking
      at the contents of R1CS variables and creating new variables from other
      OCaml values.
  *)
  module As_prover : sig
    (** An [('a, 'prover_state) t] value uses the current ['prover_state] to
        generate a value of type ['a], and update the ['prover_state] as
        necessary, within a checked computation.

        This type specialises the {!type:As_prover.t} type for the backend's
        particular field and variable type. *)
    type ('a, 'prover_state) t = ('a, field, 'prover_state) As_prover.t

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

    include Monad_let.S2 with type ('a, 's) t := ('a, 's) t

    val map2 : ('a, 's) t -> ('b, 's) t -> f:('a -> 'b -> 'c) -> ('c, 's) t
    (** Combine 2 {!type:As_prover.t} blocks using another function. *)

    val read_var : Field.Var.t -> (field, 'prover_state) t
    (** Read the contents of a R1CS variable representing a single field
        element. *)

    val get_state : ('prover_state, 'prover_state) t
    (** Read the ['prover_state] carried by the {!type:As_prover.t} monad. *)

    val set_state : 'prover_state -> (unit, 'prover_state) t
    (** Update the ['prover_state] carried by the {!type:As_prover.t} monad. *)

    val modify_state :
      ('prover_state -> 'prover_state) -> (unit, 'prover_state) t
    (** Change the ['prover_state] carried by the {!type:As_prover.t} monad. *)

    val read : ('var, 'value) Typ.t -> 'var -> ('value, 'prover_state) t
    (** [read typ x] reads the contents of the R1CS variables in [x] to create
        an OCaml variable of type ['value], according to the description given
        by [typ].
    *)

    val with_lens : ('whole, 'lens) Lens.t -> ('a, 'lens) t -> ('a, 'whole) t
    (** [with_lens lens as_prover] uses the {!type:Lens.t} provided to lift the
        prover state of [as_prover] to ['whole] from a sub-type ['lens].
    *)
  end

  (** Representation of an R1CS value and an OCaml value (if running as the
      prover) together.
  *)
  module Handle : sig
    type ('var, 'value) t

    val value : (_, 'value) t -> ('value, _) As_prover.t
    (** Get the value of a handle as the prover. *)

    val var : ('var, _) t -> 'var
    (** Get the R1CS representation of a value. *)
  end

  (** Utility functions for calling single checked computations. *)
  module Runner : sig
    type 's state

    val run : ('a, 's) Checked.t -> 's state -> 's state * 'a
  end

  type response = Request.response

  val unhandled : response

  (** The argument type for request handlers.

{[
  type _ Request.t += My_request : 'a list -> 'a Request.t

  let handled (c : ('a, _) Checked.t) : ('a, _) Checked.t =
    handle (fun (With {request; respond}) ->
      match request with
      | My_request l ->
        let x = (* Do something with l to create a single value. *) in
        respond (Provide x)
      | _ -> unhandled )
]}
  *)
  type request = Request.request =
    | With :
        { request: 'a Request.t
        ; respond: 'a Request.Response.t -> response }
        -> request

  (** The type of handlers. *)
  module Handler : sig
    type t = request -> response
  end

  (** The interface for managing proof systems. *)
  module Proof_system : sig
    (** A proof system instance for a checked computation producing a value of
        type ['a], with prover state ['s] and public inputs ['public_input].
    *)
    type ('a, 's, 'public_input) t

    val create :
         ?proving_key:Proving_key.t
      -> ?verification_key:Verification_key.t
      -> ?proving_key_path:string
      -> ?verification_key_path:string
      -> ?handlers:Handler.t list
      -> ?reduce:bool
      -> public_input:( ('a, 's) Checked.t
                      , unit
                      , 'computation
                      , 'public_input )
                      Data_spec.t
      -> 'computation
      -> ('a, 's, 'public_input) t
    (** Create a new proof system. The arguments are
        - [proving_key] -- optional, defines the key to be used for proving.
          If not present, a key will be read from [proving_key_path], or one
          will be generated automatically.
        - [verification_key] -- optional, defines the key to be used for
          verification of a proof.
          If not present, a key will be read from [verification_key_path], or
          one will be generated automatically.
        - [proving_key_path] -- optional, defines the path to a file where the
          proving key can be found. If the file does not exist and no
          [proving_key] argument is given, the generated key will be written to
          this file.
        - [verification_key_path] -- optional, defines the path to a file where
          the verification key can be found. If the file does not exist and no
          [verification_key] argument is given, the generated key will be
          written to this file.
        - [handlers] -- optional, the list of handlers that should be used to
          handle requests made from the checked computation
        - [reduce] -- optional, default [false], whether to perform the
          [reduce_to_caller] optimisation while creating the proof system
        - [public_input] -- the {!type:Data_spec.t} that describes the form
          that the public inputs must take
        - ['computation] -- a checked computation that takes as arguments
          values with the types described by [public_input] to the output type.
    *)

    val constraint_system :
      ('a, 's, 'public_input) t -> R1CS_constraint_system.t
    (** The constraint system that this proof system's checked computation
        describes.
    *)

    val digest : ('a, 's, 'public_input) t -> Md5_lib.t
    (** The MD5 hash of the constraint system. *)

    val generate_keypair : ('a, 's, 'public_input) t -> Keypair.t
    (** Generate a keypair for the checked computation, writing it to the
        [proving_key_path] and [verification_key_path], if set.
    *)

    val run_unchecked :
         public_input:(unit, 'public_input) H_list.t
      -> ?handlers:Handler.t list
      -> ?reduce:bool
      -> ('a, 's, 'public_input) t
      -> ('a -> ('b, 's) As_prover.t)
      -> 's
      -> 's * 'b
    (** Run the checked computation as the prover, without checking any
        constraints.

        [run_unchecked ~public_input proof_system eval prover_state] runs the
        checked computation described by [proof_system] with public input
        [public_input], then evaluates the result using [eval]. [eval] may be
        used to convert proof system variables back into OCaml values; see
        {!module:As_prover} for the available functions.
    *)

    val run_checked :
         public_input:(unit, 'public_input) H_list.t
      -> ?handlers:Handler.t list
      -> ?reduce:bool
      -> ('a, 's, 'public_input) t
      -> ('a -> ('b, 's) As_prover.t)
      -> 's
      -> ('s * 'b) Or_error.t
    (** Run the checked computation as the prover, checking any constraints.

        [run_checked ~public_input proof_system eval prover_state] runs the
        checked computation described by [proof_system] with public input
        [public_input], then evaluates the result using [eval]. [eval] may be
        used to convert proof system variables back into OCaml values; see
        {!module:As_prover} for the available functions.
    *)

    val check :
         public_input:(unit, 'public_input) H_list.t
      -> ?handlers:Handler.t list
      -> ?reduce:bool
      -> ('a, 's, 'public_input) t
      -> 's
      -> unit Or_error.t
    (** Run the checked computation as the prover, returning [Ok ()] if all of
        the constraints are correct, or an error describing which constraint
        was not satisfied.
    *)

    val prove :
         public_input:(unit, 'public_input) H_list.t
      -> ?proving_key:Proving_key.t
      -> ?handlers:Handler.t list
      -> ?reduce:bool
      -> ?message:Proof.message
      -> ('a, 's, 'public_input) t
      -> 's
      -> Proof.t
    (** Run the checked computation as the prover, generating a {!type:Proof.t}
        that the verifier may check efficiently.
        - The [proving_key] argument overrides the argument given to
          {!val:create}, if any.
        - The [handlers] argument adds handlers to those already given to
          {!val:create}. If handlers for the same requests were provided to
          both, the ones passed here are given priority.
        - The [reduce] argument determines whether to run use the
          [reduce_to_prover]-optimised checked computation. The default value
          may be changed with {!val:Snark0.set_reduce_to_prover}.
        - The [message] argument specifies the message to associate with the
          proof, if any.
    *)

    val verify :
         public_input:(unit, 'public_input) H_list.t
      -> ?verification_key:Verification_key.t
      -> ?message:Proof.message
      -> ('a, 's, 'public_input) t
      -> Proof.t
      -> bool
    (** Verify a {!type:Proof.t} generated by a prover.
       [verification_key] overrides the argument given to {!val:create}, if
       any.
    *)
  end

  (** Utility functions for running different representations of checked
      computations using a standard interface.
  *)
  module Perform : sig
    type ('a, 's, 't) t = 't -> 's Runner.state -> 's Runner.state * 'a

    val constraint_system :
         run:('a, 's, 't) t
      -> exposing:('t, _, 'k_var, _) Data_spec.t
      -> 'k_var
      -> R1CS_constraint_system.t

    val generate_keypair :
         run:('a, 's, 't) t
      -> exposing:('t, _, 'k_var, _) Data_spec.t
      -> 'k_var
      -> Keypair.t

    val prove :
         run:('a, 's, 't) t
      -> ?message:Proof.message
      -> Proving_key.t
      -> ('t, Proof.t, 'k_var, 'k_value) Data_spec.t
      -> 'k_var
      -> 's
      -> 'k_value

    val verify :
         ?message:Proof.message
      -> Proof.t
      -> Verification_key.t
      -> (_, bool, _, 'k_value) Data_spec.t
      -> 'k_value

    val run_unchecked : run:('a, 's, 't) t -> 't -> 's -> 's * 'a

    val run_and_check :
      run:(('a, 's) As_prover.t, 's, 't) t -> 't -> 's -> ('s * 'a) Or_error.t

    val check : run:('a, 's, 't) t -> 't -> 's -> unit Or_error.t
  end

  val assert_ : ?label:string -> Constraint.t -> (unit, 's) Checked.t
  (** Add a constraint to the constraint system, optionally with the label
      given by [label]. *)

  val assert_all : ?label:string -> Constraint.t list -> (unit, 's) Checked.t
  (** Add all of the constraints in the list to the constraint system,
      optionally with the label given by [label].
  *)

  val assert_r1cs :
       ?label:string
    -> Field.Var.t
    -> Field.Var.t
    -> Field.Var.t
    -> (unit, _) Checked.t
  (** Add a rank-1 constraint to the constraint system, optionally with the
      label given by [label].

      See {!val:Constraint.r1cs} for more information on rank-1 constraints.
  *)

  val assert_square :
    ?label:string -> Field.Var.t -> Field.Var.t -> (unit, _) Checked.t
  (** Add a 'square' constraint to the constraint system, optionally with the
      label given by [label].

      See {!val:Constraint.square} for more information.
  *)

  val as_prover : (unit, 's) As_prover.t -> (unit, 's) Checked.t
  (** Run an {!module:As_prover} block. *)

  val with_state :
       ?and_then:('s1 -> (unit, 's) As_prover.t)
    -> ('s1, 's) As_prover.t
    -> ('a, 's1) Checked.t
    -> ('a, 's) Checked.t
  (** Update the prover state by running an {!module:As_prover} block. *)

  val next_auxiliary : (int, 's) Checked.t
  (** Internal: read the value of the next unused auxiliary input index. *)

  val request_witness :
       ('var, 'value) Typ.t
    -> ('value Request.t, 's) As_prover.t
    -> ('var, 's) Checked.t
  (** [request_witness typ create_request] runs the [create_request]
      {!type:As_prover.t} block to generate a {!type:Request.t}.

      This allows us to introduce values into the R1CS without passing them as
      public inputs.

      If no handler for the request is attached by {!val:handle}, this raises
      a [Failure].
  *)

  val perform : (unit Request.t, 's) As_prover.t -> (unit, 's) Checked.t
  (** Like {!val:request_witness}, but the request doesn't return any usable
      value.
  *)

  val request :
       ?such_that:('var -> (unit, 's) Checked.t)
    -> ('var, 'value) Typ.t
    -> 'value Request.t
    -> ('var, 's) Checked.t
  (** Like {!val:request_witness}, but generates the request without using
      any {!module:As_prover} 'superpowers'.

      The argument [such_that] allows adding extra constraints on the returned
      value.

      (* TODO: Come up with a better name for this in relation to the above *)
  *)

  val exists :
       ?request:('value Request.t, 's) As_prover.t
    -> ?compute:('value, 's) As_prover.t
    -> ('var, 'value) Typ.t
    -> ('var, 's) Checked.t
  (** Introduce a value into the R1CS.
      - The [request] argument functions like {!val:request_witness}, creating
        a request and returning the result.
      - If no [request] argument is given, or if the [request] isn't handled,
        then [compute] is run to create a value.

      If [compute] is not given and [request] fails/is also not given, then
      this function raises a [Failure].
  *)

  val exists_handle :
       ?request:('value Request.t, 's) As_prover.t
    -> ?compute:('value, 's) As_prover.t
    -> ('var, 'value) Typ.t
    -> (('var, 'value) Handle.t, 's) Checked.t
  (** Like {!val:exists}, but returns a {!type:Handle.t}.

      This persists the OCaml value of the result, which is stored unchanged in
      the {!type:Handle.t} and can be recalled in later {!module:As_prover}
      blocks using {!val:Handle.value}.
  *)

  val handle : ('a, 's) Checked.t -> Handler.t -> ('a, 's) Checked.t
  (** Add a request handler to the checked computation, to be used by
      {!val:request_witness}, {!val:perform}, {!val:request} or {!val:exists}.
  *)

  val handle_as_prover :
    ('a, 's) Checked.t -> (Handler.t, 's) As_prover.t -> ('a, 's) Checked.t
  (** Generate a handler using the {!module:As_prover} 'superpowers', and use
      it for {!val:request_witness}, {!val:perform}, {!val:request} or
      {!val:exists} calls in the wrapped checked computation.
  *)

  val with_label : string -> ('a, 's) Checked.t -> ('a, 's) Checked.t
  (** Add a label to all of the constraints added in the checked computation.
      If a constraint is checked and isn't satisfied, this label will be shown
      in the error message.
  *)

  val constraint_system :
       exposing:((unit, 's) Checked.t, _, 'k_var, _) Data_spec.t
    -> 'k_var
    -> R1CS_constraint_system.t
  (** Generate the R1CS for the checked computation. *)

  val with_lens :
    ('whole, 'lens) Lens.t -> ('a, 'lens) Checked.t -> ('a, 'whole) Checked.t
  (** [with_lens lens t] uses the {!type:Lens.t} provided to lift the prover
      state of [as_prover] to ['whole] from a sub-type ['lens].
  *)

  val generate_keypair :
       exposing:((unit, 's) Checked.t, _, 'k_var, _) Data_spec.t
    -> 'k_var
    -> Keypair.t
  (** Create a new keypair for the R1CS generated by the checked computation.
  *)

  val conv :
       ('r_var -> 'r_value)
    -> ('r_var, 'r_value, 'k_var, 'k_value) Data_spec.t
    -> 'k_var
    -> 'k_value
  (** Internal: supplies arguments to a checked computation by storing them
      according to the {!type:Data_spec.t} and passing the R1CS versions.
  *)

  val prove :
       ?message:Proof.message
    -> Proving_key.t
    -> ((unit, 's) Checked.t, Proof.t, 'k_var, 'k_value) Data_spec.t
    -> 's
    -> 'k_var
    -> 'k_value
  (** Run the checked computation, creating a proof that it has been run
      correctly (ie. satisfies its constraints).
  *)

  val verify :
       ?message:Proof.message
    -> Proof.t
    -> Verification_key.t
    -> (_, bool, _, 'k_value) Data_spec.t
    -> 'k_value
  (** Verify a {!type:Proof.t} generated from a checked computation. *)

  val run_unchecked : ('a, 's) Checked.t -> 's -> 's * 'a
  (** Run a checked computation as the prover, without checking the
      constraints. *)

  val run_and_check :
    (('a, 's) As_prover.t, 's) Checked.t -> 's -> ('s * 'a) Or_error.t
  (** Run a checked computation as the prover, checking the constraints. *)

  val check : ('a, 's) Checked.t -> 's -> unit Or_error.t
  (** Run a checked computation as the prover, returning [true] if the
      constraints are all satisfied, or [false] otherwise. *)

  val generate_auxiliary_input :
       (('a, 's) Checked.t, unit, 'k_var, 'k_value) Data_spec.t
    -> 's
    -> 'k_var
    -> 'k_value
  (** Run the checked computation and generate the auxiliary input, but don't
      generate a proof.

      Returns [unit]; this is for testing only.
  *)

  val constraint_count :
    ?log:(?start:bool -> string -> int -> unit) -> (_, _) Checked.t -> int
  (** Returns the number of constraints in the constraint system.

      The optional [log] argument is called at the start and end of each
      [with_label], with the arguments [log ?start label count], where:
      - [start] is [Some true] if it the start of the [with_label], or [None]
        otherwise
      - [label] is the label added by [with_label]
      - [count] is the number of constraints at that point.
  *)

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

  val set_constraint_logger : (Constraint.t -> unit) -> unit

  val clear_constraint_logger : unit -> unit
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

(** The imperative interface to Snarky. *)
module type Run_basic = sig
  (** The type of state that As_prover blocks may read to/write from. *)
  type prover_state

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

  (** Rank-1 constraints over {!type:Field.t}s. *)
  module rec Constraint : sig
    type t = Field.t Constraint0.t

    type 'k with_constraint_args = ?label:string -> 'k

    val boolean : (Field.t -> t) with_constraint_args

    val equal : (Field.t -> Field.t -> t) with_constraint_args

    val r1cs : (Field.t -> Field.t -> Field.t -> t) with_constraint_args

    val square : (Field.t -> Field.t -> t) with_constraint_args
  end

  (** The data specification for checked computations. *)
  and Data_spec : sig
    (** A list of {!type:Typ.t} values, describing the inputs to a checked
        computation. The type [('r_var, 'r_value, 'k_var, 'k_value) t]
        represents
        - ['k_value] is the OCaml type of the computation
        - ['r_value] is the OCaml type of the result
        - ['k_var] is the type of the computation within the R1CS
        - ['k_value] is the type of the result within the R1CS.

        This functions the same as OCaml's default list type:
{[
  Data_spec.[typ1; typ2; typ3]

  Data_spec.(typ1 :: typs)

  let open Data_spec in
  [typ1; typ2; typ3; typ4; typ5]

  let open Data_spec in
  typ1 :: typ2 :: typs

]}
        all function as you would expect.
    *)
    type ('r_var, 'r_value, 'k_var, 'k_value) t =
      ('r_var, 'r_value, 'k_var, 'k_value, field) Typ0.Data_spec.t

    val size : (_, _, _, _) t -> int
    (** [size [typ1; ...; typn]] returns the number of {!type:Var.t} variables
        allocated by allocating [typ1], followed by [typ2], etc. *)

    include module type of Typ0.Data_spec0
  end

  (** Mappings from OCaml types to R1CS variables and constraints. *)
  and Typ : sig
    module Store : sig
      include Monad.S with type 'a t = ('a, field) Typ_monads.Store.t

      val store : field -> Field.t t
    end

    module Alloc : sig
      include Monad.S with type 'a t = ('a, field) Typ_monads.Alloc.t

      val alloc : Field.t t
    end

    module Read : sig
      include Monad.S with type 'a t = ('a, field) Typ_monads.Read.t

      val read : Field.t -> field t
    end

    type ('var, 'value) t =
      ('var, 'value, field, (unit, unit, field) Checked.t) Types.Typ.t

    (** Accessors for {!type:Types.Typ.t} fields: *)

    val store : ('var, 'value) t -> 'value -> 'var Store.t

    val read : ('var, 'value) t -> 'var -> 'value Read.t

    val alloc : ('var, 'value) t -> 'var Alloc.t

    val check : ('var, 'value) t -> 'var -> unit

    (** Basic instances: *)

    val unit : (unit, unit) t

    val field : (Field.t, field) t

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

    val tuple4 :
         ('var1, 'value1) t
      -> ('var2, 'value2) t
      -> ('var3, 'value3) t
      -> ('var4, 'value4) t
      -> ( 'var1 * 'var2 * 'var3 * 'var4
         , 'value1 * 'value2 * 'value3 * 'value4 )
         t

    val tuple5 :
         ('var1, 'value1) t
      -> ('var2, 'value2) t
      -> ('var3, 'value3) t
      -> ('var4, 'value4) t
      -> ('var5, 'value5) t
      -> ( 'var1 * 'var2 * 'var3 * 'var4 * 'var5
         , 'value1 * 'value2 * 'value3 * 'value4 * 'value5 )
         t

    val tuple6 :
         ('var1, 'value1) t
      -> ('var2, 'value2) t
      -> ('var3, 'value3) t
      -> ('var4, 'value4) t
      -> ('var5, 'value5) t
      -> ('var6, 'value6) t
      -> ( 'var1 * 'var2 * 'var3 * 'var4 * 'var5 * 'var6
         , 'value1 * 'value2 * 'value3 * 'value4 * 'value5 * 'value6 )
         t

    val tuple7 :
         ('var1, 'value1) t
      -> ('var2, 'value2) t
      -> ('var3, 'value3) t
      -> ('var4, 'value4) t
      -> ('var5, 'value5) t
      -> ('var6, 'value6) t
      -> ('var7, 'value7) t
      -> ( 'var1 * 'var2 * 'var3 * 'var4 * 'var5 * 'var6 * 'var7
         , 'value1 * 'value2 * 'value3 * 'value4 * 'value5 * 'value6 * 'value7
         )
         t

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

    include module type of Types.Typ.T

    val int : (int, int) t

    val string : (string, string) t

    val char : (char, char) t

    val big_int : (Big_int.big_int, Big_int.big_int) t

    val list0 : ('a, 'b) t -> ('a list, 'b list) t

    val list1 : ('a, 'b) t -> ('a list, 'b list) t

    val list2 : ('a, 'b) t -> ('a list, 'b list) t

    val list3 : ('a, 'b) t -> ('a list, 'b list) t

    val list4 : ('a, 'b) t -> ('a list, 'b list) t

    val list5 : ('a, 'b) t -> ('a list, 'b list) t

    val list6 : ('a, 'b) t -> ('a list, 'b list) t

    val list7 : ('a, 'b) t -> ('a list, 'b list) t

    val list8 : ('a, 'b) t -> ('a list, 'b list) t

    val list9 : ('a, 'b) t -> ('a list, 'b list) t

    val list10 : ('a, 'b) t -> ('a list, 'b list) t

    val list11 : ('a, 'b) t -> ('a list, 'b list) t

    val list12 : ('a, 'b) t -> ('a list, 'b list) t

    val list13 : ('a, 'b) t -> ('a list, 'b list) t

    val list14 : ('a, 'b) t -> ('a list, 'b list) t

    val list15 : ('a, 'b) t -> ('a list, 'b list) t

    val list16 : ('a, 'b) t -> ('a list, 'b list) t

    val list17 : ('a, 'b) t -> ('a list, 'b list) t

    val list18 : ('a, 'b) t -> ('a list, 'b list) t

    val list19 : ('a, 'b) t -> ('a list, 'b list) t

    val list20 : ('a, 'b) t -> ('a list, 'b list) t

    val list21 : ('a, 'b) t -> ('a list, 'b list) t

    val list22 : ('a, 'b) t -> ('a list, 'b list) t

    val list23 : ('a, 'b) t -> ('a list, 'b list) t

    val list24 : ('a, 'b) t -> ('a list, 'b list) t

    val list25 : ('a, 'b) t -> ('a list, 'b list) t

    val list26 : ('a, 'b) t -> ('a list, 'b list) t

    val list27 : ('a, 'b) t -> ('a list, 'b list) t

    val list28 : ('a, 'b) t -> ('a list, 'b list) t

    val list29 : ('a, 'b) t -> ('a list, 'b list) t

    val list30 : ('a, 'b) t -> ('a list, 'b list) t

    val list31 : ('a, 'b) t -> ('a list, 'b list) t

    val list32 : ('a, 'b) t -> ('a list, 'b list) t

    val list33 : ('a, 'b) t -> ('a list, 'b list) t

    val list34 : ('a, 'b) t -> ('a list, 'b list) t

    val list35 : ('a, 'b) t -> ('a list, 'b list) t

    val list36 : ('a, 'b) t -> ('a list, 'b list) t

    val list37 : ('a, 'b) t -> ('a list, 'b list) t

    val list38 : ('a, 'b) t -> ('a list, 'b list) t

    val list39 : ('a, 'b) t -> ('a list, 'b list) t

    val list40 : ('a, 'b) t -> ('a list, 'b list) t

    val list41 : ('a, 'b) t -> ('a list, 'b list) t

    val list42 : ('a, 'b) t -> ('a list, 'b list) t

    val list43 : ('a, 'b) t -> ('a list, 'b list) t

    val list44 : ('a, 'b) t -> ('a list, 'b list) t

    val list45 : ('a, 'b) t -> ('a list, 'b list) t

    val list46 : ('a, 'b) t -> ('a list, 'b list) t

    val list47 : ('a, 'b) t -> ('a list, 'b list) t

    val list48 : ('a, 'b) t -> ('a list, 'b list) t

    val list49 : ('a, 'b) t -> ('a list, 'b list) t

    val list50 : ('a, 'b) t -> ('a list, 'b list) t

    val list51 : ('a, 'b) t -> ('a list, 'b list) t

    val list52 : ('a, 'b) t -> ('a list, 'b list) t

    val list53 : ('a, 'b) t -> ('a list, 'b list) t

    val list54 : ('a, 'b) t -> ('a list, 'b list) t

    val list55 : ('a, 'b) t -> ('a list, 'b list) t

    val list56 : ('a, 'b) t -> ('a list, 'b list) t

    val list57 : ('a, 'b) t -> ('a list, 'b list) t

    val list58 : ('a, 'b) t -> ('a list, 'b list) t

    val list59 : ('a, 'b) t -> ('a list, 'b list) t

    val list60 : ('a, 'b) t -> ('a list, 'b list) t

    val list61 : ('a, 'b) t -> ('a list, 'b list) t

    val list62 : ('a, 'b) t -> ('a list, 'b list) t

    val list63 : ('a, 'b) t -> ('a list, 'b list) t

    val list64 : ('a, 'b) t -> ('a list, 'b list) t

    val list65 : ('a, 'b) t -> ('a list, 'b list) t

    val list66 : ('a, 'b) t -> ('a list, 'b list) t

    val list67 : ('a, 'b) t -> ('a list, 'b list) t

    val list68 : ('a, 'b) t -> ('a list, 'b list) t

    val list69 : ('a, 'b) t -> ('a list, 'b list) t

    val list70 : ('a, 'b) t -> ('a list, 'b list) t

    val list71 : ('a, 'b) t -> ('a list, 'b list) t

    val list72 : ('a, 'b) t -> ('a list, 'b list) t

    val list73 : ('a, 'b) t -> ('a list, 'b list) t

    val list74 : ('a, 'b) t -> ('a list, 'b list) t

    val list75 : ('a, 'b) t -> ('a list, 'b list) t

    val list76 : ('a, 'b) t -> ('a list, 'b list) t

    val list77 : ('a, 'b) t -> ('a list, 'b list) t

    val list78 : ('a, 'b) t -> ('a list, 'b list) t

    val list79 : ('a, 'b) t -> ('a list, 'b list) t

    val list80 : ('a, 'b) t -> ('a list, 'b list) t

    val list81 : ('a, 'b) t -> ('a list, 'b list) t

    val list82 : ('a, 'b) t -> ('a list, 'b list) t

    val list83 : ('a, 'b) t -> ('a list, 'b list) t

    val list84 : ('a, 'b) t -> ('a list, 'b list) t

    val list85 : ('a, 'b) t -> ('a list, 'b list) t

    val list86 : ('a, 'b) t -> ('a list, 'b list) t

    val list87 : ('a, 'b) t -> ('a list, 'b list) t

    val list88 : ('a, 'b) t -> ('a list, 'b list) t

    val list89 : ('a, 'b) t -> ('a list, 'b list) t

    val list90 : ('a, 'b) t -> ('a list, 'b list) t

    val list91 : ('a, 'b) t -> ('a list, 'b list) t

    val list92 : ('a, 'b) t -> ('a list, 'b list) t

    val list93 : ('a, 'b) t -> ('a list, 'b list) t

    val list94 : ('a, 'b) t -> ('a list, 'b list) t

    val list95 : ('a, 'b) t -> ('a list, 'b list) t

    val list96 : ('a, 'b) t -> ('a list, 'b list) t

    val list97 : ('a, 'b) t -> ('a list, 'b list) t

    val list98 : ('a, 'b) t -> ('a list, 'b list) t

    val list99 : ('a, 'b) t -> ('a list, 'b list) t

    val list100 : ('a, 'b) t -> ('a list, 'b list) t

    val list101 : ('a, 'b) t -> ('a list, 'b list) t

    val list102 : ('a, 'b) t -> ('a list, 'b list) t

    val list103 : ('a, 'b) t -> ('a list, 'b list) t

    val list104 : ('a, 'b) t -> ('a list, 'b list) t

    val list105 : ('a, 'b) t -> ('a list, 'b list) t

    val list106 : ('a, 'b) t -> ('a list, 'b list) t

    val list107 : ('a, 'b) t -> ('a list, 'b list) t

    val list108 : ('a, 'b) t -> ('a list, 'b list) t

    val list109 : ('a, 'b) t -> ('a list, 'b list) t

    val list110 : ('a, 'b) t -> ('a list, 'b list) t

    val list111 : ('a, 'b) t -> ('a list, 'b list) t

    val list112 : ('a, 'b) t -> ('a list, 'b list) t

    val list113 : ('a, 'b) t -> ('a list, 'b list) t

    val list114 : ('a, 'b) t -> ('a list, 'b list) t

    val list115 : ('a, 'b) t -> ('a list, 'b list) t

    val list116 : ('a, 'b) t -> ('a list, 'b list) t

    val list117 : ('a, 'b) t -> ('a list, 'b list) t

    val list118 : ('a, 'b) t -> ('a list, 'b list) t

    val list119 : ('a, 'b) t -> ('a list, 'b list) t

    val list120 : ('a, 'b) t -> ('a list, 'b list) t

    val list121 : ('a, 'b) t -> ('a list, 'b list) t

    val list122 : ('a, 'b) t -> ('a list, 'b list) t

    val list123 : ('a, 'b) t -> ('a list, 'b list) t

    val list124 : ('a, 'b) t -> ('a list, 'b list) t

    val list125 : ('a, 'b) t -> ('a list, 'b list) t

    val list126 : ('a, 'b) t -> ('a list, 'b list) t

    val list127 : ('a, 'b) t -> ('a list, 'b list) t

    val list128 : ('a, 'b) t -> ('a list, 'b list) t

    val list129 : ('a, 'b) t -> ('a list, 'b list) t

    val list130 : ('a, 'b) t -> ('a list, 'b list) t

    val list131 : ('a, 'b) t -> ('a list, 'b list) t

    val list132 : ('a, 'b) t -> ('a list, 'b list) t

    val list133 : ('a, 'b) t -> ('a list, 'b list) t

    val list134 : ('a, 'b) t -> ('a list, 'b list) t

    val list135 : ('a, 'b) t -> ('a list, 'b list) t

    val list136 : ('a, 'b) t -> ('a list, 'b list) t

    val list137 : ('a, 'b) t -> ('a list, 'b list) t

    val list138 : ('a, 'b) t -> ('a list, 'b list) t

    val list139 : ('a, 'b) t -> ('a list, 'b list) t

    val list140 : ('a, 'b) t -> ('a list, 'b list) t

    val list141 : ('a, 'b) t -> ('a list, 'b list) t

    val list142 : ('a, 'b) t -> ('a list, 'b list) t

    val list143 : ('a, 'b) t -> ('a list, 'b list) t

    val list144 : ('a, 'b) t -> ('a list, 'b list) t

    val list145 : ('a, 'b) t -> ('a list, 'b list) t

    val list146 : ('a, 'b) t -> ('a list, 'b list) t

    val list147 : ('a, 'b) t -> ('a list, 'b list) t

    val list148 : ('a, 'b) t -> ('a list, 'b list) t

    val list149 : ('a, 'b) t -> ('a list, 'b list) t

    val list150 : ('a, 'b) t -> ('a list, 'b list) t

    val list151 : ('a, 'b) t -> ('a list, 'b list) t

    val list152 : ('a, 'b) t -> ('a list, 'b list) t

    val list153 : ('a, 'b) t -> ('a list, 'b list) t

    val list154 : ('a, 'b) t -> ('a list, 'b list) t

    val list155 : ('a, 'b) t -> ('a list, 'b list) t

    val list156 : ('a, 'b) t -> ('a list, 'b list) t

    val list157 : ('a, 'b) t -> ('a list, 'b list) t

    val list158 : ('a, 'b) t -> ('a list, 'b list) t

    val list159 : ('a, 'b) t -> ('a list, 'b list) t

    val list160 : ('a, 'b) t -> ('a list, 'b list) t

    val list161 : ('a, 'b) t -> ('a list, 'b list) t

    val list162 : ('a, 'b) t -> ('a list, 'b list) t

    val list163 : ('a, 'b) t -> ('a list, 'b list) t

    val list164 : ('a, 'b) t -> ('a list, 'b list) t

    val list165 : ('a, 'b) t -> ('a list, 'b list) t

    val list166 : ('a, 'b) t -> ('a list, 'b list) t

    val list167 : ('a, 'b) t -> ('a list, 'b list) t

    val list168 : ('a, 'b) t -> ('a list, 'b list) t

    val list169 : ('a, 'b) t -> ('a list, 'b list) t

    val list170 : ('a, 'b) t -> ('a list, 'b list) t

    val list171 : ('a, 'b) t -> ('a list, 'b list) t

    val list172 : ('a, 'b) t -> ('a list, 'b list) t

    val list173 : ('a, 'b) t -> ('a list, 'b list) t

    val list174 : ('a, 'b) t -> ('a list, 'b list) t

    val list175 : ('a, 'b) t -> ('a list, 'b list) t

    val list176 : ('a, 'b) t -> ('a list, 'b list) t

    val list177 : ('a, 'b) t -> ('a list, 'b list) t

    val list178 : ('a, 'b) t -> ('a list, 'b list) t

    val list179 : ('a, 'b) t -> ('a list, 'b list) t

    val list180 : ('a, 'b) t -> ('a list, 'b list) t

    val list181 : ('a, 'b) t -> ('a list, 'b list) t

    val list182 : ('a, 'b) t -> ('a list, 'b list) t

    val list183 : ('a, 'b) t -> ('a list, 'b list) t

    val list184 : ('a, 'b) t -> ('a list, 'b list) t

    val list185 : ('a, 'b) t -> ('a list, 'b list) t

    val list186 : ('a, 'b) t -> ('a list, 'b list) t

    val list187 : ('a, 'b) t -> ('a list, 'b list) t

    val list188 : ('a, 'b) t -> ('a list, 'b list) t

    val list189 : ('a, 'b) t -> ('a list, 'b list) t

    val list190 : ('a, 'b) t -> ('a list, 'b list) t

    val list191 : ('a, 'b) t -> ('a list, 'b list) t

    val list192 : ('a, 'b) t -> ('a list, 'b list) t

    val list193 : ('a, 'b) t -> ('a list, 'b list) t

    val list194 : ('a, 'b) t -> ('a list, 'b list) t

    val list195 : ('a, 'b) t -> ('a list, 'b list) t

    val list196 : ('a, 'b) t -> ('a list, 'b list) t

    val list197 : ('a, 'b) t -> ('a list, 'b list) t

    val list198 : ('a, 'b) t -> ('a list, 'b list) t

    val list199 : ('a, 'b) t -> ('a list, 'b list) t

    val list200 : ('a, 'b) t -> ('a list, 'b list) t

    val list201 : ('a, 'b) t -> ('a list, 'b list) t

    val list202 : ('a, 'b) t -> ('a list, 'b list) t

    val list203 : ('a, 'b) t -> ('a list, 'b list) t

    val list204 : ('a, 'b) t -> ('a list, 'b list) t

    val list205 : ('a, 'b) t -> ('a list, 'b list) t

    val list206 : ('a, 'b) t -> ('a list, 'b list) t

    val list207 : ('a, 'b) t -> ('a list, 'b list) t

    val list208 : ('a, 'b) t -> ('a list, 'b list) t

    val list209 : ('a, 'b) t -> ('a list, 'b list) t

    val list210 : ('a, 'b) t -> ('a list, 'b list) t

    val list211 : ('a, 'b) t -> ('a list, 'b list) t

    val list212 : ('a, 'b) t -> ('a list, 'b list) t

    val list213 : ('a, 'b) t -> ('a list, 'b list) t

    val list214 : ('a, 'b) t -> ('a list, 'b list) t

    val list215 : ('a, 'b) t -> ('a list, 'b list) t

    val list216 : ('a, 'b) t -> ('a list, 'b list) t

    val list217 : ('a, 'b) t -> ('a list, 'b list) t

    val list218 : ('a, 'b) t -> ('a list, 'b list) t

    val list219 : ('a, 'b) t -> ('a list, 'b list) t

    val list220 : ('a, 'b) t -> ('a list, 'b list) t

    val list221 : ('a, 'b) t -> ('a list, 'b list) t

    val list222 : ('a, 'b) t -> ('a list, 'b list) t

    val list223 : ('a, 'b) t -> ('a list, 'b list) t

    val list224 : ('a, 'b) t -> ('a list, 'b list) t

    val list225 : ('a, 'b) t -> ('a list, 'b list) t

    val list226 : ('a, 'b) t -> ('a list, 'b list) t

    val list227 : ('a, 'b) t -> ('a list, 'b list) t

    val list228 : ('a, 'b) t -> ('a list, 'b list) t

    val list229 : ('a, 'b) t -> ('a list, 'b list) t

    val list230 : ('a, 'b) t -> ('a list, 'b list) t

    val list231 : ('a, 'b) t -> ('a list, 'b list) t

    val list232 : ('a, 'b) t -> ('a list, 'b list) t

    val list233 : ('a, 'b) t -> ('a list, 'b list) t

    val list234 : ('a, 'b) t -> ('a list, 'b list) t

    val list235 : ('a, 'b) t -> ('a list, 'b list) t

    val list236 : ('a, 'b) t -> ('a list, 'b list) t

    val list237 : ('a, 'b) t -> ('a list, 'b list) t

    val list238 : ('a, 'b) t -> ('a list, 'b list) t

    val list239 : ('a, 'b) t -> ('a list, 'b list) t

    val list240 : ('a, 'b) t -> ('a list, 'b list) t

    val list241 : ('a, 'b) t -> ('a list, 'b list) t

    val list242 : ('a, 'b) t -> ('a list, 'b list) t

    val list243 : ('a, 'b) t -> ('a list, 'b list) t

    val list244 : ('a, 'b) t -> ('a list, 'b list) t

    val list245 : ('a, 'b) t -> ('a list, 'b list) t

    val list246 : ('a, 'b) t -> ('a list, 'b list) t

    val list247 : ('a, 'b) t -> ('a list, 'b list) t

    val list248 : ('a, 'b) t -> ('a list, 'b list) t

    val list249 : ('a, 'b) t -> ('a list, 'b list) t

    val list250 : ('a, 'b) t -> ('a list, 'b list) t

    val list251 : ('a, 'b) t -> ('a list, 'b list) t

    val list252 : ('a, 'b) t -> ('a list, 'b list) t

    val list253 : ('a, 'b) t -> ('a list, 'b list) t

    val list254 : ('a, 'b) t -> ('a list, 'b list) t

    val list255 : ('a, 'b) t -> ('a list, 'b list) t

    val list256 : ('a, 'b) t -> ('a list, 'b list) t
  end

  (** Representation of booleans within a field.

      This representation ties the value of [true] to {!val:Field.one} and
      [false] to {!val:Field.zero}, adding a check in {!val:Boolean.typ} to
      ensure that these are the only vales. *)
  and Boolean : sig
    type var = Field.t Boolean0.t

    type value = bool

    val to_field : var -> Field.t

    val true_ : var

    val false_ : var

    val if_ : var -> then_:var -> else_:var -> var

    val not : var -> var

    val ( && ) : var -> var -> var

    val ( || ) : var -> var -> var

    val ( lxor ) : var -> var -> var

    val any : var list -> var

    val all : var list -> var

    val of_field : Field.t -> var
    (** Convert a value in a field to a boolean, adding checks to the R1CS that
       it is a valid boolean value. *)

    val var_of_value : value -> var

    val typ : (var, value) Typ.t
    (** The relationship between {!val:var} and {!val:value}, with a check that
        the value is valid (ie. {!val:Field.zero} or {!val:Field.one}). *)

    val typ_unchecked : (var, value) Typ.t
    (** {!val:typ} without a validity check for the underlying field value. *)

    val equal : var -> var -> var

    module Expr : sig
      (** Expression trees. *)
      type t

      val ( ! ) : var -> t

      val ( && ) : t -> t -> t

      val ( || ) : t -> t -> t

      val any : t list -> t

      val all : t list -> t

      val not : t -> t

      val eval : t -> var
      (** Evaluate the expression tree. *)

      val assert_ : t -> unit
    end

    module Unsafe : sig
      val of_cvar : Field.t -> var
    end

    module Assert : sig
      val ( = ) : Boolean.var -> Boolean.var -> unit

      val equal : Boolean.var -> Boolean.var -> unit

      val is_true : Boolean.var -> unit

      val any : var list -> unit

      val all : var list -> unit

      val exactly_one : var list -> unit
    end
  end

  and Field : sig
    module Constant : sig
      (** The finite field over which the R1CS operates. *)
      type t = field [@@deriving bin_io, sexp, hash, compare]

      val gen : t Core_kernel.Quickcheck.Generator.t
      (** A generator for Quickcheck tests. *)

      include Field_intf.Extended with type t := t

      include Stringable.S with type t := t

      val unpack : t -> bool list

      val to_bits : t -> bool list
      (** Convert a field element into its constituent bits. *)

      val project : bool list -> t

      val of_bits : bool list -> t
      (** Convert a list of bits into a field element. *)
    end

    type t = field Cvar.t

    val size_in_bits : int

    val size : Bignum_bigint.t

    val length : t -> int
    (** For debug purposes *)

    val var_indices : t -> int list

    val to_constant_and_terms : t -> field option * (field * Var.t) list
    (** Convert a {!type:t} value to its constituent constant and a list of
          scaled R1CS variables. *)

    val constant : field -> t

    val of_string : string -> t

    val to_constant : t -> field option

    val linear_combination : (field * t) list -> t

    val sum : t list -> t

    val add : t -> t -> t

    val sub : t -> t -> t

    val scale : t -> field -> t

    val project : Boolean.var list -> t

    val pack : Boolean.var list -> t

    val of_int : int -> t

    val one : t

    val zero : t

    val mul : t -> t -> t

    val square : t -> t

    val div : t -> t -> t

    val inv : t -> t

    val equal : t -> t -> Boolean.var

    val unpack : t -> length:int -> Boolean.var list

    val unpack_flagged :
      t -> length:int -> Boolean.var list * [`Success of Boolean.var]

    val unpack_full : t -> Boolean.var Bitstring_lib.Bitstring.Lsb_first.t

    val choose_preimage_var : t -> length:int -> Boolean.var list

    val to_bits : ?length:int -> t -> Boolean.var list

    val of_bits : Boolean.var list -> t

    type comparison_result = {less: Boolean.var; less_or_equal: Boolean.var}

    val compare : bit_length:int -> t -> t -> comparison_result

    val if_ : Boolean.var -> then_:t -> else_:t -> t

    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val ( * ) : t -> t -> t

    val ( / ) : t -> t -> t

    module Unsafe : sig
      val of_index : int -> t
    end

    module Assert : sig
      val lte : bit_length:int -> t -> t -> unit

      val gte : bit_length:int -> t -> t -> unit

      val lt : bit_length:int -> t -> t -> unit

      val gt : bit_length:int -> t -> t -> unit

      val not_equal : t -> t -> unit

      val equal : t -> t -> unit

      val non_zero : t -> unit
    end

    val typ : (t, Constant.t) Typ.t
  end

  val load_pedersen_params :
    string -> (Field.t * Field.t) Tuple_lib.Quadruple.t array

  type boolean = Boolean.var

  module Select : sig
    type 'a t = boolean -> then_:'a -> else_:'a -> 'a

    val field : Field.t t

    val boolean : boolean t

    val tuple2 : 'a1 t -> 'a2 t -> ('a1 * 'a2) t

    val list : 'a t -> 'a list t

    val array : 'a t -> 'a array t

    val id : 'a t -> 'a t
  end

  module Proof : sig
    type t

    type message

    include Binable.S with type t := t
  end

  module Bitstring_checked : sig
    type t = Boolean.var list

    val equal : t -> t -> Boolean.var

    val lt_value :
         Boolean.var Bitstring_lib.Bitstring.Msb_first.t
      -> bool Bitstring_lib.Bitstring.Msb_first.t
      -> Boolean.var

    module Assert : sig
      val equal : t -> t -> unit
    end
  end

  (** The functions in this module may only be run as the prover; trying to
      run them outside of functions that refer to [As_prover.t] will result in
      a runtime error. *)
  module As_prover : sig
    (** This type marks function arguments that can include function calls from
        this module. Using these functions outside of these will result in a
        runtime error. *)
    type 'a t = 'a

    val in_prover_block : unit -> bool

    val read_var : Field.t -> Field.Constant.t

    val get_state : unit -> prover_state

    val set_state : prover_state -> unit

    val read : ('var, 'value) Typ.t -> 'var -> 'value

    val modify_state : (prover_state -> prover_state) -> unit

    include Field_intf.Extended with type t := field

    val unpack : field -> bool list
    (** Convert a field element into its constituent bits. *)

    val project : bool list -> field

    val with_lens :
      (prover_state, 'lens) Lens.t -> ('a, field, 'lens) As_prover.t -> 'a t
    (** Lift the monadic {!type:As_prover.t} defined with state ['lens] to an
        as-prover computation using [prover_state].
    *)
  end

  module Handle : sig
    type ('var, 'value) t

    val value : (_, 'value) t -> (unit -> 'value) As_prover.t

    val var : ('var, _) t -> 'var
  end

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

  module Proof_system : sig
    type ('a, 'public_input) t

    val create :
         ?proving_key:Proving_key.t
      -> ?verification_key:Verification_key.t
      -> ?proving_key_path:string
      -> ?verification_key_path:string
      -> ?handlers:Handler.t list
      -> public_input:( unit -> 'a
                      , unit
                      , 'computation
                      , 'public_input )
                      Data_spec.t
      -> 'computation
      -> ('a, 'public_input) t

    val constraint_system : ('a, 'public_input) t -> R1CS_constraint_system.t

    val digest : ('a, 'public_input) t -> Md5_lib.t

    val generate_keypair : ('a, 'public_input) t -> Keypair.t

    val run_unchecked :
         public_input:(unit, 'public_input) H_list.t
      -> ?handlers:Handler.t list
      -> ('a, 'public_input) t
      -> prover_state
      -> prover_state * 'a

    val run_checked :
         public_input:(unit, 'public_input) H_list.t
      -> ?handlers:Handler.t list
      -> ('a, 'public_input) t
      -> prover_state
      -> (prover_state * 'a) Or_error.t

    val check :
         public_input:(unit, 'public_input) H_list.t
      -> ?handlers:Handler.t list
      -> ('a, 'public_input) t
      -> prover_state
      -> unit Or_error.t

    val prove :
         public_input:(unit, 'public_input) H_list.t
      -> ?proving_key:Proving_key.t
      -> ?handlers:Handler.t list
      -> ?message:Proof.message
      -> ('a, 'public_input) t
      -> prover_state
      -> Proof.t

    val verify :
         public_input:(unit, 'public_input) H_list.t
      -> ?verification_key:Verification_key.t
      -> ?message:Proof.message
      -> ('a, 'public_input) t
      -> Proof.t
      -> bool
  end

  val assert_ : ?label:string -> Constraint.t -> unit

  val assert_all : ?label:string -> Constraint.t list -> unit

  val assert_r1cs : ?label:string -> Field.t -> Field.t -> Field.t -> unit

  val assert_r1 : Field.t -> Field.t -> Field.t -> unit

  val assert_square : ?label:string -> Field.t -> Field.t -> unit

  val as_prover : (unit -> unit) As_prover.t -> unit

  val next_auxiliary : unit -> int

  val request_witness :
    ('var, 'value) Typ.t -> (unit -> 'value Request.t) As_prover.t -> 'var

  val perform : (unit -> unit Request.t) As_prover.t -> unit

  val request :
       ?such_that:('var -> unit)
    -> ('var, 'value) Typ.t
    -> 'value Request.t
    -> 'var
  (** TODO: Come up with a better name for this in relation to the above *)

  val exists :
       ?request:(unit -> 'value Request.t) As_prover.t
    -> ?compute:(unit -> 'value) As_prover.t
    -> ('var, 'value) Typ.t
    -> 'var

  val exists_handle :
       ?request:(unit -> 'value Request.t) As_prover.t
    -> ?compute:(unit -> 'value) As_prover.t
    -> ('var, 'value) Typ.t
    -> ('var, 'value) Handle.t

  val handle : (unit -> 'a) -> Handler.t -> 'a

  val handle_as_prover : (unit -> 'a) -> (unit -> Handler.t As_prover.t) -> 'a

  val with_label : string -> (unit -> 'a) -> 'a

  val make_checked : (unit -> 'a) -> ('a, prover_state, field) Types.Checked.t

  val constraint_system :
       exposing:(unit -> 'a, _, 'k_var, _) Data_spec.t
    -> 'k_var
    -> R1CS_constraint_system.t

  val generate_keypair :
    exposing:(unit -> 'a, _, 'k_var, _) Data_spec.t -> 'k_var -> Keypair.t

  val prove :
       ?message:Proof.message
    -> Proving_key.t
    -> (unit -> 'a, Proof.t, 'k_var, 'k_value) Data_spec.t
    -> 'k_var
    -> prover_state
    -> 'k_value

  val verify :
       ?message:Proof.message
    -> Proof.t
    -> Verification_key.t
    -> (_, bool, _, 'k_value) Data_spec.t
    -> 'k_value

  val run_unchecked : (unit -> 'a) -> prover_state -> prover_state * 'a

  val run_and_check :
       (unit -> (unit -> 'a) As_prover.t)
    -> prover_state
    -> (prover_state * 'a) Or_error.t

  val check : (unit -> 'a) -> prover_state -> unit Or_error.t

  val constraint_count :
    ?log:(?start:bool -> string -> int -> unit) -> (unit -> 'a) -> int

  val set_constraint_logger : (Constraint.t -> unit) -> unit

  val clear_constraint_logger : unit -> unit

  module Internal_Basic : Basic with type field = field

  val run_checked : ('a, prover_state) Internal_Basic.Checked.t -> 'a
end

module type Run = sig
  include Run_basic

  module Number :
    Number_intf.Run
    with type field := field
     and type field_var := Field.t
     and type bool_var := Boolean.var

  module Enumerable (M : sig
    type t [@@deriving enum]
  end) :
    Enumerable_intf.Run
    with type ('a, 'b) typ := ('a, 'b) Typ.t
     and type bool_var := Boolean.var
     and type var = Field.t
     and type t := M.t
end
