let stdlib =
  {|
  module Request : {
    type req('a) = ..;

    type t('a) = req('a);

    module Response : {
      type t('a) = Provide('a) | Delegate(req('a)) | Unhandled;
    };

    type response = ..;
  };

  module Var : {
    type t;

    let (>=) : t -> t -> bool;

    let (<=) : t -> t -> bool;

    let (=) : t -> t -> bool;

    let (>) : t -> t -> bool;

    let (<) : t -> t -> bool;

    let (<>) : t -> t -> bool;

    let equal : t -> t -> bool;

    let compare : t -> t -> int;

    let min : t -> t -> t;

    let max : t -> t -> t;

    let ascending : t -> t -> int;

    let descending : t -> t -> int;

    let between : t -> low:t -> high:t -> bool;

    let clamp_exn : t -> min:t -> max:t -> t;

    let clamp : t -> min:t -> max:t -> Base__.Or_error.t(t);

    let create : int -> t;
  };

  type field;

  type field_var;

  module Constraint : {
    type t =
      | Boolean(field_var)
      | Equal(field_var, field_var)
      | Square(field_var, field_var)
      | R1CS(field_var, field_var);

    let boolean : ?label:string -> Field.t -> t;

    let equal : ?label:string -> Field.t -> Field.t -> t;

    let r1cs : ?label:string -> Field.t -> Field.t -> Field.t -> t;

    let square : ?label:string -> Field.t -> Field.t -> t;
  };

  module Typ : {
    module Store : {
      type t('a);

      let bind : t('a) -> f:('a -> t('b)) -> t('b);

      let return : 'a -> t('a);

      let map : t('a) -> f:('a -> 'b) -> t('b);

      let store : field -> t(Field.t);
    };

    module Alloc : {
      type t('a);

      let bind : t('a) -> f:('a -> t('b)) -> t('b);

      let return : 'a -> t('a);

      let map : t('a) -> f:('a -> 'b) -> t('b);

      let alloc : t(Field.t);
    };

    module Read : {
      type t('a);

      let bind : t('a) -> f:('a -> t('b)) -> t('b);

      let return : 'a -> t('a);

      let map : t('a) -> f:('a -> 'b) -> t('b);

      let read : Field.t -> t(field);
    };


    type t('var, 'value) = {
      store: 'value -> Store.t('var),
      read: 'var -> Read.t('value),
      alloc: Alloc.t('var),
      check: 'var -> unit
    };

    let store : {t('var, 'value)} -> 'value -> Store.t('var);

    let read : {t('var, 'value)} -> 'var -> Read.t('value);

    let alloc : {t('var, 'value)} -> Alloc.t('var);

    instance check : {t('var, 'value)} -> 'var -> unit;

    instance unit : t(unit, unit);

    instance field : t(Field.t, field);

    instance tuple2 :
         {t('var1, 'value1)}
      -> {t('var2, 'value2)}
      -> t(('var1, 'var2), ('value1, 'value2));

    instance (*) :
         {t('var1, 'value1)}
      -> {t('var2, 'value2)}
      -> t(('var1, 'var2), ('value1, 'value2));

    instance tuple3 :
         {t('var1, 'value1)}
      -> {t('var2, 'value2)}
      -> {t('var3, 'value3)}
      -> t(('var1, 'var2, 'var3), ('value1, 'value2, 'value3));

    let list :
      length:int -> {t('var, 'value)} -> t(list('var), list('value));
  };

  module Boolean : {
    type var = Boolean0.t(Field.t);

    type value = bool;

    let true_ : var;

    let false_ : var;

    let if_ : var -> then_:var -> else_:var -> var;

    let not : var -> var;

    let (&&) : var -> var -> var;

    let (||) : var -> var -> var;

    let lxor : var -> var -> var;

    let any : list(var) -> var;

    let all : list(var) -> var;

    let of_field : Field.t -> var;

    let var_of_value : value -> var;

    instance typ : Typ.t(var, value);

    let equal : var -> var -> var;

    module Unsafe : { let of_cvar : Field.t -> var;  };

    module Assert : {
      let (=) : Boolean.var -> Boolean.var -> unit;

      let is_true : Boolean.var -> unit;

      let any : list(var) -> unit;

      let all : list(var) -> unit;

      let exactly_one : list(var) -> unit;

    };

  };

  module Field : {
    module Constant : {
      type t = field;

      let compare : t -> t -> int;

      let of_int : int -> t;

      let one : t;

      let zero : t;

      let add : t -> t -> t;

      let sub : t -> t -> t;

      let mul : t -> t -> t;

      let inv : t -> t;

      let square : t -> t;

      let sqrt : t -> t;

      let is_square : t -> bool;

      let equal : t -> t -> bool;

      let size_in_bits : int;

      let print : t -> unit;

      let random : unit -> t;

      let negate : t -> t;

      let (+) : t -> t -> t;

      let (*) : t -> t -> t;

      let (-) : t -> t -> t;

      let (/) : t -> t -> t;

      let of_string : string -> t;

      let to_string : t -> string;

      let unpack : t -> list(bool);

      let project : list(bool) -> t;

    };

    type t;

    let size_in_bits : int;

    let length : t -> int;

    let constant : field -> t;

    let to_constant : t -> option(field);

    let linear_combination : list((field, t)) -> t;

    let sum : list(t) -> t;

    let add : t -> t -> t;

    let sub : t -> t -> t;

    let scale : t -> field -> t;

    let project : list(Boolean.var) -> t;

    let pack : list(Boolean.var) -> t;

    let of_int : int -> t;

    let one : t;

    let zero : t;

    let mul : t -> t -> t;

    let square : t -> t;

    let div : t -> t -> t;

    let inv : t -> t;

    let equal : t -> t -> Boolean.var;

    let unpack : t -> length:int -> list(Boolean.var);

    let unpack_full : t -> list(Boolean.var);

    let choose_preimage_var : t -> length:int -> list(Boolean.var);

    type comparison_result =
      {less: Boolean.var, less_or_equal: Boolean.var};

    let compare : bit_length:int -> t -> t -> comparison_result;

    let if_ : Boolean.var -> then_:t -> else_:t -> t;

    let (+) : t -> t -> t;

    let (-) : t -> t -> t;

    let (*) : t -> t -> t;

    let (/) : t -> t -> t;

    module Unsafe : { let of_index : int -> t;  };

    module Assert : {
      let lte : bit_length:int -> t -> t -> unit;

      let gte : bit_length:int -> t -> t -> unit;

      let lt : bit_length:int -> t -> t -> unit;

      let gt : bit_length:int -> t -> t -> unit;

      let not_equal : t -> t -> unit;

      let equal : t -> t -> unit;

      let non_zero : t -> unit;

    };

    instance typ : Typ.t(t, Constant.t);

  };

  module Bitstring_checked : {
    type t = list(Boolean.var);

    let equal : t -> t -> Boolean.var;

    let lt_value : list(Boolean.var) -> list(bool) -> Boolean.var;

    module Assert : { let equal : t -> t -> unit;  };

  };

  module As_prover : {
    type t('a) = 'a;

    let in_prover_block : unit -> bool;

    let read_var : Field.t -> Field.Constant.t;

    let read : {Typ.t('var, 'value)} -> 'var -> 'value;

    let of_int : int -> field;

    let one : field;

    let zero : field;

    let add : field -> field -> field;

    let sub : field -> field -> field;

    let mul : field -> field -> field;

    let inv : field -> field;

    let square : field -> field;

    let sqrt : field -> field;

    let is_square : field -> bool;

    let equal : field -> field -> bool;

    let size_in_bits : int;

    let print : field -> unit;

    let random : unit -> field;

    let to_string : field -> string;

    let negate : field -> field;

    let (+) : field -> field -> field;

    let (*) : field -> field -> field;

    let (-) : field -> field -> field;

    let (/) : field -> field -> field;

    let unpack : field -> list(bool);

    let project : list(bool) -> field;

  };

  module Handle : {
    type t('var, 'value);

    let value : t(_, 'value) -> As_prover.t(unit -> 'value);

    let var : t('var, _) -> 'var;

  };

  type response = Request.response;

  let unhandled : response;

  type request =
    | With
        { request: Request.t('a)
        , respond: Request.Response.t('a) -> response}
      : request;

  module Handler : {
    type t = request -> response;
  };

  let assert_ : ?label:string -> Constraint.t -> unit;

  let assert_all : ?label:string -> list(Constraint.t) -> unit;

  let assert_r1cs :
    ?label:string -> Field.t -> Field.t -> Field.t -> unit;

  let assert_square : ?label:string -> Field.t -> Field.t -> unit;

  let as_prover : As_prover.t(unit -> unit) -> unit;

  let next_auxiliary : unit -> int;

  let request_witness :
    {Typ.t('var, 'value)}
    -> As_prover.t(unit -> request('value))
    -> 'var;

  let perform : As_prover.t(unit -> request(unit)) -> unit;

  let request :
    ?such_that:('var -> unit)
    -> {Typ.t('var, 'value)}
    -> request('value)
    -> 'var;

  let exists :
    ?request:As_prover.t(unit -> request('value))
    -> ?compute:As_prover.t(unit -> 'value)
    -> {Typ.t('var, 'value)}
    -> 'var;

  let exists_handle :
    ?request:As_prover.t(unit -> request('value))
    -> ?compute:As_prover.t(unit -> 'value)
    -> {Typ.t('var, 'value)}
    -> Handle.t('var, 'value);

  let handle : (unit -> 'a) -> Handler.t -> 'a;

  let handle_as_prover :
    (unit -> 'a) -> (unit -> As_prover.t(Handler.t)) -> 'a;

  let with_label : string -> (unit -> 'a) -> 'a;

  module Number : {
    type t;

    let (+) : t -> t -> t;

    let (-) : t -> t -> t;

    let (*) : t -> t -> t;

    let constant : field -> t;

    let one : t;

    let zero : t;

    let if_ : Boolean.var -> then_:t -> else_:t -> t;

    let (<) : t -> t -> Boolean.var;

    let (>) : t -> t -> Boolean.var;

    let (<=) : t -> t -> Boolean.var;

    let (>=) : t -> t -> Boolean.var;

    let (=) : t -> t -> Boolean.var;

    let min : t -> t -> t;

    let max : t -> t -> t;

    let to_var : t -> Field.t;

    let of_bits : list(Boolean.var) -> t;

    let to_bits : t -> list(Boolean.var);

    let clamp_to_n_bits : t -> int -> t;

  };
|}
