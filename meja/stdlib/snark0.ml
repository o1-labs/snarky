let ocaml =
  ( __LINE__ + 1
  , {|
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

    let create : int -> t;
  };

  module Constraint : {
    type t =
      | Boolean(field_var)
      | Equal(field_var, field_var)
      | Square(field_var, field_var)
      | R1CS(field_var, field_var);

    let boolean : ?label:string -> field_var -> t;

    let equal : ?label:string -> field_var -> field_var -> t;

    let r1cs : ?label:string -> field_var -> field_var -> field_var -> t;

    let square : ?label:string -> field_var -> field_var -> t;
  };

  module Typ : {
    module Store : {
      type t('a);

      let bind : t('a) -> f:('a -> t('b)) -> t('b);

      let return : 'a -> t('a);

      let map : t('a) -> f:('a -> 'b) -> t('b);

      let store : field -> t(field_var);
    };

    module Alloc : {
      type t('a);

      let bind : t('a) -> f:('a -> t('b)) -> t('b);

      let return : 'a -> t('a);

      let map : t('a) -> f:('a -> 'b) -> t('b);

      let alloc : t(field_var);
    };

    module Read : {
      type t('a);

      let bind : t('a) -> f:('a -> t('b)) -> t('b);

      let return : 'a -> t('a);

      let map : t('a) -> f:('a -> 'b) -> t('b);

      let read : field_var -> t(field);
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

    instance field : t(field_var, field);

    let tuple2 :
         {t('var1, 'value1)}
      -> {t('var2, 'value2)}
      -> t(('var1, 'var2), ('value1, 'value2));

    instance ( * ) :
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
    type var = bool_var;

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

    let of_field : field_var -> var;

    let var_of_value : value -> var;

    instance typ : Typ.t(var, value);

    let equal : var -> var -> var;

    module Unsafe : { let of_cvar : field_var -> var;  };

    module Assert : {
      let (=) : var -> var -> unit;

      let is_true : var -> unit;

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

      let ( * ) : t -> t -> t;

      let (-) : t -> t -> t;

      let (/) : t -> t -> t;

      let of_string : string -> t;

      let to_string : t -> string;

      let unpack : t -> list(bool);

      let project : list(bool) -> t;

    };

    type t = field_var;

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
    -> As_prover.t(unit -> Request.t('value))
    -> 'var;

  let perform : As_prover.t(unit -> Request.t(unit)) -> unit;

  let request :
       ?such_that:('var -> unit)
    -> {Typ.t('var, 'value)}
    -> Request.t('value)
    -> 'var;

  let exists :
       ?request:As_prover.t(unit -> Request.t('value))
    -> ?compute:As_prover.t(unit -> 'value)
    -> {Typ.t('var, 'value)}
    -> 'var;

  let exists_handle :
       ?request:As_prover.t(unit -> Request.t('value))
    -> ?compute:As_prover.t(unit -> 'value)
    -> {Typ.t('var, 'value)}
    -> Handle.t('var, 'value);

  let handle : (unit -> 'a) -> Handler.t -> 'a;

  let handle_as_prover :
    (unit -> 'a) -> As_prover.t((unit -> Handler.t)) -> 'a;

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
  )

let checked =
  ( __LINE__ + 1
  , {|
  module Constraint = {
    type t = Constraint.t;

    let boolean = Constraint.boolean;

    let equal = Constraint.equal;

    let r1cs = Constraint.r1cs;

    let square = Constraint.square;
  };

  module Boolean = {
    type t = Boolean.var;

    let true_ = Boolean.true_;

    let false_ = Boolean.false_;

    let if_ = Boolean.if_;

    let not = Boolean.not;

    let (&&) = Boolean.(&&);

    let (||) = Boolean.(||);

    let lxor = Boolean.lxor;

    let any = Boolean.any;

    let all = Boolean.all;

    let of_field = Boolean.of_field;

    let equal = Boolean.equal;

    module Assert = {
      let (=) = Boolean.Assert.(=);

      let is_true = Boolean.Assert.is_true;

      let any = Boolean.Assert.any;

      let all = Boolean.Assert.all;

      let exactly_one = Boolean.Assert.exactly_one;
    };
  };

  module Field = {
    type t = Field.t;

    let size_in_bits = Field.size_in_bits;

    let length = Field.length;

    let constant = Field.constant;

    let to_constant = Field.to_constant;

    let linear_combination = Field.linear_combination;

    let sum = Field.sum;

    let add = Field.add;

    let sub = Field.sub;

    let scale = Field.scale;

    let project = Field.project;

    let pack = Field.pack;

    let of_int = Field.of_int;

    let one = Field.one;

    let zero = Field.zero;

    let mul = Field.mul;

    let square = Field.square;

    let div = Field.div;

    let inv = Field.inv;

    let equal = Field.equal;

    let unpack = Field.unpack;

    let unpack_full = Field.unpack_full;

    let choose_preimage_var = Field.choose_preimage_var;

    type comparison_result = Field.comparison_result;

    let compare = Field.compare;

    let if_ = Field.if_;

    let (+) = Field.(+);

    let (-) = Field.(-);

    let (*) = Field.(*);

    let (/) = Field.(/);

    module Assert = {
      let lte = Field.Assert.lte;

      let gte = Field.Assert.gte;

      let lt = Field.Assert.lt;

      let gt = Field.Assert.gt;

      let not_equal = Field.Assert.not_equal;

      let equal = Field.Assert.equal;

      let non_zero = Field.Assert.non_zero;

    };
  };

  module Bitstring = {
    type t = Bitstring_checked.t;

    let equal = Bitstring_checked.equal;

    let lt_value = Bitstring_checked.lt_value;

    module Assert = {
      let equal = Bitstring_checked.Assert.equal;
    };

  };

  module Handle = {
    type t = Handle.t;

    let var = Handle.var;

  };

  module Handler = {
    type t = Handler.t;
  };

  let assert_ = assert_;

  let assert_all = assert_all;

  let assert_r1cs = assert_r1cs;

  let assert_square = assert_square;

  let as_prover = as_prover;

  let next_auxiliary = next_auxiliary;

  let request_witness = request_witness;

  let perform = perform;

  let request = request;

  let exists = exists;

  let exists_handle = exists_handle;

  let handle = handle;

  let handle_as_prover = handle_as_prover;

  let with_label = with_label;

  module Number = {
    type t = Number.t;

    let (+) = Number.(+);

    let (-) = Number.(-);

    let (*) = Number.(*);

    let constant = Number.constant;

    let one = Number.one;

    let zero = Number.zero;

    let if_ = Number.if_;

    let (<) = Number.(<);

    let (>) = Number.(>);

    let (<=) = Number.(<=);

    let (>=) = Number.(>=);

    let (=) = Number.(=);

    let min = Number.min;

    let max = Number.max;

    let to_var = Number.to_var;

    let of_bits = Number.of_bits;

    let to_bits = Number.to_bits;

    let clamp_to_n_bits = Number.clamp_to_n_bits;

  };
|}
  )

let prover =
  ( __LINE__ + 1
  , {|
  module Request = {
    type req = Request.req;

    type t = Request.req;

    module Response = {
      type t = Request.Response.t;
    };

    type response = Request.response;

    let unhandled = unhandled;

    /* Bring constructors into scope without bringing the name itself. */
    type _ += Request.Response.Provide;
  };

  module Typ = {
    module Store = {
      type t = Typ.Store.t;

      let bind = Typ.Store.bind;

      let return = Typ.Store.return;

      let map = Typ.Store.map;

      let store = Typ.Store.store;
    };

    module Alloc = {
      type t = Typ.Alloc.t;

      let bind = Typ.Alloc.bind;

      let return = Typ.Alloc.return;

      let map = Typ.Alloc.map;

      let alloc = Typ.Alloc.alloc;
    };

    module Read = {
      type t = Typ.Read.t;

      let bind = Typ.Read.bind;

      let return = Typ.Read.return;

      let map = Typ.Read.map;

      let read = Typ.Read.read;
    };


    type t = Typ.t;

    let store = Typ.store;

    let read = Typ.read;

    let alloc = Typ.alloc;

    instance check = Typ.check;

    instance unit = Typ.unit;

    instance field = Typ.field;

    let tuple2 = Typ.tuple2;

    instance ( * ) = Typ.( * );

    instance tuple3 = Typ.tuple3;

    let list = Typ.list;
  };

  module Field = {
    type t = Field.Constant.t;

    let compare = Field.Constant.compare;

    let of_int = Field.Constant.of_int;

    let one = Field.Constant.one;

    let zero = Field.Constant.zero;

    let add = Field.Constant.add;

    let sub = Field.Constant.sub;

    let mul = Field.Constant.mul;

    let inv = Field.Constant.inv;

    let square = Field.Constant.square;

    let sqrt = Field.Constant.sqrt;

    let is_square = Field.Constant.is_square;

    let equal = Field.Constant.equal;

    let size_in_bits = Field.Constant.size_in_bits;

    let print = Field.Constant.print;

    let random = Field.Constant.random;

    let negate = Field.Constant.negate;

    let (+) = Field.Constant.(+);

    let ( * ) = Field.Constant.( * );

    let (-) = Field.Constant.(-);

    let (/) = Field.Constant.(/);

    let of_string = Field.Constant.of_string;

    let to_string = Field.Constant.to_string;

    let unpack = Field.Constant.unpack;

    let project = Field.Constant.project;

    instance typ = Field.typ;

  };

  module As_prover = {
    type t = As_prover.t;

    let in_prover_block = As_prover.in_prover_block;

    let read_var = As_prover.read_var;

    let read = As_prover.read;

    let of_int = As_prover.of_int;

    let one = As_prover.one;

    let zero = As_prover.zero;

    let add = As_prover.add;

    let sub = As_prover.sub;

    let mul = As_prover.mul;

    let inv = As_prover.inv;

    let square = As_prover.square;

    let sqrt = As_prover.sqrt;

    let is_square = As_prover.is_square;

    let equal = As_prover.equal;

    let size_in_bits = As_prover.size_in_bits;

    let print = As_prover.print;

    let random = As_prover.random;

    let to_string = As_prover.to_string;

    let negate = As_prover.negate;

    let (+) = As_prover.(+);

    let (*) = As_prover.(*);

    let (-) = As_prover.(-);

    let (/) = As_prover.(/);

    let unpack = As_prover.unpack;

    let project = As_prover.project;
  };

  module Handle = {
    type t = Handle.t;

    let value = Handle.value;
  };

  type response = response;

  let unhandled = unhandled;

  type request = request;

  /* Bring constructors into scope without bringing the name itself. */
  type _ += Request.Response.Provide;

  module Handler = {
    type t = Handler.t;
  };
|}
  )
