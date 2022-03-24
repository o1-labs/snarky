module type S = sig
  type _ checked

  type (_, _) typ

  type bool_var

  type t

  val bit_length : int

  type var

  val typ : (var, t) typ

  val to_bits : t -> bool list

  val var : t -> var

  val assert_equal : var -> var -> unit checked

  val var_to_bits : var -> bool_var list checked

  val if_ : bool_var -> then_:var -> else_:var -> var checked

  val ( = ) : var -> var -> bool_var checked
end

module type Run = sig
  type (_, _) typ

  type bool_var

  type t

  val bit_length : int

  type var

  val typ : (var, t) typ

  val to_bits : t -> bool list

  val var : t -> var

  val assert_equal : var -> var -> unit

  val var_to_bits : var -> bool_var list

  val if_ : bool_var -> then_:var -> else_:var -> var

  val ( = ) : var -> var -> bool_var
end
