module type S = sig
  type (_, _) checked

  type field

  type field_var

  type bool_var

  type t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : t -> t -> (t, _) checked

  val constant : field -> t

  val one : t

  val zero : t

  val if_ : bool_var -> then_:t -> else_:t -> (t, _) checked

  val ( < ) : t -> t -> (bool_var, _) checked

  val ( > ) : t -> t -> (bool_var, _) checked

  val ( <= ) : t -> t -> (bool_var, _) checked

  val ( >= ) : t -> t -> (bool_var, _) checked

  val ( = ) : t -> t -> (bool_var, _) checked

  val min : t -> t -> (t, _) checked

  val max : t -> t -> (t, _) checked

  val to_var : t -> field_var

  val of_bits : bool_var list -> t

  val to_bits : t -> (bool_var list, _) checked

  val div_pow_2 : t -> [ `Two_to_the of int ] -> (t, _) checked

  val ceil_div_pow_2 : t -> [ `Two_to_the of int ] -> (t, _) checked

  val mul_pow_2 : t -> [ `Two_to_the of int ] -> (t, _) checked

  val mod_pow_2 : t -> [ `Two_to_the of int ] -> (t, _) checked

  val of_pow_2 : [ `Two_to_the of int ] -> t

  val clamp_to_n_bits : t -> int -> (t, _) checked
end

module type Run = sig
  type field

  type field_var

  type bool_var

  type t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : t -> t -> t

  val constant : field -> t

  val one : t

  val zero : t

  val if_ : bool_var -> then_:t -> else_:t -> t

  val ( < ) : t -> t -> bool_var

  val ( > ) : t -> t -> bool_var

  val ( <= ) : t -> t -> bool_var

  val ( >= ) : t -> t -> bool_var

  val ( = ) : t -> t -> bool_var

  val min : t -> t -> t

  val max : t -> t -> t

  val to_var : t -> field_var

  val of_bits : bool_var list -> t

  val to_bits : t -> bool_var list

  val div_pow_2 : t -> [ `Two_to_the of int ] -> t

  val ceil_div_pow_2 : t -> [ `Two_to_the of int ] -> t

  val mul_pow_2 : t -> [ `Two_to_the of int ] -> t

  val mod_pow_2 : t -> [ `Two_to_the of int ] -> t

  val of_pow_2 : [ `Two_to_the of int ] -> t

  val clamp_to_n_bits : t -> int -> t
end
