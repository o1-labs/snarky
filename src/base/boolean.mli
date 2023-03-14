type 'v t = private 'v

val to_field_var : 'v t -> 'v

module Unsafe : sig
  val create : 'v -> 'v t
end
