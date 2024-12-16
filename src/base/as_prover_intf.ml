module type Basic = sig
  module Types : Types.Types

  type field

  type 'a t = 'a Types.As_prover.t

  include Monad_let.S with type 'a t := 'a t

  val run : 'a t -> (field Cvar.t -> field) -> 'a

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val read_var : field Cvar.t -> field t

  val read : ('var, 'value) Types.Typ.t -> 'var -> 'value t

  module Provider : sig
    type 'a t

    val run : 'a t -> (field Cvar.t -> field) -> Request.Handler.t -> 'a option
  end

  module Handle : sig
    val value : ('var, 'value) Handle.t -> 'value Types.As_prover.t
  end
end
