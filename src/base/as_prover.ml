module type S = sig
  module Types : Types.Types

  include
    As_prover_intf.Basic
      with type ('a, 'f) t = ('a, 'f) Types.As_prover.t
       and type ('a, 'f) Provider.t = ('a, 'f) Types.Provider.t
end

module type Extended = sig
  type field

  module Types : Types.Types

  include
    S
      with module Types := Types
      with type 'f field := field
       and type ('a, 'f) t := ('a, 'f) Types.As_prover.t

  type 'a t = ('a, field) Types.As_prover.t
end

(* TODO: this functor doesn't do anything anymore no? *)
module Make
    (Checked : Checked_intf.S)
    (As_prover : As_prover_intf.Basic
                   with type ('a, 'f) t := ('a, 'f) Checked.Types.As_prover.t
                    and type 'f field := 'f Checked.field
                    and type ('a, 'f) Provider.t =
                     ('a, 'f) Checked.Types.Provider.t) :
  S with module Types = Checked.Types and type 'f field = 'f Checked.field =
struct
  module Types = Checked.Types

  type ('a, 'f) t = ('a, 'f) Types.As_prover.t

  type 'f field = 'f Checked.field

  include As_prover
end

module T : S with module Types = Checked_ast.Types and type 'f field := 'f =
  Make (Checked_ast) (As_prover0)

include T

module Make_extended (Env : sig
  type field
end)
(Checked : Checked_intf.S with type 'f field := Env.field)
(As_prover : S
               with module Types := Checked.Types
               with type 'f field := Env.field) =
struct
  module Types = Checked.Types

  type 'a t = ('a, Env.field) Types.As_prover.t

  include Env

  include (
    As_prover :
      S
        with module Types := Types
        with type 'f field := field
         and type ('a, 'f) t := ('a, 'f) Types.As_prover.t )
end
