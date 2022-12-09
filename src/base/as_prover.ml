module type S = sig
  module Types : Types.Types

  include
    As_prover_intf.Basic
      with type ('a, 'f) Provider.t = ('a, 'f) Types.Provider.t
end

module type Extended = sig
  type field

  include S with type 'f field := field

  type 'a t = ('a, field) As_prover0.t
end

(* TODO: this functor doesn't do anything anymore no? *)
module Make
    (Checked : Checked_intf.S)
    (As_prover : As_prover_intf.Basic
                   with type 'f field := 'f Checked.field
                    and type ('a, 'f) Provider.t =
                     ('a, 'f) Checked.Types.Provider.t) :
  S with module Types = Checked.Types and type 'f field = 'f Checked.field =
struct
  module Types = Checked.Types

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
  include Env

  include (As_prover : S with module Types := Types with type 'f field := field)

  type 'a t = ('a, field) As_prover0.t
end
