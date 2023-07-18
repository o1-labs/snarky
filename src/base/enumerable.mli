(** Functors for the monadic interface *)
module Make
    (Impl : Snark_intf.Basic) (M : sig
      type t [@@deriving enum]
    end) :
  Enumerable_intf.S
    with type 'a checked := 'a Impl.Checked.t
     and type ('a, 'b) typ := ('a, 'b) Impl.Typ.t
     and type bool_var := Impl.Boolean.var
     and type var = Impl.Field.Var.t
     and type t := M.t

(** Functors for the imperative interface *)
module Run : sig
  module Make
      (Impl : Snark_intf.Run_basic) (M : sig
        type t [@@deriving enum]
      end) :
    Enumerable_intf.Run
      with type ('a, 'b) typ := ('a, 'b) Impl.Typ.t
       and type bool_var := Impl.Boolean.var
       and type var = Impl.Field.t
       and type t := M.t
end
