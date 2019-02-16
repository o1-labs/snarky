module Make (Impl : Snark_intf.Basic) (M : Enumerable_intf.Enum) :
  Enumerable_intf.S
  with type ('a, 'b) checked := ('a, 'b) Impl.Checked.t
   and type ('a, 'b) typ := ('a, 'b) Impl.Typ.t
   and type bool_var := Impl.Boolean.var
   and type var = Impl.Field.Var.t
   and type t := M.t
