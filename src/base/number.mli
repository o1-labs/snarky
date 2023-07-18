(** Functors to make the monadic interface of Number *)
module Make (Impl : Snark_intf.Basic) :
  Number_intf.S
    with type 'a checked := 'a Impl.Checked.t
     and type field := Impl.Field.t
     and type field_var := Impl.Field.Var.t
     and type bool_var := Impl.Boolean.var

(** Functors to make the imperative interface of Number *)
module Run : sig
  module Make (Impl : Snark_intf.Run_basic) :
    Number_intf.Run
      with type field := Impl.Field.Constant.t
       and type field_var := Impl.Field.t
       and type bool_var := Impl.Boolean.var
end
