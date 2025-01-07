module Make (Types : sig
  type field

  type constraint_
end) :
  Run_state_intf.S
    with type field = Types.field
     and type constraint_ = Types.constraint_
