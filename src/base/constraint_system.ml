type 'f t =
  | T :
      (module Backend_intf.Constraint_system_intf
         with type Field.t = 'f
          and type t = 't)
      * 't
      -> 'f t
