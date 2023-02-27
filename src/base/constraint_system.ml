type ('f, 'cvar) t =
  | T :
      (module Backend_intf.Constraint_system_intf
         with type Field.t = 'f
          and type cvar = 'cvar
          and type t = 't )
      * 't
      -> ('f, 'cvar) t
