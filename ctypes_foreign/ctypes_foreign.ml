(** An implementation of [Ctypes.FOREIGN] using the usual libffi bindings. *)

open Ctypes
open Foreign

type 'a fn = 'a Ctypes.fn

type 'a return = 'a

let ( @-> ) = ( @-> )

let returning = returning

type 'a result = 'a

let foreign name typ = foreign name typ

let foreign_value name typ = foreign_value name typ
