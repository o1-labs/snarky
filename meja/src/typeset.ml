open Core_kernel

module T = struct
  type t = Type0.type_expr [@@deriving sexp]

  let compare typ1 typ2 = Int.compare typ1.Type0.type_id typ2.Type0.type_id
end

include Comparable.Make (T)
include Set
