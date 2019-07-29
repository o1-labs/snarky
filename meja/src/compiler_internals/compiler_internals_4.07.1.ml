(** Parts of OCaml's compiler internals that we depend upon, modified to have
    a consistent interface across all versions.
*)

module Location = struct
  include Location

  (* Make [error_of_printer] compatible with OCaml 4.08.0's optional location
     parameter. *)
  let error_of_printer ?(loc = none) fmt = error_of_printer loc fmt
end
