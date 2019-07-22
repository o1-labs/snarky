(** Parts of OCaml's compiler internals that we depend upon, modified to have
    a consistent interface across all versions.
*)

module Location = struct
  include Location
end
