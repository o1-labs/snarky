(* Follow the JaneStreet coding guidelines. A module M will include M0 to use
   the first version of the module M. If a new version of the module M is
   defined, the module name will be M1, and it will be included in the module M
*)
include Snark0
module Backend_intf = Backend_intf
module Merkle_tree = Merkle_tree
