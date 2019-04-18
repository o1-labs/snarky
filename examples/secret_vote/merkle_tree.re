open Backend;
open Fold_lib

/* 
 A merkle tree of commitments.
*/

let merge_init = Fold.(to_list(string_triples("merg")));

let merge (l, r) = {
  Pedersen.digest(
    boolean_triples(merge_init)
    @ Pedersen.Digest.to_triples(l)
    @ Pedersen.Digest.to_triples(r) )
};

let leaf_init = Fold.(to_list(string_triples("leaf")));

let hash_leaf(x : Commitment.t) = {
  Pedersen.digest(
    boolean_triples(leaf_init)
    @ Commitment.to_triples(x) );
};

let depth = 5;

module Index = {
  type t = list(Boolean.var);
  let typ = Typ.list(~length=depth, Boolean.typ);

  module Constant = {
    type t = list(bool);
  };
};

type t = Pedersen.Digest.t

let typ = Pedersen.Digest.typ;

type Snarky.Request.t(_) +=
  | Merkle_path (Index.Constant.t)
  : Snarky.Request.t( (Commitment.t, list(Pedersen.Digest.t)) );

let lookup(root : t, index : Index.t) : Commitment.t = {
  /* Hint:
   define check_merkle_path(index : Index.t, c : Commitment.t
   */
  ignore(root);
  ignore(index);

  failwith("TODO")
};

module Constant = {
  type hash = Pedersen.Digest.Constant.t;

  type t = 
    | Leaf (hash, Commitment.Constant.t)
    | Node (hash, t, t);

  let hash = fun
    | Leaf (h, _) => h
    | Node (h, _, _) => h

  let rec get_path(t, index) = {
    switch (t, index) {
      | (Leaf(_, c), []) => (c, [])
      | (Node(_, l, r), [b, ...bs]) => {
        let (sub_tree, other) =
          if (b) {
            (r, l)
          } else {
            (l, r)
          };
        let (c, path) = get_path(sub_tree, bs);
        (c, [hash(other), ...path])
      }
      | _ => failwith("Bad index")
    };
  };
};
