open Core;
open Backend;
open Fold_lib;

/*
  A merkle tree of commitments.
 */

let merge_init = Fold.(to_list(string_triples("merg")));

let merge = (l, r) => {
  Pedersen.digest(
    boolean_triples(merge_init)
    @ Pedersen.Digest.to_triples(l)
    @ Pedersen.Digest.to_triples(r),
  );
};

let leaf_init = Fold.(to_list(string_triples("leaf")));

let hash_leaf = (x: Commitment.t) => {
  Pedersen.digest(boolean_triples(leaf_init) @ Commitment.to_triples(x));
};

let depth = 4;

module Index = {
  type t = list(Boolean.var);
  let typ = Typ.list(~length=depth, Boolean.typ);

  module Constant = {
    type t = list(bool);
  };
};

type t = Pedersen.Digest.t;

let typ = Pedersen.Digest.typ;

module Authentication_path = {
  type t = (Commitment.t, list(Pedersen.Digest.t));

  module Constant = {
    type t = (Commitment.Constant.t, list(Pedersen.Digest.Constant.t));
  };

  let typ =
    Typ.(Commitment.typ * Typ.list(~length=depth, Pedersen.Digest.typ));
};

type Snarky.Request.t(_) +=
  | Merkle_path(Index.Constant.t): Snarky.Request.t(
                                      Authentication_path.Constant.t,
                                    );

let lookup = (root: t, index: Index.t): Commitment.t => {
  /* Hint:
     define check_merkle_path(index : Index.t, c : Commitment.t
     */
  ignore(root);
  ignore(index);

  failwith("TODO");
};

module Constant = {
  type hash = Pedersen.Digest.Constant.t;

  type t =
    | Leaf(hash, Commitment.Constant.t)
    | Node(hash, t, t);

  let hash =
    fun
    | Leaf(h, _) => h
    | Node(h, _, _) => h;

  let get_path = {
    let rec get_path = (acc, t, index) => {
      switch (t, index) {
      | (Leaf(_, c), []) => (c, acc)
      | (Node(_, l, r), [b, ...bs]) =>
        let (sub_tree, other) =
          if (b) {
            (r, l);
          } else {
            (l, r);
          };
        get_path([hash(other), ...acc], sub_tree, bs);
      | _ => failwith("Bad index")
      };
    };

    get_path([]);
  };

  let get_path = (t, index) => {
    let (c, path_rev) = get_path(t, index);
    (c, List.rev(path_rev));
  };

  let merge = (l, r) => {
    Pedersen.Constant.digest(
      merge_init
      @ Pedersen.Digest.Constant.to_triples(l)
      @ Pedersen.Digest.Constant.to_triples(r),
    );
  };

  let hash_leaf = (x: Commitment.Constant.t) => {
    Pedersen.Constant.digest(leaf_init @ Commitment.Constant.to_triples(x));
  };

  let singleton = c => Leaf(hash_leaf(c), c);

  let of_commitments = cs => {
    let n = List.length(cs);
    let m = 1 lsl depth;
    let cs = cs @ List.init(m - n, ~f=_ => Commitment.Constant.empty);
    let rec pair_up = ts => {
      switch (ts) {
      | [] => []
      | [_] => failwith("Not power of two")
      | [l, r, ...ts] => [
          Node(merge(hash(l), hash(r)), l, r),
          ...pair_up(ts),
        ]
      };
    };
    let rec go = ts => {
      switch (ts) {
      | [t] => t
      | _ => go(pair_up(ts))
      };
    };
    go(List.map(cs, ~f=singleton));
  };

  let handler = (t, Snarky.Request.With({request, respond})) =>
    switch (request) {
    | Merkle_path(index) => respond(Provide(get_path(t, index)))
    | _ => failwith("Unhandled")
    };
};
