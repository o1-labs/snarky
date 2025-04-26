open Core_kernel

module Address = struct
  type t = int
end

module Free_hash = struct
  type 'a t = Hash_value of 'a | Hash_empty | Merge of 'a t * 'a t
  [@@deriving sexp]

  let diff t1 t2 =
    let module M = struct
      exception Done of bool list
    end in
    let rec go path t1 t2 =
      match (t1, t2) with
      | Hash_empty, Hash_empty ->
          None
      | Hash_value x, Hash_value y ->
          (* poly equality; we don't know type of x and y *)
          if Caml.( = ) x y then None else raise (M.Done path)
      | Merge (l1, r1), Merge (l2, r2) ->
          ignore (go (false :: path) l1 l2) ;
          ignore (go (true :: path) r1 r2) ;
          None
      | Hash_empty, Hash_value _
      | Hash_empty, Merge _
      | Hash_value _, Hash_empty
      | Hash_value _, Merge _
      | Merge _, Hash_empty
      | Merge _, Hash_value _ ->
          raise (M.Done path)
    in
    try go [] t1 t2 with M.Done addr -> Some addr

  let rec run t ~hash ~merge =
    match t with
    | Hash_value x ->
        hash (Some x)
    | Hash_empty ->
        hash None
    | Merge (l, r) ->
        merge (run ~hash ~merge l) (run ~hash ~merge r)
end

type ('hash, 'a) non_empty_tree =
  | Node of 'hash * ('hash, 'a) tree * ('hash, 'a) tree
  | Leaf of 'hash * 'a

and ('hash, 'a) tree = Non_empty of ('hash, 'a) non_empty_tree | Empty
[@@deriving sexp]

type ('hash, 'a) t =
  { tree : ('hash, 'a) non_empty_tree
  ; depth : int
  ; count : int
  ; hash : 'a option -> 'hash
  ; merge : 'hash -> 'hash -> 'hash
  }
[@@deriving sexp]

let check_exn { tree; hash; merge; _ } =
  let default = hash None in
  let rec check_hash = function
    | Non_empty t ->
        check_hash_non_empty t
    | Empty ->
        default
  and check_hash_non_empty = function
    | Leaf (h, x) ->
        (* poly equality; don't know the hash type *)
        assert (Caml.( = ) h (hash (Some x))) ;
        h
    | Node (h, l, r) ->
        (* poly equality *)
        assert (Caml.( = ) (merge (check_hash l) (check_hash r)) h) ;
        h
  in
  ignore (check_hash_non_empty tree)

let non_empty_hash = function Node (h, _, _) -> h | Leaf (h, _) -> h

let depth { depth; _ } = depth

let tree_hash ~default = function
  | Empty ->
      default
  | Non_empty t ->
      non_empty_hash t

let to_list : ('hash, 'a) t -> 'a list =
  let rec go acc = function
    | Empty ->
        acc
    | Non_empty (Leaf (_, x)) ->
        x :: acc
    | Non_empty (Node (_h, l, r)) ->
        let acc' = go acc r in
        go acc' l
  in
  fun t -> go [] (Non_empty t.tree)

let left_tree hash merge depth x =
  let empty_hash = hash None in
  let rec go i h acc =
    if i = depth then (h, acc)
    else
      let h' = merge h empty_hash in
      go (i + 1) h' (Node (h', Non_empty acc, Empty))
  in
  let h = hash (Some x) in
  go 0 h (Leaf (h, x))

let insert hash merge t0 mask0 address x =
  let default = hash None in
  let rec go mask t =
    if mask = 0 then
      match t with
      | Empty ->
          Leaf (hash (Some x), x)
      | Non_empty _ ->
          failwith "Tree should be empty"
    else
      let go_left = mask land address = 0 in
      let mask' = mask lsr 1 in
      match t with
      | Empty ->
          if go_left then
            let t_l' = go mask' Empty in
            Node (merge (non_empty_hash t_l') default, Non_empty t_l', Empty)
          else
            let t_r' = go mask' Empty in
            Node (merge default (non_empty_hash t_r'), Empty, Non_empty t_r')
      | Non_empty (Node (_h, t_l, t_r)) ->
          if go_left then
            let t_l' = go mask' t_l in
            Node
              ( merge (non_empty_hash t_l') (tree_hash ~default t_r)
              , Non_empty t_l'
              , t_r )
          else
            let t_r' = go mask' t_r in
            Node
              ( merge (tree_hash ~default t_l) (non_empty_hash t_r')
              , t_l
              , Non_empty t_r' )
      | Non_empty (Leaf _) ->
          failwith "Cannot insert into leaf"
  in
  go mask0 t0

let ith_bit n i = (n lsr i) land 1 = 1

let get { tree; depth; _ } addr0 =
  let rec get t i =
    match t with Empty -> None | Non_empty t -> get_non_empty t i
  and get_non_empty t i =
    match t with
    | Node (_, l, r) ->
        let go_right = ith_bit addr0 i in
        if go_right then get r (i - 1) else get l (i - 1)
    | Leaf (_, x) ->
        Some x
  in
  get_non_empty tree (depth - 1)

let get_exn t addr = Option.value_exn (get t addr)

let set_dirty default tree addr x =
  let rec go tree addr =
    match (tree, addr) with
    | Empty, go_right :: bs ->
        let t = Non_empty (go Empty bs) in
        let l, r = if go_right then (Empty, t) else (t, Empty) in
        Node (default, l, r)
    | Empty, [] ->
        Leaf (default, x)
    | Non_empty t, _ ->
        go_non_empty t addr
  and go_non_empty tree addr =
    match (tree, addr) with
    | Leaf _, [] ->
        Leaf (default, x)
    | Node (_, l, r), go_right :: bs ->
        let l', r' =
          if go_right then (l, Non_empty (go r bs)) else (Non_empty (go l bs), r)
        in
        Node (default, l', r')
    | Leaf _, _ :: _ | Node _, [] ->
        failwith "Merkle_tree.set_dirty (go_non_empty): Mismatch"
  in
  go_non_empty tree (List.rev addr)

let recompute_hashes { tree; hash; merge; _ } =
  let h =
    let default = hash None in
    fun t -> tree_hash ~default t
  in
  let rec go = function
    | Non_empty t ->
        Non_empty (go_non_empty t)
    | Empty ->
        Empty
  and go_non_empty = function
    | Leaf (_, x) ->
        Leaf (hash (Some x), x)
    | Node (_, l, r) ->
        let l' = go l in
        let r' = go r in
        Node (merge (h l') (h r'), l', r')
  in
  go_non_empty tree

let address_of_int ~depth n : bool list =
  List.init depth ~f:(fun i -> n land (1 lsl i) <> 0)

let add_many t xs =
  let default = t.hash None in
  let left_tree_dirty depth x =
    let rec go i acc =
      if i = depth then acc
      else go (i + 1) (Node (default, Non_empty acc, Empty))
    in
    go 0 (Leaf (default, x))
  in
  let add_one_dirty { tree; depth; count; hash; merge } x =
    if count = 1 lsl depth then
      let t_r = left_tree_dirty depth x in
      { tree = Node (default, Non_empty tree, Non_empty t_r)
      ; count = count + 1
      ; depth = depth + 1
      ; hash
      ; merge
      }
    else
      { tree = set_dirty default tree (address_of_int ~depth count) x
      ; count = count + 1
      ; depth
      ; hash
      ; merge
      }
  in
  let t = List.fold_left xs ~init:t ~f:add_one_dirty in
  { t with tree = recompute_hashes t }

let add { tree; depth; count; hash; merge } x =
  if count = 1 lsl depth then
    let h_r, t_r = left_tree hash merge depth x in
    let h_l = non_empty_hash tree in
    { tree = Node (merge h_l h_r, Non_empty tree, Non_empty t_r)
    ; count = count + 1
    ; depth = depth + 1
    ; hash
    ; merge
    }
  else
    { tree = insert hash merge (Non_empty tree) (1 lsl (depth - 1)) count x
    ; count = count + 1
    ; depth
    ; hash
    ; merge
    }

let root { tree; _ } = non_empty_hash tree

let create ~hash ~merge x =
  { tree = Leaf (hash (Some x), x); count = 1; depth = 0; hash; merge }

let get_path { tree; hash; depth; _ } addr0 =
  let default = hash None in
  let rec go acc t i =
    if i < 0 then acc
    else
      let go_right = ith_bit addr0 i in
      if go_right then
        match t with
        | Leaf _ ->
            failwith "get_path"
        | Node (_h, _t_l, Empty) ->
            failwith "get_path"
        | Node (_h, t_l, Non_empty t_r) ->
            go (tree_hash ~default t_l :: acc) t_r (i - 1)
      else
        match t with
        | Leaf _ ->
            failwith "get_path"
        | Node (_h, Empty, _t_r) ->
            failwith "get_path"
        | Node (_h, Non_empty t_l, t_r) ->
            go (tree_hash ~default t_r :: acc) t_l (i - 1)
  in
  go [] tree (depth - 1)

let implied_root ~merge addr0 entry_hash path0 =
  let rec go acc i path =
    match path with
    | [] ->
        acc
    | h :: hs ->
        go (if ith_bit addr0 i then merge h acc else merge acc h) (i + 1) hs
  in
  go entry_hash 0 path0

let rec free_tree_hash = function
  | Empty ->
      Free_hash.Hash_empty
  | Non_empty (Leaf (_, x)) ->
      Hash_value x
  | Non_empty (Node (_, l, r)) ->
      Merge (free_tree_hash l, free_tree_hash r)

let free_root { tree; _ } = free_tree_hash (Non_empty tree)

let get_free_path { tree; depth; _ } addr0 =
  let rec go acc t i =
    if i < 0 then acc
    else
      let go_right = ith_bit addr0 i in
      if go_right then
        match t with
        | Leaf _ ->
            failwith "get_path"
        | Node (_h, _t_l, Empty) ->
            failwith "get_path"
        | Node (_h, t_l, Non_empty t_r) ->
            go (free_tree_hash t_l :: acc) t_r (i - 1)
      else
        match t with
        | Leaf _ ->
            failwith "get_path"
        | Node (_h, Empty, _t_r) ->
            failwith "get_path"
        | Node (_h, Non_empty t_l, t_r) ->
            go (free_tree_hash t_r :: acc) t_l (i - 1)
  in
  go [] tree (depth - 1)

let implied_free_root addr0 x path0 =
  implied_root
    ~merge:(fun a b -> Free_hash.Merge (a, b))
    addr0 (Hash_value x) path0

type ('hash, 'a) merkle_tree = ('hash, 'a) t

module Checked
    (Impl : Snark_intf.Basic) (Hash : sig
      type var

      type value

      val typ : (var, value) Impl.Typ.t

      val merge : height:int -> var -> var -> var Impl.Checked.t

      val if_ : Impl.Boolean.var -> then_:var -> else_:var -> var Impl.Checked.t

      val assert_equal : var -> var -> unit Impl.Checked.t
    end) (Elt : sig
      type var

      type value

      val typ : (var, value) Impl.Typ.t

      val hash : var -> Hash.var Impl.Checked.t
    end) =
struct
  open Impl

  module Address = struct
    type var = Boolean.var list

    type value = int

    let typ ~depth : (var, value) Typ.t =
      Typ.transport
        (Typ.list ~length:depth Boolean.typ)
        ~there:(address_of_int ~depth)
        ~back:
          (List.foldi ~init:0 ~f:(fun i acc b ->
               if b then acc lor (1 lsl i) else acc ) )
  end

  module Path = struct
    type value = Hash.value list

    type var = Hash.var list

    let typ ~depth : (var, value) Typ.t = Typ.(list ~length:depth Hash.typ)
  end

  let implied_root entry_hash addr0 path0 =
    let rec go height acc addr path =
      let open Let_syntax in
      match (addr, path) with
      | [], [] ->
          return acc
      | b :: bs, h :: hs ->
          let%bind l = Hash.if_ b ~then_:h ~else_:acc
          and r = Hash.if_ b ~then_:acc ~else_:h in
          let%bind acc' = Hash.merge ~height l r in
          go (height + 1) acc' bs hs
      | _, _ ->
          failwith
            "Merkle_tree.Checked.implied_root: address, path length mismatch"
    in
    go 0 entry_hash addr0 path0

  type _ Request.t +=
    | Get_element : Address.value -> (Elt.value * Path.value) Request.t
    | Get_path : Address.value -> Path.value Request.t
    | Set : Address.value * Elt.value -> unit Request.t

  (* addr0 should have least significant bit first *)
  let%snarkydef_ fetch_and_update_req ~(depth : int) root addr0 ~f :
      (Hash.var * [ `Old of Elt.var ] * [ `New of Elt.var ]) Checked.t =
    let open Let_syntax in
    let%bind prev, prev_path =
      request_witness
        Typ.(Elt.typ * Path.typ ~depth)
        Impl.As_prover.(
          read (Address.typ ~depth) addr0 >>| fun addr -> Get_element addr)
    in
    let%bind () =
      let%bind prev_entry_hash = Elt.hash prev in
      implied_root prev_entry_hash addr0 prev_path >>= Hash.assert_equal root
    in
    let%bind next = f prev in
    let%bind next_entry_hash = Elt.hash next in
    let%bind () =
      perform
        (let open Impl.As_prover in
        let open Let_syntax in
        let%map addr = read (Address.typ ~depth) addr0
        and next = read Elt.typ next in
        Set (addr, next))
    in
    let%map new_root = implied_root next_entry_hash addr0 prev_path in
    (new_root, `Old prev, `New next)

  (* addr0 should have least significant bit first *)
  let%snarkydef_ modify_req ~(depth : int) root addr0 ~f : Hash.var Checked.t =
    let%map root, _, _ = fetch_and_update_req ~depth root addr0 ~f in
    root

  (* addr0 should have least significant bit first *)
  let%snarkydef_ get_req ~(depth : int) root addr0 : Elt.var Checked.t =
    let open Let_syntax in
    let%bind prev, prev_path =
      request_witness
        Typ.(Elt.typ * Path.typ ~depth)
        Impl.As_prover.(
          map (read (Address.typ ~depth) addr0) ~f:(fun a -> Get_element a))
    in
    let%bind () =
      let%bind prev_entry_hash = Elt.hash prev in
      implied_root prev_entry_hash addr0 prev_path >>= Hash.assert_equal root
    in
    return prev

  (* addr0 should have least significant bit first *)
  let%snarkydef_ update_req ~(depth : int) ~root ~prev ~next addr0 :
      Hash.var Checked.t =
    let open Let_syntax in
    let%bind prev_entry_hash = Elt.hash prev
    and next_entry_hash = Elt.hash next
    and prev_path =
      request_witness (Path.typ ~depth)
        Impl.As_prover.(
          map (read (Address.typ ~depth) addr0) ~f:(fun a -> Get_path a))
    in
    let%bind () =
      implied_root prev_entry_hash addr0 prev_path >>= Hash.assert_equal root
    in
    let%bind () =
      perform
        (let open Impl.As_prover in
        let open Let_syntax in
        let%map addr = read (Address.typ ~depth) addr0
        and next = read Elt.typ next in
        Set (addr, next))
    in
    implied_root next_entry_hash addr0 prev_path
end

module Run = struct
  module Make
      (Impl : Snark_intf.Run_basic) (Hash : sig
        type var

        type value

        val typ : (var, value) Impl.Typ.t

        val merge : height:int -> var -> var -> var

        val if_ : Impl.Boolean.var -> then_:var -> else_:var -> var

        val assert_equal : var -> var -> unit
      end) (Elt : sig
        type var

        type value

        val typ : (var, value) Impl.Typ.t

        val hash : var -> Hash.var
      end) =
  struct
    open Impl

    include
      Checked
        (Impl.Internal_Basic)
        (struct
          include Hash

          let merge ~height x y = make_checked (fun () -> merge ~height x y)

          let if_ x ~then_ ~else_ = make_checked (fun () -> if_ x ~then_ ~else_)

          let assert_equal x y = make_checked (fun () -> assert_equal x y)
        end)
        (struct
          include Elt

          let hash var = make_checked (fun () -> hash var)
        end)

    let implied_root entry_hash addr0 path0 =
      run_checked (implied_root entry_hash addr0 path0)

    let modify_req ~depth root addr0 ~f =
      run_checked
        (modify_req ~depth root addr0 ~f:(fun x -> make_checked (fun () -> f x)))

    let get_req ~depth root addr0 = run_checked (get_req ~depth root addr0)

    let update_req ~depth ~root ~prev ~next addr0 =
      run_checked (update_req ~depth ~root ~prev ~next addr0)
  end
end
