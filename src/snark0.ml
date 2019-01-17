module Bignum_bigint = Bigint
open Core_kernel

let () = Camlsnark_c.linkme

module Make_basic (Backend : Backend_intf.S) = struct
  open Backend

  type field = Field.t

  module Bigint = struct
    include Bigint.R

    let of_bignum_bigint n = of_decimal_string (Bignum_bigint.to_string n)

    let to_bignum_bigint n =
      let rec go i two_to_the_i acc =
        if i = Field.size_in_bits then acc
        else
          let acc' =
            if test_bit n i then Bignum_bigint.(acc + two_to_the_i) else acc
          in
          go (i + 1) Bignum_bigint.(two_to_the_i + two_to_the_i) acc'
      in
      go 0 Bignum_bigint.one Bignum_bigint.zero
  end

  module Proof = Proof

  module Verification_key = struct
    include Verification_key
    include Binable.Of_stringable (Verification_key)
  end

  module Proving_key = struct
    include Proving_key
  end

  module Keypair = struct
    type t = {pk: Proving_key.t; vk: Verification_key.t}
    [@@deriving fields, bin_io]

    let create = Fields.create

    let of_backend_keypair kp = {pk= Keypair.pk kp; vk= Keypair.vk kp}

    let generate = Fn.compose of_backend_keypair Backend.Keypair.create
  end

  module Var = struct
    module T = struct
      include Backend.Var

      let compare x y = Int.compare (index x) (index y)

      let t_of_sexp _ = failwith "Var.t_of_sexp"

      let sexp_of_t v =
        Sexp.(List [Atom "var"; Atom (Int.to_string (index v))])
    end

    include T
    include Comparable.Make (T)
  end

  module Field0 = struct
    include Field

    let size = Bigint.to_bignum_bigint Backend.field_size

    let inv x = if equal x zero then failwith "Field.inv: zero" else inv x

    (* TODO: Optimize *)
    let div x y = mul x (inv y)

    let negate x = sub zero x

    let unpack x =
      let n = Bigint.of_field x in
      List.init size_in_bits ~f:(fun i -> Bigint.test_bit n i)

    let project =
      let rec go x acc = function
        | [] -> acc
        | b :: bs -> go (Field.add x x) (if b then Field.add acc x else acc) bs
      in
      fun bs -> go Field.one Field.zero bs

    let compare t1 t2 = Bigint.(compare (of_field t1) (of_field t2))

    let hash_fold_t s x =
      Bignum_bigint.hash_fold_t s Bigint.(to_bignum_bigint (of_field x))

    let hash = Hash.of_fold hash_fold_t

    let to_bignum_bigint = Fn.compose Bigint.to_bignum_bigint Bigint.of_field

    let of_bignum_bigint = Fn.compose Bigint.to_field Bigint.of_bignum_bigint

    let sexp_of_t = Fn.compose Bignum_bigint.sexp_of_t to_bignum_bigint

    let t_of_sexp = Fn.compose of_bignum_bigint Bignum_bigint.t_of_sexp

    let to_string = Fn.compose Bignum_bigint.to_string to_bignum_bigint

    let of_string = Fn.compose of_bignum_bigint Bignum_bigint.of_string

    module Infix = struct
      let ( + ) = add

      let ( * ) = mul

      let ( - ) = sub

      let ( / ) = div
    end
  end

  module Cvar = struct
    include Cvar.Make (Field0) (Var)

    let var_indices t =
      let _, terms = to_constant_and_terms t in
      List.map ~f:(fun (_, v) -> Var.index v) terms
  end

  module Linear_combination = struct
    let of_constant = function
      | None -> Linear_combination.create ()
      | Some c -> Linear_combination.of_field c

    let of_var (cv : Cvar.t) =
      let constant, terms = Cvar.to_constant_and_terms cv in
      let t = of_constant constant in
      List.iter terms ~f:(fun (c, v) -> Linear_combination.add_term t c v) ;
      t

    let of_field = Backend.Linear_combination.of_field

    let zero = of_field Field.zero
  end

  module Constraint = struct
    open Constraint

    type basic = Cvar.t Constraint.basic

    type 'k with_constraint_args = ?label:string -> 'k

    type t = Cvar.t Constraint.t [@@deriving sexp]

    let basic_to_r1cs_constraint : basic -> R1CS_constraint.t =
      let of_var = Linear_combination.of_var in
      function
      | Boolean v ->
          let lc = of_var v in
          let constr = R1CS_constraint.create lc lc lc in
          R1CS_constraint.set_is_square constr true ;
          constr
      | Equal (v1, v2) ->
          (* 0 * 0 = (v1 - v2) *)
          let constr =
            R1CS_constraint.create Linear_combination.zero
              Linear_combination.zero
              (of_var (Cvar.sub v1 v2))
          in
          R1CS_constraint.set_is_square constr true ;
          constr
      | Square (a, c) ->
          let a = of_var a in
          let constr = R1CS_constraint.create a a (of_var c) in
          R1CS_constraint.set_is_square constr true ;
          constr
      | R1CS (a, b, c) ->
          let constr =
            R1CS_constraint.create (of_var a) (of_var b) (of_var c)
          in
          R1CS_constraint.set_is_square constr false ;
          constr

    let create_basic ?label basic = {basic; annotation= label}

    let override_label {basic; annotation= a} label_opt =
      {basic; annotation= (match label_opt with Some x -> Some x | None -> a)}

    let equal ?label x y = [create_basic ?label (Equal (x, y))]

    let boolean ?label x = [create_basic ?label (Boolean x)]

    let r1cs ?label a b c = [create_basic ?label (R1CS (a, b, c))]

    let square ?label a c = [create_basic ?label (Square (a, c))]

    let stack_to_string = String.concat ~sep:"\n"

    let add ~stack (t : t) system =
      List.iter t ~f:(fun {basic; annotation} ->
          let label = Option.value annotation ~default:"<unknown>" in
          let c = basic_to_r1cs_constraint basic in
          R1CS_constraint_system.add_constraint_with_annotation system c
            (stack_to_string (label :: stack)) )

    let eval_basic t get_value =
      match t with
      | Boolean v ->
          let x = get_value v in
          Field.(equal x zero || equal x one)
      | Equal (v1, v2) -> Field.equal (get_value v1) (get_value v2)
      | R1CS (v1, v2, v3) ->
          Field.(equal (mul (get_value v1) (get_value v2)) (get_value v3))
      | Square (a, c) -> Field.equal (Field.square (get_value a)) (get_value c)

    let eval t get_value =
      List.for_all t ~f:(fun {basic; _} -> eval_basic basic get_value)

    let annotation (t : t) =
      String.concat ~sep:"; "
        (List.filter_map t ~f:(fun {annotation; _} -> annotation))
  end

  module Typ_monads = struct
    open Typ_monads

    module A = struct
      type t1 = Field.t

      type t2 = Cvar.t
    end

    module Store = struct
      include Restrict_monad.Make3 (Store) (A)

      let store = Store.store

      let run = Store.run
    end

    module Read = struct
      include Restrict_monad.Make3 (Read) (A)

      let read = Read.read

      let run = Read.run
    end

    module Alloc = struct
      open Alloc

      include Restrict_monad.Make2
                (Alloc)
                (struct
                  type t = Cvar.t
                end)

      let alloc = alloc

      let run = run

      let size t =
        let dummy = Cvar.Unsafe.of_var (Backend.Var.create 0) in
        let rec go acc = function
          | Pure _ -> acc
          | Free (T.Alloc k) -> go (acc + 1) (k dummy)
        in
        go 0 t
    end
  end

  module As_prover0 = struct
    include As_prover.Make (struct
      type t = Cvar.t -> Field.t
    end)

    let read_var (v : Cvar.t) : (Field.t, 's) t = fun tbl s -> (s, tbl v)
  end

  module Handler = struct
    type t = Request.request -> Request.response
  end

  module Typ0 = struct
    type ('var, 'value) t =
      ('var, 'value, Field.t, Cvar.t, R1CS_constraint_system.t) Types.Typ.t
  end

  module Checked0 = struct
    type ('a, 's) t =
      ('a, 's, Field.t, Cvar.t, R1CS_constraint_system.t) Types.Checked.t
  end

  module Checked1 = struct
    module T = struct
      open Types.Checked
      include Checked0

      let return x = Pure x

      let as_prover x = As_prover (x, return ())

      let rec map : type s a b. (a, s) t -> f:(a -> b) -> (b, s) t =
       fun t ~f ->
        match t with
        | Pure x -> Pure (f x)
        | With_label (s, t, k) -> With_label (s, t, fun b -> map (k b) ~f)
        | With_constraint_system (c, k) -> With_constraint_system (c, map k ~f)
        | As_prover (x, k) -> As_prover (x, map k ~f)
        | Add_constraint (c, t1) -> Add_constraint (c, map t1 ~f)
        | With_state (p, and_then, t_sub, k) ->
            With_state (p, and_then, t_sub, fun b -> map (k b) ~f)
        | With_handler (h, t, k) -> With_handler (h, t, fun b -> map (k b) ~f)
        | Clear_handler (t, k) -> Clear_handler (t, fun b -> map (k b) ~f)
        | Exists (typ, c, k) -> Exists (typ, c, fun v -> map (k v) ~f)
        | Next_auxiliary k -> Next_auxiliary (fun x -> map (k x) ~f)

      let map = `Custom map

      let rec bind : type s a b. (a, s) t -> f:(a -> (b, s) t) -> (b, s) t =
       fun t ~f ->
        match t with
        | Pure x -> f x
        | With_label (s, t, k) -> With_label (s, t, fun b -> bind (k b) ~f)
        | With_constraint_system (c, k) -> With_constraint_system (c, bind k ~f)
        | As_prover (x, k) -> As_prover (x, bind k ~f)
        (* Someday: This case is probably a performance bug *)
        | Add_constraint (c, t1) -> Add_constraint (c, bind t1 ~f)
        | With_state (p, and_then, t_sub, k) ->
            With_state (p, and_then, t_sub, fun b -> bind (k b) ~f)
        | With_handler (h, t, k) -> With_handler (h, t, fun b -> bind (k b) ~f)
        | Clear_handler (t, k) -> Clear_handler (t, fun b -> bind (k b) ~f)
        | Exists (typ, c, k) -> Exists (typ, c, fun v -> bind (k v) ~f)
        | Next_auxiliary k -> Next_auxiliary (fun x -> bind (k x) ~f)
    end

    include T
    include Monad.Make2 (T)
  end

  module Typ = struct
    open Types.Typ
    include Typ_monads
    include Typ0

    type ('var, 'value) typ = ('var, 'value) t

    module Data_spec = struct
      type ('r_var, 'r_value, 'k_var, 'k_value) t =
        | ( :: ) :
            ('var, 'value) typ * ('r_var, 'r_value, 'k_var, 'k_value) t
            -> ('r_var, 'r_value, 'var -> 'k_var, 'value -> 'k_value) t
        | [] : ('r_var, 'r_value, 'r_var, 'r_value) t

      let size t =
        let rec go : type r_var r_value k_var k_value.
            int -> (r_var, r_value, k_var, k_value) t -> int =
         fun acc t ->
          match t with
          | [] -> acc
          | {alloc; _} :: t' -> go (acc + Alloc.size alloc) t'
        in
        go 0 t
    end

    let store ({store; _} : ('var, 'value) t) (x : 'value) : 'var Store.t =
      store x

    let read ({read; _} : ('var, 'value) t) (v : 'var) : 'value Read.t = read v

    let alloc ({alloc; _} : ('var, 'value) t) : 'var Alloc.t = alloc

    let check ({check; _} : ('var, 'value) t) (v : 'var) :
        (unit, 's) Checked1.t =
      let do_nothing : (unit, _) As_prover0.t = fun _ s -> (s, ()) in
      With_state (do_nothing, (fun () -> do_nothing), check v, Checked1.return)

    let unit : (unit, unit) t =
      let s = Store.return () in
      let r = Read.return () in
      let c = Checked1.return () in
      { store= (fun () -> s)
      ; read= (fun () -> r)
      ; check= (fun () -> c)
      ; alloc= Alloc.return () }

    let field : (Cvar.t, Field.t) t =
      { store= Store.store
      ; read= Read.read
      ; alloc= Alloc.alloc
      ; check= (fun _ -> Checked1.return ()) }

    let hlist (type k_var k_value)
        (spec0 : (unit, unit, k_var, k_value) Data_spec.t) :
        ((unit, k_var) H_list.t, (unit, k_value) H_list.t) t =
      let store xs0 : _ Store.t =
        let rec go : type k_var k_value.
               (unit, unit, k_var, k_value) Data_spec.t
            -> (unit, k_value) H_list.t
            -> (unit, k_var) H_list.t Store.t =
         fun spec0 xs0 ->
          let open Data_spec in
          let open H_list in
          match (spec0, xs0) with
          | [], [] -> Store.return H_list.[]
          | s :: spec, x :: xs ->
              let open Store.Let_syntax in
              let%map y = store s x and ys = go spec xs in
              y :: ys
        in
        go spec0 xs0
      in
      let read xs0 : (unit, k_value) H_list.t Read.t =
        let rec go : type k_var k_value.
               (unit, unit, k_var, k_value) Data_spec.t
            -> (unit, k_var) H_list.t
            -> (unit, k_value) H_list.t Read.t =
         fun spec0 xs0 ->
          let open Data_spec in
          let open H_list in
          match (spec0, xs0) with
          | [], [] -> Read.return H_list.[]
          | s :: spec, x :: xs ->
              let open Read.Let_syntax in
              let%map y = read s x and ys = go spec xs in
              y :: ys
        in
        go spec0 xs0
      in
      let alloc : _ Alloc.t =
        let rec go : type k_var k_value.
               (unit, unit, k_var, k_value) Data_spec.t
            -> (unit, k_var) H_list.t Alloc.t =
         fun spec0 ->
          let open Data_spec in
          let open H_list in
          match spec0 with
          | [] -> Alloc.return H_list.[]
          | s :: spec ->
              let open Alloc.Let_syntax in
              let%map y = alloc s and ys = go spec in
              y :: ys
        in
        go spec0
      in
      let check xs0 : (unit, unit) Checked1.t =
        let rec go : type k_var k_value.
               (unit, unit, k_var, k_value) Data_spec.t
            -> (unit, k_var) H_list.t
            -> (unit, unit) Checked1.t =
         fun spec0 xs0 ->
          let open Data_spec in
          let open H_list in
          let open Checked1.Let_syntax in
          match (spec0, xs0) with
          | [], [] -> return ()
          | s :: spec, x :: xs ->
              let%map () = check s x and () = go spec xs in
              ()
        in
        go spec0 xs0
      in
      {read; store; alloc; check}

    let transport ({read; store; alloc; check} : ('var1, 'value1) t)
        ~(there : 'value2 -> 'value1) ~(back : 'value1 -> 'value2) :
        ('var1, 'value2) t =
      { alloc
      ; store= (fun x -> store (there x))
      ; read= (fun v -> Read.map ~f:back (read v))
      ; check }

    let transport_var ({read; store; alloc; check} : ('var1, 'value) t)
        ~(there : 'var2 -> 'var1) ~(back : 'var1 -> 'var2) : ('var2, 'value) t
        =
      { alloc= Alloc.map alloc ~f:back
      ; store= (fun x -> Store.map (store x) ~f:back)
      ; read= (fun x -> read (there x))
      ; check= (fun x -> check (there x)) }

    (* TODO: Do a CPS style thing instead if it ends up being an issue converting
     back and forth. *)
    let of_hlistable (spec : (unit, unit, 'k_var, 'k_value) Data_spec.t)
        ~(var_to_hlist : 'var -> (unit, 'k_var) H_list.t)
        ~(var_of_hlist : (unit, 'k_var) H_list.t -> 'var)
        ~(value_to_hlist : 'value -> (unit, 'k_value) H_list.t)
        ~(value_of_hlist : (unit, 'k_value) H_list.t -> 'value) :
        ('var, 'value) t =
      let {read; store; alloc; check} = hlist spec in
      { read= (fun v -> Read.map ~f:value_of_hlist (read (var_to_hlist v)))
      ; store= (fun x -> Store.map ~f:var_of_hlist (store (value_to_hlist x)))
      ; alloc= Alloc.map ~f:var_of_hlist alloc
      ; check= (fun v -> check (var_to_hlist v)) }

    let list ~length ({read; store; alloc; check} : ('elt_var, 'elt_value) t) :
        ('elt_var list, 'elt_value list) t =
      let store ts =
        let n = List.length ts in
        if n <> length then
          failwithf "Typ.list: Expected length %d, got %d" length n () ;
        Store.all (List.map ~f:store ts)
      in
      let alloc = Alloc.all (List.init length ~f:(fun _ -> alloc)) in
      let check ts = Checked1.all_unit (List.map ts ~f:check) in
      let read vs = Read.all (List.map vs ~f:read) in
      {read; store; alloc; check}

    (* TODO-someday: Make more efficient *)
    let array ~length ({read; store; alloc; check} : ('elt_var, 'elt_value) t)
        : ('elt_var array, 'elt_value array) t =
      let store ts =
        assert (Array.length ts = length) ;
        Store.map ~f:Array.of_list
          (Store.all (List.map ~f:store (Array.to_list ts)))
      in
      let alloc =
        let open Alloc.Let_syntax in
        let%map vs = Alloc.all (List.init length ~f:(fun _ -> alloc)) in
        Array.of_list vs
      in
      let read vs =
        assert (Array.length vs = length) ;
        Read.map ~f:Array.of_list
          (Read.all (List.map ~f:read (Array.to_list vs)))
      in
      let check ts =
        assert (Array.length ts = length) ;
        let open Checked1.Let_syntax in
        let rec go i =
          if i = length then return ()
          else
            let%map () = check ts.(i) and () = go (i + 1) in
            ()
        in
        go 0
      in
      {read; store; alloc; check}

    (* TODO: Assert that a stored value has the same shape as the template. *)
    module Of_traversable (T : Traversable.S) = struct
      let typ ~template
          ({read; store; alloc; check} : ('elt_var, 'elt_value) t) :
          ('elt_var T.t, 'elt_value T.t) t =
        let traverse_store =
          let module M = T.Traverse (Store) in
          M.f
        in
        let traverse_read =
          let module M = T.Traverse (Read) in
          M.f
        in
        let traverse_alloc =
          let module M = T.Traverse (Alloc) in
          M.f
        in
        let traverse_checked =
          let module M =
            T.Traverse
              (Restrict_monad.Make2
                 (Checked1)
                 (struct
                   type t = unit
                 end)) in
          M.f
        in
        let read var = traverse_read var ~f:read in
        let store value = traverse_store value ~f:store in
        let alloc = traverse_alloc template ~f:(fun () -> alloc) in
        let check t = Checked1.map (traverse_checked t ~f:check) ~f:ignore in
        {read; store; alloc; check}
    end

    let tuple2 (typ1 : ('var1, 'value1) t) (typ2 : ('var2, 'value2) t) :
        ('var1 * 'var2, 'value1 * 'value2) t =
      let alloc =
        let open Alloc.Let_syntax in
        let%map x = typ1.alloc and y = typ2.alloc in
        (x, y)
      in
      let read (x, y) =
        let open Read.Let_syntax in
        let%map x = typ1.read x and y = typ2.read y in
        (x, y)
      in
      let store (x, y) =
        let open Store.Let_syntax in
        let%map x = typ1.store x and y = typ2.store y in
        (x, y)
      in
      let check (x, y) =
        let open Checked1.Let_syntax in
        let%map () = typ1.check x and () = typ2.check y in
        ()
      in
      {read; store; alloc; check}

    let ( * ) = tuple2

    let tuple3 (typ1 : ('var1, 'value1) t) (typ2 : ('var2, 'value2) t)
        (typ3 : ('var3, 'value3) t) :
        ('var1 * 'var2 * 'var3, 'value1 * 'value2 * 'value3) t =
      let alloc =
        let open Alloc.Let_syntax in
        let%map x = typ1.alloc and y = typ2.alloc and z = typ3.alloc in
        (x, y, z)
      in
      let read (x, y, z) =
        let open Read.Let_syntax in
        let%map x = typ1.read x and y = typ2.read y and z = typ3.read z in
        (x, y, z)
      in
      let store (x, y, z) =
        let open Store.Let_syntax in
        let%map x = typ1.store x and y = typ2.store y and z = typ3.store z in
        (x, y, z)
      in
      let check (x, y, z) =
        let open Checked1.Let_syntax in
        let%map () = typ1.check x
        and () = typ2.check y
        and () = typ3.check z in
        ()
      in
      {read; store; alloc; check}
  end

  module As_prover = struct
    include As_prover0

    type ('a, 'prover_state) as_prover = ('a, 'prover_state) t

    let read ({read; _} : ('var, 'value) Typ.t) (var : 'var) :
        ('value, 'prover_state) t =
     fun tbl s -> (s, Typ.Read.run (read var) tbl)

    module Ref = struct
      type 'a t = 'a option ref

      let create (x : ('a, 's) As_prover0.t) : ('a t, 's) Checked1.t =
        let r = ref None in
        let open Checked1.Let_syntax in
        let%map () = Checked1.as_prover (map x ~f:(fun x -> r := Some x)) in
        r

      let get (r : 'a t) _tbl s = (s, Option.value_exn !r)

      let set (r : 'a t) x _tbl s = (s, (r := Some x))
    end
  end

  module Handle = struct
    include Handle

    let value (t : ('var, 'value) t) : ('value, 's) As_prover0.t =
     fun _ s -> (s, Option.value_exn t.value)

    let var {var; _} = var
  end

  module Checked = struct
    open Types.Checked
    include Checked1

    let request_witness (typ : ('var, 'value) Typ.t)
        (r : ('value Request.t, 's) As_prover.t) =
      Exists (typ, Request r, fun h -> return (Handle.var h))

    let perform req = request_witness Typ.unit req

    let request ?such_that typ r =
      match such_that with
      | None -> request_witness typ (As_prover.return r)
      | Some such_that ->
          let open Let_syntax in
          let%bind x = request_witness typ (As_prover.return r) in
          let%map () = such_that x in
          x

    let exists ?request ?compute typ =
      let provider =
        let request =
          Option.value request ~default:(As_prover.return Request.Fail)
        in
        match compute with
        | None -> Provider.Request request
        | Some c -> Provider.Both (request, c)
      in
      Exists (typ, provider, fun h -> return (Handle.var h))

    type response = Request.response

    let unhandled = Request.unhandled

    type request = Request.request =
      | With :
          { request: 'a Request.t
          ; respond: 'a Request.Response.t -> response }
          -> request

    let handle t k = With_handler (Request.Handler.create_single k, t, return)

    let next_auxiliary = Next_auxiliary return

    let with_constraint_system f = With_constraint_system (f, return ())

    let with_label s t = With_label (s, t, return)

    let do_nothing _ = As_prover.return ()

    let with_state ?(and_then = do_nothing) f sub =
      With_state (f, and_then, sub, return)

    let assert_ ?label c =
      Add_constraint
        (List.map c ~f:(fun c -> Constraint.override_label c label), return ())

    let assert_r1cs ?label a b c = assert_ (Constraint.r1cs ?label a b c)

    let assert_square ?label a c = assert_ (Constraint.square ?label a c)

    let assert_all =
      let map_concat_rev xss ~f =
        let rec go acc xs xss =
          match (xs, xss) with
          | [], [] -> acc
          | [], xs :: xss -> go acc xs xss
          | x :: xs, _ -> go (f x :: acc) xs xss
        in
        go [] [] xss
      in
      fun ?label cs ->
        Add_constraint
          ( map_concat_rev ~f:(fun c -> Constraint.override_label c label) cs
          , return () )

    let assert_equal ?label x y = assert_ (Constraint.equal ?label x y)

    let constraint_count ?(log = fun ?start _ _ -> ()) (t : (_, _) t) : int =
      let next_auxiliary = ref 1 in
      let alloc_var () =
        let v = Backend.Var.create !next_auxiliary in
        incr next_auxiliary ; Cvar.Unsafe.of_var v
      in
      let rec go : type a s. int -> (a, s) t -> int * a =
       fun count t0 ->
        match t0 with
        | Pure x -> (count, x)
        | With_constraint_system (_f, k) -> go count k
        | As_prover (_x, k) -> go count k
        | Add_constraint (_c, t) -> go (count + 1) t
        | Next_auxiliary k -> go count (k !next_auxiliary)
        | With_label (s, t, k) ->
            log ~start:true s count ;
            let count', y = go count t in
            log s count' ;
            go count' (k y)
        | With_state (_p, _and_then, t_sub, k) ->
            let count', y = go count t_sub in
            go count' (k y)
        | With_handler (_h, t, k) ->
            let count, x = go count t in
            go count (k x)
        | Clear_handler (t, k) ->
            let count, x = go count t in
            go count (k x)
        | Exists ({alloc; check; _}, _c, k) ->
            let var = Typ.Alloc.run alloc alloc_var in
            (* TODO: Push a label onto the stack here *)
            let count, () = go count (check var) in
            go count (k {Handle.var; value= None})
      in
      fst (go 0 t)

    let run (type a s) ~num_inputs ~input ~next_auxiliary ~aux ?system
        ?(eval_constraints = false) (t0 : (a, s) t) (s0 : s option) =
      next_auxiliary := 1 + num_inputs ;
      (* We can't evaluate the constraints if we are not computing over a value. *)
      let eval_constraints = eval_constraints && Option.is_some s0 in
      let get_value : Cvar.t -> Field.t =
        let get_one v =
          let i = Backend.Var.index v in
          if i <= num_inputs then Field.Vector.get input (i - 1)
          else Field.Vector.get aux (i - num_inputs - 1)
        in
        Cvar.eval get_one
      and store_field_elt x =
        let v = Backend.Var.create !next_auxiliary in
        incr next_auxiliary ;
        Field.Vector.emplace_back aux x ;
        Cvar.Unsafe.of_var v
      and alloc_var () =
        let v = Backend.Var.create !next_auxiliary in
        incr next_auxiliary ; Cvar.Unsafe.of_var v
      in
      let run_as_prover x s =
        match (x, s) with
        | Some x, Some s ->
            let s', y = As_prover.run x get_value s in
            (Some s', Some y)
        | _, _ -> (None, None)
      in
      Option.iter system ~f:(fun system ->
          R1CS_constraint_system.set_primary_input_size system num_inputs ) ;
      (* INVARIANT: go _ _ _ s = (s', _) gives (s' = Some _) iff (s = Some _) *)
      let rec go : type a s.
             string list
          -> (a, s) t
          -> Request.Handler.t
          -> s option
          -> s option * a =
       fun stack t handler s ->
        match t with
        | Pure x -> (s, x)
        | With_constraint_system (f, k) ->
            Option.iter ~f system ; go stack k handler s
        | With_label (lab, t, k) ->
            let s', y = go (lab :: stack) t handler s in
            go stack (k y) handler s'
        | As_prover (x, k) ->
            let s', (_ : unit option) = run_as_prover (Some x) s in
            go stack k handler s'
        | Add_constraint (c, t) ->
            if eval_constraints && not (Constraint.eval c get_value) then
              failwithf "Constraint unsatisfied:\n%s\n%s\n"
                (Constraint.annotation c)
                (Constraint.stack_to_string stack)
                () ;
            Option.iter system ~f:(fun system -> Constraint.add ~stack c system) ;
            go stack t handler s
        | With_state (p, and_then, t_sub, k) ->
            let s, s_sub = run_as_prover (Some p) s in
            let s_sub, y = go stack t_sub handler s_sub in
            let s, (_ : unit option) =
              run_as_prover (Option.map ~f:and_then s_sub) s
            in
            go stack (k y) handler s
        | With_handler (h, t, k) ->
            let s', y = go stack t (Request.Handler.push handler h) s in
            go stack (k y) handler s'
        | Clear_handler (t, k) ->
            let s', y = go stack t Request.Handler.fail s in
            go stack (k y) handler s'
        | Exists ({store; alloc; check; _}, p, k) -> (
          match s with
          | Some s ->
              let s', value = Provider.run p get_value s handler in
              let var = Typ.Store.run (store value) store_field_elt in
              (* TODO: Push a label onto the stack here *)
              let (_ : unit option), () =
                go stack (check var) handler (Some ())
              in
              go stack (k {Handle.var; value= Some value}) handler (Some s')
          | None ->
              let var = Typ.Alloc.run alloc alloc_var in
              (* TODO: Push a label onto the stack here *)
              let (_ : unit option), () = go stack (check var) handler None in
              go stack (k {Handle.var; value= None}) handler None )
        | Next_auxiliary k -> go stack (k !next_auxiliary) handler s
      in
      let retval = go [] t0 Request.Handler.fail s0 in
      Option.iter system ~f:(fun system ->
          let auxiliary_input_size = !next_auxiliary - (1 + num_inputs) in
          R1CS_constraint_system.set_auxiliary_input_size system
            auxiliary_input_size ) ;
      retval

    (* TODO-someday: Add pass to unify variables which have an Equal constraint *)
    let constraint_system ~num_inputs (t : (unit, 's) t) :
        R1CS_constraint_system.t =
      let input = Field.Vector.create () in
      let next_auxiliary = ref (1 + num_inputs) in
      let aux = Field.Vector.create () in
      let system = R1CS_constraint_system.create () in
      ignore (run ~num_inputs ~input ~next_auxiliary ~aux ~system t None) ;
      system

    let auxiliary_input (type s) ~num_inputs (t0 : (unit, s) t) (s0 : s)
        (input : Field.Vector.t) : Field.Vector.t =
      let next_auxiliary = ref (1 + num_inputs) in
      let aux = Field.Vector.create () in
      ignore (run ~num_inputs ~input ~next_auxiliary ~aux t0 (Some s0)) ;
      aux

    let run_and_check' (type a s) (t0 : (a, s) t) (s0 : s) =
      let num_inputs = 0 in
      let input = Field.Vector.create () in
      let next_auxiliary = ref 1 in
      let aux = Field.Vector.create () in
      let system = R1CS_constraint_system.create () in
      let get_value : Cvar.t -> Field.t =
        let get_one v = Field.Vector.get aux (Backend.Var.index v - 1) in
        Cvar.eval get_one
      in
      match
        run ~num_inputs ~input ~next_auxiliary ~aux ~system
          ~eval_constraints:true t0 (Some s0)
      with
      | exception e -> Or_error.of_exn e
      | Some s, x ->
          let primary_input = Field.Vector.create () in
          R1CS_constraint_system.set_auxiliary_input_size system
            (!next_auxiliary - 1) ;
          if
            not
              (R1CS_constraint_system.is_satisfied system ~primary_input
                 ~auxiliary_input:aux)
          then Or_error.error_string "Unknown constraint unsatisfied"
          else Ok (s, x, get_value)
      | None, _ ->
          failwith "run_and_check': Expected a value from run, got None."

    let run_unchecked (type a s) (t0 : (a, s) t) (s0 : s) =
      let num_inputs = 0 in
      let input = Field.Vector.create () in
      let next_auxiliary = ref 1 in
      let aux = Field.Vector.create () in
      match run ~num_inputs ~input ~next_auxiliary ~aux t0 (Some s0) with
      | Some s, x -> (s, x)
      | None, _ ->
          failwith "run_unchecked: Expected a value from run, got None."

    let run_and_check t s =
      Or_error.map (run_and_check' t s) ~f:(fun (s, x, get_value) ->
          let s', x = As_prover.run x get_value s in
          (s', x) )

    let check t s = Or_error.is_ok (run_and_check' t s)

    let equal (x : Cvar.t) (y : Cvar.t) : (Cvar.t Boolean.t, _) t =
      let open Let_syntax in
      let%bind inv =
        exists Typ.field
          ~compute:
            (let open As_prover.Let_syntax in
            let%map x = As_prover.read_var x and y = As_prover.read_var y in
            if Field.equal x y then Field.zero else Field.inv (Field.sub x y))
      and r =
        exists Typ.field
          ~compute:
            (let open As_prover.Let_syntax in
            let%map x = As_prover.read_var x and y = As_prover.read_var y in
            if Field.equal x y then Field.one else Field.zero)
      in
      let%map () =
        let open Constraint in
        let open Cvar.Infix in
        assert_all
          [ r1cs ~label:"equals_1" inv (x - y) (Cvar.constant Field.one - r)
          ; r1cs ~label:"equals_2" r (x - y) (Cvar.constant Field.zero) ]
      in
      Boolean.Unsafe.create r

    let mul ?(label = "Checked.mul") (x : Cvar.t) (y : Cvar.t) =
      match (x, y) with
      | Constant x, Constant y -> return (Cvar.constant (Field.mul x y))
      | Constant x, _ -> return (Cvar.scale y x)
      | _, Constant y -> return (Cvar.scale x y)
      | _, _ ->
          with_label label
            (let open Let_syntax in
            let%bind z =
              exists Typ.field
                ~compute:
                  As_prover.(map2 (read_var x) (read_var y) ~f:Field.mul)
            in
            let%map () = assert_r1cs x y z in
            z)

    let square ?(label = "Checked.square") (x : Cvar.t) =
      match x with
      | Constant x -> return (Cvar.constant (Field.square x))
      | _ ->
          with_label label
            (let open Let_syntax in
            let%bind z =
              exists Typ.field
                ~compute:As_prover.(map (read_var x) ~f:Field.square)
            in
            let%map () = assert_square x z in
            z)

    (* We get a better stack trace by failing at the call to is_satisfied, so we
     put a bogus value for the inverse to make the constraint system unsat if
     x is zero. *)
    let inv ?(label = "Checked.inv") (x : Cvar.t) =
      match x with
      | Constant x -> return (Cvar.constant (Field.inv x))
      | _ ->
          with_label label
            (let open Let_syntax in
            let%bind x_inv =
              exists Typ.field
                ~compute:
                  As_prover.(
                    map (read_var x) ~f:(fun x ->
                        if Field.(equal zero x) then Field.zero
                        else Backend.Field.inv x ))
            in
            let%map () =
              assert_r1cs ~label:"field_inverse" x x_inv
                (Cvar.constant Field.one)
            in
            x_inv)

    let div ?(label = "Checked.div") x y =
      with_label label
        (let open Let_syntax in
        let%bind y_inv = inv y in
        mul x y_inv)

    let%snarkydef if_ (b : Cvar.t Boolean.t) ~then_ ~else_ =
      let open Let_syntax in
      (* r = e + b (t - e)
      r - e = b (t - e)
    *)
      let%bind r =
        exists Typ.field
          ~compute:
            (let open As_prover in
            let open Let_syntax in
            let%bind b = read_var (b :> Cvar.t) in
            read Typ.field (if Field.equal b Field.one then then_ else else_))
      in
      let%map () =
        assert_r1cs
          (b :> Cvar.t)
          Cvar.Infix.(then_ - else_)
          Cvar.Infix.(r - else_)
      in
      r

    let%snarkydef assert_non_zero (v : Cvar.t) =
      let open Let_syntax in
      let%map _ = inv v in
      ()

    module Boolean = struct
      open Boolean.Unsafe

      type var = Cvar.t Boolean.t

      type value = bool

      let true_ : var = create (Cvar.constant Field.one)

      let false_ : var = create (Cvar.constant Field.zero)

      let not (x : var) : var =
        create Cvar.Infix.((true_ :> Cvar.t) - (x :> Cvar.t))

      let if_ b ~(then_ : var) ~(else_ : var) =
        Checked1.map ~f:create
          (if_ b ~then_:(then_ :> Cvar.t) ~else_:(else_ :> Cvar.t))

      let ( && ) (x : var) (y : var) =
        (* (x + y)^2 = 2 z + x + y

           x^2 + 2 x*y + y^2 = 2 z + x + y
           x + 2 x*y + y = 2 z + x + y
           2 x*y = 2 z
           x * y = z
        *)
        let x = (x :> Cvar.t) in
        let y = (y :> Cvar.t) in
        let open Let_syntax in
        let%bind z =
          exists Typ.field
            ~compute:
              (let open As_prover in
              let open Let_syntax in
              let%map x = read_var x and y = read_var y in
              if Field.(equal one x) && Field.(equal one y) then Field.one
              else Field.zero)
        in
        let%map () =
          let x_plus_y = Cvar.add x y in
          assert_square x_plus_y Cvar.Infix.((Field.of_int 2 * z) + x_plus_y)
        in
        create z

      let ( || ) x y =
        let open Let_syntax in
        let%map both_false = (not x) && not y in
        not both_false

      let any = function
        | [] -> return false_
        | [b1] -> return b1
        | [b1; b2] -> b1 || b2
        | bs ->
            let open Let_syntax in
            let%map all_zero =
              equal (Cvar.sum (bs :> Cvar.t list)) (Cvar.constant Field.zero)
            in
            not all_zero

      let all = function
        | [] -> return true_
        | [b1] -> return b1
        | [b1; b2] -> b1 && b2
        | bs ->
            equal
              (Cvar.constant (Field.of_int (List.length bs)))
              (Cvar.sum (bs :> Cvar.t list))

      let equal (x : var) (y : var) = equal (x :> Cvar.t) (y :> Cvar.t)

      let of_field x =
        let open Let_syntax in
        let%map () = assert_ (Constraint.boolean x) in
        create x

      let var_of_value b = if b then true_ else false_

      module Unsafe = struct
        let of_cvar (t : Cvar.t) : var = create t
      end

      let typ : (var, value) Typ.t =
        let open Typ in
        let store b =
          Store.(map (store (if b then Field.one else Field.zero)) ~f:create)
        in
        let read (v : var) =
          let open Read.Let_syntax in
          let%map x = Read.read (v :> Cvar.t) in
          if Field.equal x Field.one then true
          else if Field.equal x Field.zero then false
          else failwith "Boolean.typ: Got non boolean value for variable"
        in
        let alloc = Alloc.(map alloc ~f:create) in
        let check (v : var) =
          assert_ (Constraint.boolean ~label:"boolean-alloc" (v :> Cvar.t))
        in
        {read; store; alloc; check}

      let typ_unchecked : (var, value) Typ.t =
        {typ with check= (fun _ -> return ())}

      module Assert = struct
        let ( = ) (x : var) (y : var) = assert_equal (x :> Cvar.t) (y :> Cvar.t)

        let is_true (v : var) = v = true_

        let%snarkydef any (bs : var list) =
          assert_non_zero (Cvar.sum (bs :> Cvar.t list))

        let%snarkydef all (bs : var list) =
          assert_equal
            (Cvar.sum (bs :> Cvar.t list))
            (Cvar.constant (Field.of_int (List.length bs)))

        let%snarkydef exactly_one (bs : var list) =
          assert_equal (Cvar.sum (bs :> Cvar.t list)) (Cvar.constant Field.one)
      end

      module Expr = struct
        type t = Var of var | And of t list | Or of t list | Not of t

        let rec eval t =
          let open Let_syntax in
          match t with
          | Not t -> eval t >>| not
          | Var v -> return v
          | And ts -> Checked1.all (List.map ~f:eval ts) >>= all
          | Or ts -> Checked1.all (List.map ~f:eval ts) >>= any

        let assert_ t = eval t >>= Assert.is_true

        let ( ! ) v = Var v

        let ( && ) x y = And [x; y]

        let ( || ) x y = Or [x; y]

        let not t = Not t

        let any xs = Or xs

        let all xs = And xs
      end
    end

    module Control = struct end

    let two_to_the n =
      let rec go acc i =
        if i = 0 then acc else go (Field0.add acc acc) (i - 1)
      in
      go Field0.one n

    type _ Request.t +=
      | Choose_preimage : Field.t * int -> bool list Request.t

    let choose_preimage_unchecked v ~length =
      exists
        (Typ.list Boolean.typ ~length)
        ~request:
          As_prover.(
            map (read_var v) ~f:(fun x -> Choose_preimage (x, length)))
        ~compute:
          (let open As_prover.Let_syntax in
          let%map x = As_prover.read_var v in
          let x = Bigint.of_field x in
          List.init length ~f:(fun i -> Bigint.test_bit x i))

    let packing_sum (bits : Boolean.var list) =
      let ts, _ =
        List.fold_left bits ~init:([], Field.one) ~f:(fun (acc, c) v ->
            ((c, (v :> Cvar.t)) :: acc, Field.add c c) )
      in
      Cvar.linear_combination ts

    let choose_preimage (v : Cvar.t) ~length : (Boolean.var list, 's) t =
      let open Let_syntax in
      let%bind bits = choose_preimage_unchecked v ~length in
      let lc = packing_sum bits in
      let%map () =
        assert_r1cs ~label:"Choose_preimage" lc (Cvar.constant Field.one) v
      in
      bits

    let choose_preimage_flagged (v : Cvar.t) ~length =
      let open Let_syntax in
      let%bind bits = choose_preimage_unchecked v ~length in
      let lc = packing_sum bits in
      let%map success = equal lc v in
      (bits, `Success success)

    module List =
      Monad_sequence.List
        (Checked1)
        (struct
          type t = Boolean.var

          include Boolean
        end)
  end

  module Data_spec = Typ.Data_spec

  module Run = struct
    open Data_spec

    let alloc_var next_input () =
      let v = Backend.Var.create !next_input in
      incr next_input ; Cvar.Unsafe.of_var v

    let rec collect_input_constraints : type s r2 k1 k2.
           int ref
        -> ((unit, s) Checked.t, r2, k1, k2) t
        -> k1
        -> (unit, s) Checked.t =
     fun next_input t k ->
      match t with
      | [] -> k
      | {alloc; check; _} :: t' ->
          let var = Typ.Alloc.run alloc (alloc_var next_input) in
          let r = collect_input_constraints next_input t' (k var) in
          let open Checked.Let_syntax in
          let%map () = Checked.with_state (As_prover.return ()) (check var)
          and () = r in
          ()

    let r1cs_h : type s r2 k1 k2.
           int ref
        -> ((unit, s) Checked.t, r2, k1, k2) t
        -> k1
        -> R1CS_constraint_system.t =
     fun next_input t k ->
      let r = collect_input_constraints next_input t k in
      Checked.constraint_system ~num_inputs:(!next_input - 1) r

    let constraint_system :
           exposing:((unit, 's) Checked.t, _, 'k_var, _) t
        -> 'k_var
        -> R1CS_constraint_system.t =
     fun ~exposing k -> r1cs_h (ref 1) exposing k

    let generate_keypair :
        exposing:((unit, 's) Checked.t, _, 'k_var, _) t -> 'k_var -> Keypair.t
        =
     fun ~exposing k -> Keypair.generate (constraint_system ~exposing k)

    let verify :
           Proof.t
        -> Verification_key.t
        -> ('r_var, bool, 'k_var, 'k_value) t
        -> 'k_value =
     fun proof vk t0 ->
      let primary_input = Field.Vector.create () in
      let store_field_elt =
        let next_input = ref 1 in
        fun x ->
          let v = Backend.Var.create !next_input in
          incr next_input ;
          Field.Vector.emplace_back primary_input x ;
          Cvar.Unsafe.of_var v
      in
      let rec go : type r_var k_var k_value.
          (r_var, bool, k_var, k_value) t -> k_value =
       fun t ->
        match t with
        | [] -> Proof.verify proof vk primary_input
        | {store; _} :: t' ->
            fun value ->
              let _var = Typ.Store.run (store value) store_field_elt in
              go t'
      in
      go t0

    let conv : type r_var r_value.
           (r_var -> Field.Vector.t -> r_value)
        -> (r_var, r_value, 'k_var, 'k_value) t
        -> 'k_var
        -> 'k_value =
     fun cont0 t0 k0 ->
      let primary_input = Field.Vector.create () in
      let store_field_elt =
        let next_input = ref 1 in
        fun x ->
          let v = Backend.Var.create !next_input in
          incr next_input ;
          Field.Vector.emplace_back primary_input x ;
          Cvar.Unsafe.of_var v
      in
      let rec go : type k_var k_value.
          (r_var, r_value, k_var, k_value) t -> k_var -> k_value =
       fun t k ->
        match t with
        | [] -> cont0 k primary_input
        | {store; _} :: t' ->
            fun value ->
              let var = Typ.Store.run (store value) store_field_elt in
              go t' (k var)
      in
      go t0 k0

    let prove :
           Proving_key.t
        -> ((unit, 's) Checked.t, Proof.t, 'k_var, 'k_value) t
        -> 's
        -> 'k_var
        -> 'k_value =
     fun key t s k ->
      conv
        (fun c primary ->
          let auxiliary =
            Checked.auxiliary_input
              ~num_inputs:(Field.Vector.length primary)
              c s primary
          in
          Proof.create key ~primary ~auxiliary )
        t k
  end

  module Cvar1 = struct
    include Cvar

    let project (vars : Checked.Boolean.var list) =
      let rec go c acc = function
        | [] -> List.rev acc
        | v :: vs -> go (Field.add c c) ((c, v) :: acc) vs
      in
      Cvar.linear_combination (go Field.one [] (vars :> Cvar.t list))

    let pack vars =
      assert (List.length vars < Field.size_in_bits) ;
      project vars

    let unpack v ~length =
      assert (length < Field.size_in_bits) ;
      Checked.choose_preimage v ~length

    let unpack_flagged v ~length =
      assert (length < Field.size_in_bits) ;
      Checked.choose_preimage_flagged v ~length
  end

  module Field = struct
    include Field0

    let gen =
      Quickcheck.Generator.map
        Bignum_bigint.(gen_incl zero (size - one))
        ~f:(fun x -> Bigint.(to_field (of_bignum_bigint x)))

    type var = Cvar.t

    let typ = Typ.field

    module Checked = struct
      include Cvar1

      let to_constant : var -> Field0.t option = function
        | Constant x -> Some x
        | _ -> None

      let equal = Checked.equal

      let mul x y = Checked.mul ~label:"Field.Checked.mul" x y

      let square x = Checked.square ~label:"Field.Checked.square" x

      let div x y = Checked.div ~label:"Field.Checked.div" x y

      let inv x = Checked.inv ~label:"Field.Checked.inv" x

      let choose_preimage_var = Checked.choose_preimage

      type comparison_result =
        {less: Checked.Boolean.var; less_or_equal: Checked.Boolean.var}

      let if_ = Checked.if_

      let compare ~bit_length a b =
        let open Checked in
        let open Let_syntax in
        [%with_label "compare"]
          (let alpha_packed =
             Cvar.Infix.(Cvar.constant (two_to_the bit_length) + b - a)
           in
           let%bind alpha = unpack alpha_packed ~length:(bit_length + 1) in
           let prefix, less_or_equal =
             match Core_kernel.List.split_n alpha bit_length with
             | p, [l] -> (p, l)
             | _ -> failwith "compare: Invalid alpha"
           in
           let%bind not_all_zeros = Boolean.any prefix in
           let%map less = Boolean.(less_or_equal && not_all_zeros) in
           {less; less_or_equal})

      module Assert = struct
        let lt ~bit_length x y =
          let open Checked in
          let open Let_syntax in
          let%bind {less; _} = compare ~bit_length x y in
          Boolean.Assert.is_true less

        let lte ~bit_length x y =
          let open Checked in
          let open Let_syntax in
          let%bind {less_or_equal; _} = compare ~bit_length x y in
          Boolean.Assert.is_true less_or_equal

        let gt ~bit_length x y = lt ~bit_length y x

        let gte ~bit_length x y = lte ~bit_length y x

        let non_zero = Checked.assert_non_zero

        let equal x y = Checked.assert_equal ~label:"Checked.Assert.equal" x y

        let not_equal (x : t) (y : t) =
          Checked.with_label "Checked.Assert.not_equal" (non_zero (sub x y))
      end

      let lt_bitstring_value =
        let module Boolean = Checked.Boolean in
        let module Expr = struct
          module Binary = struct
            type 'a t = Lit of 'a | And of 'a * 'a t | Or of 'a * 'a t
          end

          module Nary = struct
            type 'a t = Lit of 'a | And of 'a t list | Or of 'a t list

            let rec of_binary : 'a Binary.t -> 'a t = function
              | Lit x -> Lit x
              | And (x, And (y, t)) -> And [Lit x; Lit y; of_binary t]
              | Or (x, Or (y, t)) -> Or [Lit x; Lit y; of_binary t]
              | And (x, t) -> And [Lit x; of_binary t]
              | Or (x, t) -> Or [Lit x; of_binary t]

            let rec eval =
              let open Checked.Let_syntax in
              function
              | Lit x -> return x
              | And xs -> Checked.List.map xs ~f:eval >>= Boolean.all
              | Or xs -> Checked.List.map xs ~f:eval >>= Boolean.any
          end
        end in
        let rec lt_binary xs ys : Boolean.var Expr.Binary.t =
          match (xs, ys) with
          | [], [] -> Lit Boolean.false_
          | [_x], [false] -> Lit Boolean.false_
          | [x], [true] -> Lit (Boolean.not x)
          | [x1; _x2], [true; false] -> Lit (Boolean.not x1)
          | [_x1; _x2], [false; false] -> Lit Boolean.false_
          | x :: xs, false :: ys -> And (Boolean.not x, lt_binary xs ys)
          | x :: xs, true :: ys -> Or (Boolean.not x, lt_binary xs ys)
          | _ :: _, [] | [], _ :: _ ->
              failwith "lt_bitstring_value: Got unequal length strings"
        in
        fun (xs : Boolean.var Bitstring_lib.Bitstring.Msb_first.t)
            (ys : bool Bitstring_lib.Bitstring.Msb_first.t) ->
          let open Expr.Nary in
          eval
            (of_binary (lt_binary (xs :> Boolean.var list) (ys :> bool list)))

      let field_size_bits =
        List.init Field.size_in_bits ~f:(fun i ->
            Bigint.test_bit field_size (Field.size_in_bits - 1 - i) )
        |> Bitstring_lib.Bitstring.Msb_first.of_list

      let unpack_full x =
        let module Bitstring = Bitstring_lib.Bitstring in
        let open Checked.Let_syntax in
        let%bind res =
          choose_preimage_var x ~length:Field.size_in_bits
          >>| Bitstring.Lsb_first.of_list
        in
        let%map () =
          lt_bitstring_value
            (Bitstring.Msb_first.of_lsb_first res)
            field_size_bits
          >>= Checked.Boolean.Assert.is_true
        in
        res
    end
  end

  module Bitstring_checked = struct
    type t = Checked.Boolean.var list

    let lt_value = Field.Checked.lt_bitstring_value

    let chunk_for_equality (t1 : t) (t2 : t) =
      let chunk_size = Field.size_in_bits - 1 in
      let rec go acc t1 t2 =
        match (t1, t2) with
        | [], [] -> acc
        | _, _ ->
            let t1_a, t1_b = List.split_n t1 chunk_size in
            let t2_a, t2_b = List.split_n t2 chunk_size in
            go ((Cvar1.pack t1_a, Cvar1.pack t2_a) :: acc) t1_b t2_b
      in
      go [] t1 t2

    let equal t1 t2 =
      let open Checked in
      all
        (Core.List.map (chunk_for_equality t1 t2) ~f:(fun (x1, x2) ->
             equal x1 x2 ))
      >>= Boolean.all

    module Assert = struct
      let equal t1 t2 =
        let open Checked in
        Core.List.map (chunk_for_equality t1 t2) ~f:(fun (x1, x2) ->
            Constraint.equal x1 x2 )
        |> assert_all ~label:"Bitstring.Assert.equal"
    end
  end

  let%test_unit "lt_bitstring_value" =
    let gen =
      let open Quickcheck.Generator in
      let open Let_syntax in
      let%bind length = small_positive_int in
      let%map x = list_with_length length bool
      and y = list_with_length length bool in
      (x, y)
    in
    Quickcheck.test gen ~f:(fun (x, y) ->
        let correct_answer = x < y in
        let (), lt =
          Checked.run_and_check
            (Checked.map
               ~f:(As_prover.read Checked.Boolean.typ)
               (Field.Checked.lt_bitstring_value
                  (Bitstring_lib.Bitstring.Msb_first.of_list
                     (List.map ~f:Checked.Boolean.var_of_value x))
                  (Bitstring_lib.Bitstring.Msb_first.of_list y)))
            ()
          |> Or_error.ok_exn
        in
        assert (lt = correct_answer) )

  include Checked

  let generate_keypair = Run.generate_keypair

  let conv f = Run.conv (fun x _ -> f x)

  let prove = Run.prove

  let verify = Run.verify

  let constraint_system = Run.constraint_system

  module R1CS_constraint_system = struct
    include R1CS_constraint_system
  end
end

module Make (Backend : Backend_intf.S) = struct
  module Basic = Make_basic (Backend)
  include Basic
  module Number = Number.Make (Basic)
  module Enumerable = Enumerable.Make (Basic)
end
