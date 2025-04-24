open Core_kernel
module Runner = Checked_runner

let set_eval_constraints b = Runner.eval_constraints := b

module Make
    (Backend : Backend_intf.S)
    (Types : Types.Types
               with type field = Backend.Field.t
                and type field_var = Backend.Cvar.t)
    (Checked : Checked_intf.Extended
                 with module Types := Types
                 with type run_state = Backend.Run_state.t
                  and type constraint_ = Backend.Constraint.t)
    (As_prover : As_prover.Intf with module Types := Types)
    (Typ : Snark_intf.Typ_intf
             with type field := Backend.Field.t
              and type field_var := Backend.Cvar.t
              and type 'field checked_unit := unit Types.Checked.t
              and type ('var, 'value, 'aux) typ' :=
               ('var, 'value, 'aux) Types.Typ.typ'
              and type ('var, 'value) typ := ('var, 'value) Types.Typ.typ)
    (Runner : Runner.S
                with module Types := Types
                with type constr := Backend.Constraint.t option
                 and type r1cs := Backend.R1CS_constraint_system.t
                 and type run_state = Backend.Run_state.t) =
struct
  open Backend

  open Runners.Make (Backend) (Types) (Checked) (As_prover) (Runner)

  open (
    Checked :
      Checked_intf.Extended
        with module Types := Types
        with type constraint_ := Constraint.t )

  (* [equal_constraints z z_inv r] asserts that
     if z = 0 then r = 1, or
     if z <> 0 then r = 0 and z * z_inv = 1
  *)
  let equal_constraints (z : Cvar.t) (z_inv : Cvar.t) (r : Cvar.t) =
    Checked.assert_all
      [ Constraint.r1cs z_inv z Cvar.(constant Field.one - r)
      ; Constraint.r1cs r z (Cvar.constant Field.zero)
      ]

  (* [equal_vars z] computes [(r, z_inv)] that satisfy the constraints in
     [equal_constraints z z_inv r].

     In particular, [r] is [1] if [z = 0] and [0] otherwise.
  *)
  let equal_vars (z : Cvar.t) : (Field.t * Field.t) As_prover.t =
    let open As_prover.Let_syntax in
    let%map z = As_prover.read_var z in
    if Field.equal z Field.zero then (Field.one, Field.zero)
    else (Field.zero, Field.inv z)

  let constant (Typ typ : _ Typ.t) x =
    let fields, aux = typ.value_to_fields x in
    let field_vars = Array.map fields ~f:(fun x -> Cvar.constant x) in
    typ.var_of_fields (field_vars, aux)

  let equal (x : Cvar.t) (y : Cvar.t) : Cvar.t Boolean.t Checked.t =
    match (Cvar.to_constant x, Cvar.to_constant y) with
    | Some x, Some y ->
        Checked.return
          (Boolean.Unsafe.create
             (Cvar.constant
                (if Field.equal x y then Field.one else Field.zero) ) )
    | _ ->
        let z = Cvar.(x - y) in
        let%bind r, inv =
          Checked.exists Typ.(tuple2 field field) ~compute:(equal_vars z)
        in
        let%map () = equal_constraints z inv r in
        Boolean.Unsafe.create r

  let mul ?(label = "Checked.mul") (x : Cvar.t) (y : Cvar.t) =
    match (Cvar.to_constant x, Cvar.to_constant y) with
    | Some x, Some y ->
        return (Cvar.constant (Field.mul x y))
    | Some x, _ ->
        return (Cvar.scale y x)
    | _, Some y ->
        return (Cvar.scale x y)
    | _, _ ->
        with_label label (fun () ->
            let open Let_syntax in
            let%bind z =
              Checked.exists Typ.field
                ~compute:As_prover.(map2 (read_var x) (read_var y) ~f:Field.mul)
            in
            let%map () = assert_r1cs x y z in
            z )

  let square ?(label = "Checked.square") (x : Cvar.t) =
    match Cvar.to_constant x with
    | Some x ->
        return (Cvar.constant (Field.square x))
    | _ ->
        with_label label (fun () ->
            let open Let_syntax in
            let%bind z =
              exists Typ.field
                ~compute:As_prover.(map (read_var x) ~f:Field.square)
            in
            let%map () = assert_square x z in
            z )

  (* We get a better stack trace by failing at the call to is_satisfied, so we
     put a bogus value for the inverse to make the constraint system unsat if
     x is zero. *)
  let inv ?(label = "Checked.inv") (x : Cvar.t) =
    match Cvar.to_constant x with
    | Some x ->
        return (Cvar.constant (Field.inv x))
    | _ ->
        with_label label (fun () ->
            let open Let_syntax in
            let%bind x_inv =
              exists Typ.field
                ~compute:
                  As_prover.(
                    map (read_var x) ~f:(fun x ->
                        if Field.(equal zero x) then Field.zero
                        else Backend.Field.inv x ))
            in
            let%map () = assert_r1cs x x_inv (Cvar.constant Field.one) in
            x_inv )

  let div ?(label = "Checked.div") (x : Cvar.t) (y : Cvar.t) =
    match (Cvar.to_constant x, Cvar.to_constant y) with
    | Some x, Some y ->
        return (Cvar.constant (Field.( / ) x y))
    | _ ->
        with_label label (fun () ->
            let open Let_syntax in
            let%bind y_inv = inv y in
            mul x y_inv )

  let%snarkydef_ if_ (b : Cvar.t Boolean.t) ~(then_ : Cvar.t) ~(else_ : Cvar.t)
      =
    let open Let_syntax in
    (* r = e + b (t - e)
       r - e = b (t - e)
    *)
    let b = (b :> Cvar.t) in
    match Cvar.to_constant b with
    | Some b ->
        if Field.(equal b one) then return then_ else return else_
    | _ -> (
        match (Cvar.to_constant then_, Cvar.to_constant else_) with
        | Some t, Some e ->
            return Cvar.((t * b) + (e * (constant Field0.one - b)))
        | _, _ ->
            let%bind r =
              exists Typ.field
                ~compute:
                  (let open As_prover in
                  let open Let_syntax in
                  let%bind b = read_var b in
                  read Typ.field
                    (if Field.equal b Field.one then then_ else else_))
            in
            let%map () = assert_r1cs b Cvar.(then_ - else_) Cvar.(r - else_) in
            r )

  let%snarkydef_ assert_non_zero (v : Cvar.t) =
    let open Let_syntax in
    let%map _ = inv v in
    ()

  module Boolean = struct
    open Boolean.Unsafe

    type var = Cvar.t Boolean.t

    type value = bool

    let true_ : var = create (Cvar.constant Field.one)

    let false_ : var = create (Cvar.constant Field.zero)

    let not (x : var) : var = create Cvar.((true_ :> Cvar.t) - (x :> Cvar.t))

    let if_ b ~(then_ : var) ~(else_ : var) =
      map ~f:create (if_ b ~then_:(then_ :> Cvar.t) ~else_:(else_ :> Cvar.t))

    (* This is unused for now as we are not using any square constraint system based
       backends. *)
    let _and_for_square_constraint_systems (x : var) (y : var) =
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
        assert_square x_plus_y Cvar.((Field.of_int 2 * z) + x_plus_y)
      in
      create z

    let ( && ) (x : var) (y : var) : var Checked.t =
      Checked.map ~f:create (mul (x :> Cvar.t) (y :> Cvar.t))

    let ( &&& ) = ( && )

    let ( || ) x y =
      let open Let_syntax in
      let%map both_false = (not x) && not y in
      not both_false

    let ( ||| ) = ( || )

    let any = function
      | [] ->
          return false_
      | [ b1 ] ->
          return b1
      | [ b1; b2 ] ->
          b1 || b2
      | bs ->
          let open Let_syntax in
          let%map all_zero =
            equal (Cvar.sum (bs :> Cvar.t list)) (Cvar.constant Field.zero)
          in
          not all_zero

    let all = function
      | [] ->
          return true_
      | [ b1 ] ->
          return b1
      | [ b1; b2 ] ->
          b1 && b2
      | bs ->
          equal
            (Cvar.constant (Field.of_int (List.length bs)))
            (Cvar.sum (bs :> Cvar.t list))

    let to_constant (b : var) =
      Option.map (Cvar.to_constant (b :> Cvar.t)) ~f:Field.(equal one)

    let var_of_value b = if b then true_ else false_

    let typ : (var, value) Typ.t =
      let (Typ typ) =
        Typ.field
        |> Typ.transport
             ~there:(function true -> Field.one | false -> Field.zero)
             ~back:(fun x -> if Field.equal x Field.zero then false else true)
        |> Typ.transport_var
             ~there:(fun (b : var) -> (b :> Cvar.t))
             ~back:create
      in
      Typ
        { typ with
          check = (fun v -> Checked.assert_ (Constraint.boolean (v :> Cvar.t)))
        }

    let typ_unchecked : (var, value) Typ.t =
      let (Typ typ) = typ in
      Typ { typ with check = (fun _ -> Checked.return ()) }

    let%test_unit "all" =
      let gen =
        let open Quickcheck.Generator in
        let open Let_syntax in
        let%bind length = small_positive_int in
        list_with_length length bool
      in
      Quickcheck.test gen ~sexp_of:[%sexp_of: bool list] ~f:(fun x ->
          let r =
            run_and_check
              (Checked.map ~f:(As_prover.read typ)
                 (all (List.map ~f:(constant typ_unchecked) x)) )
            |> Or_error.ok_exn
          in
          [%test_eq: bool] r (List.for_all x ~f:Fn.id) )

    let ( lxor ) b1 b2 =
      match (to_constant b1, to_constant b2) with
      | Some b1, Some b2 ->
          return (constant typ (Caml.not (Bool.equal b1 b2)))
      | Some true, None ->
          return (not b2)
      | None, Some true ->
          return (not b1)
      | Some false, None ->
          return b2
      | None, Some false ->
          return b1
      | None, None ->
          (* (1 - 2 a) (1 - 2 b) = 1 - 2 c
             1 - 2 (a + b) + 4 a b = 1 - 2 c
             - 2 (a + b) + 4 a b = - 2 c
             (a + b) - 2 a b = c
             2 a b = a + b - c
          *)
          let open Let_syntax in
          let%bind res =
            exists typ_unchecked
              ~compute:
                As_prover.(
                  map2 ~f:Bool.( <> ) (read typ_unchecked b1)
                    (read typ_unchecked b2))
          in
          let%map () =
            let a = (b1 :> Cvar.t) in
            let b = (b2 :> Cvar.t) in
            let c = (res :> Cvar.t) in
            let open Cvar in
            assert_r1cs (a + a) b (a + b - c)
          in
          res

    module Array = struct
      let num_true (bs : var array) =
        Array.fold bs ~init:(Cvar.constant Field.zero) ~f:(fun x y ->
            Cvar.add x (y :> Cvar.t) )

      let any = function
        | [||] ->
            return false_
        | [| b1 |] ->
            return b1
        | [| b1; b2 |] ->
            b1 || b2
        | bs ->
            let open Let_syntax in
            let%map all_zero = equal (num_true bs) (Cvar.constant Field.zero) in
            not all_zero

      let all = function
        | [||] ->
            return true_
        | [| b1 |] ->
            return b1
        | [| b1; b2 |] ->
            b1 && b2
        | bs ->
            equal (Cvar.constant (Field.of_int (Array.length bs))) (num_true bs)

      module Assert = struct
        let any bs = assert_non_zero (num_true bs)

        let all bs =
          assert_equal (num_true bs)
            (Cvar.constant (Field.of_int (Array.length bs)))
      end
    end

    let equal (a : var) (b : var) = a lxor b >>| not

    let of_field x =
      let open Let_syntax in
      let%map () = assert_ (Constraint.boolean x) in
      create x

    module Unsafe = struct
      let of_cvar (t : Cvar.t) : var = create t
    end

    module Assert = struct
      let ( = ) (x : var) (y : var) = assert_equal (x :> Cvar.t) (y :> Cvar.t)

      let is_true (v : var) = v = true_

      let%snarkydef_ any (bs : var list) =
        assert_non_zero (Cvar.sum (bs :> Cvar.t list))

      let%snarkydef_ all (bs : var list) =
        assert_equal
          (Cvar.sum (bs :> Cvar.t list))
          (Cvar.constant (Field.of_int (List.length bs)))

      let%snarkydef_ exactly_one (bs : var list) =
        assert_equal (Cvar.sum (bs :> Cvar.t list)) (Cvar.constant Field.one)
    end

    module Expr = struct
      type t = Var of var | And of t list | Or of t list | Not of t

      let rec eval t =
        let open Let_syntax in
        match t with
        | Not t ->
            eval t >>| not
        | Var v ->
            return v
        | And ts ->
            Checked.all (List.map ~f:eval ts) >>= all
        | Or ts ->
            Checked.all (List.map ~f:eval ts) >>= any

      let assert_ t = eval t >>= Assert.is_true

      let ( ! ) v = Var v

      let ( && ) x y = And [ x; y ]

      let ( &&& ) = ( && )

      let ( || ) x y = Or [ x; y ]

      let ( ||| ) = ( || )

      let not t = Not t

      let any xs = Or xs

      let all xs = And xs
    end
  end
end
