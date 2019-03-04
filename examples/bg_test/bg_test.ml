open Core_kernel
open Snarky
open Snark

let div_unsafe (type f) ~m:((module I) : f m) x y =
  let open I in
  let z =
    exists Field.typ
      ~compute:
        As_prover.(
          map2 (read_var x) (read_var y) ~f:Field.Constant.Infix.( / ))
  in
  assert_r1cs z y x ; z

module Curve = struct
  type 'f params = {a: 'f; b: 'f}

  let typ (type f) ~m:((module I) : f m) = I.Typ.(tuple2 field field)

  let double (type f) ~m:((module I) : f m) ~params (ax, ay) =
    let open I in
    let x_squared = Field.square ax in
    let lambda =
      exists Field.typ
        ~compute:
          As_prover.(
            Let_syntax.(
              let%map x_squared = read_var x_squared and ay = read_var ay in
              Field.Constant.Infix.(
                (x_squared + x_squared + x_squared + params.a) / (ay + ay))))
    in
    let bx =
      exists Field.typ
        ~compute:
          As_prover.(
            Let_syntax.(
              let%map lambda = read_var lambda and ax = read_var ax in
              Field.Constant.(Infix.(square lambda - (ax + ax)))))
    in
    let by =
      exists Field.typ
        ~compute:
          (let open As_prover in
          let open Let_syntax in
          let%map lambda = read_var lambda
          and ax = read_var ax
          and ay = read_var ay
          and bx = read_var bx in
          Field.Constant.Infix.((lambda * (ax - bx)) - ay))
    in
    let open Field in
    assert_r1cs (lambda + lambda) ay
      ((of_int 3 * x_squared) + constant params.a) ;
    assert_square lambda (bx + ax + ax) ;
    assert_r1cs lambda (ax - bx) (by + ay) ;
    (bx, by)

  let add' (type f) ~div ~(m : f m) (ax, ay) (bx, by) =
    let (module I) = m in
    let open I in
    let lambda = div ~m Field.(by - ay) Field.(bx - ax) in
    let cx =
      exists Field.typ
        ~compute:
          As_prover.(
            Let_syntax.(
              let%map lambda = read_var lambda
              and s = read_var Field.(ax + bx) in
              Field.Constant.(Infix.(square lambda - s)))
          )
    in
    assert_square lambda Field.(cx + ax + bx) ;
    let cy =
      exists Field.typ
        ~compute:
          As_prover.(
            Let_syntax.(
              let%map lambda = read_var lambda
              and ax = read_var ax
              and cx = read_var cx
              and ay = read_var ay in
              Field.Constant.Infix.((lambda * (ax - cx)) - ay)))
    in
    I.assert_r1cs lambda Field.(ax - cx) Field.(cy + ay) ;
    (cx, cy)

  let choose (type f) ~m:((module I) : f m) b (x0, x1) (y0, y1) =
    let open I.Field in
    (if_ b ~then_:x0 ~else_:x1, if_ b ~then_:y0 ~else_:y1)

  let scale ~m ~params pt bits ~init =
    let ( + ) = add' ~div:div_unsafe ~m in
    let rec go two_to_the_i acc = function
      | [] -> acc
      | [b] -> choose ~m b (two_to_the_i + acc) acc
      | b :: bs ->
          let acc = choose ~m b (two_to_the_i + acc) acc in
          go (double ~m ~params two_to_the_i) acc bs
    in
    go pt init bits

  let add_exn ~m p q = add' ~div:(fun (type f) ~m:((module I) : f m) -> I.Field.(/)) ~m p q

  module Assert = struct
    let equal (type f) ~m:((module I): f m) (x1, y1) (x2, y2) =
      I.Field.Assert.equal x1 x2;
      I.Field.Assert.equal y1 y2
  end
end

module Bitstring = struct
  type t = bool list
  type 'f var = 'f Boolean.t list

  let random ~length =
    List.init length  ~f:(fun _ -> Random.bool ())

  let typ ~length (type f) ~m:((module I) : f m) =
    I.Typ.list ~length I.Boolean.typ
end

module Scalar = struct
  include Bitstring
  let length = 256
  let random () = random ~length
  let typ (type f) ~(m : f m) = typ ~length ~m
end

module UInt64 = struct
  include Bitstring
  let length = 64
  let random () = random ~length
  let typ (type f) ~(m : f m) = typ ~length ~m
end

type _ Request.t +=
  | Randomness : Scalar.t Request.t
  | Amount     : UInt64.t Request.t

module Inner = Snark.Make(Backends.Mnt6.Default)
module M = Snark.Run.Make(Backends.Mnt4.Default)
module G = Backends.Mnt6.G1

let constant_point (type f) ~m:((module I) : f m) p =
  let x, y = Backends.Mnt6.G1.to_affine_coordinates p in
  M.Field.(constant x, constant y)

let m : M.field m = (module M)

let printi n = Core.printf "%d\n%!" n

let () =
  let g1 = G.random () in
  let g2 = G.random () in
  let res =
    let actual () =
      let module M = Snark.Run.Make(Backends.Mnt4.Default) in
      let m : M.field m = (module M) in
      Curve.add_exn ~m
        (constant_point ~m g1)
        (constant_point ~m g2)
    in
    printi 1;
    let c =
      fun x ->
        M.As_prover.read
          (Curve.typ ~m)
          (actual ())
          x
    in
    printi 2;
    (* Segfaults here *)
    M.run_and_check c
    |> Or_error.ok_exn
  in
  printi 3;
  [%test_eq: M.Field.Constant.t * M.Field.Constant.t]
    res
    (G.to_affine_coordinates (G.add g1 g2))

let init = G.random ()
let g = G.one
let h = G.random ()

let params : _ Curve.params = 
  { a = Libsnark.Curves.Mnt6.G1.Coefficients.a
  ; b = Libsnark.Curves.Mnt6.G1.Coefficients.b
  }

let commit
      ~amount
      ~randomness
  =
  let amount_g =
    Curve.scale ~m ~params
      ~init:(constant_point ~m init)
      (constant_point ~m g)
      amount
  in
  Curve.scale ~m ~params ~init:amount_g
    (constant_point ~m h)
    randomness

let commit_unchecked ~amount ~randomness =
  let open G in
  init
  +
  scale_field
    g
    (Inner.Field.project amount)
  +
  scale_field
    h
    (Inner.Field.project randomness)

let main commitment () =
  let open M in
  let amount =
    exists (UInt64.typ ~m)
      ~request:(As_prover.return Amount)
  in
  let randomness =
    exists (Scalar.typ ~m)
      ~request:(As_prover.return Randomness)
  in
  Curve.Assert.equal ~m
    (commit ~amount ~randomness) 
    commitment

(* Our proof system will prove the following:

   public-input: three group elements (g, h, c)
*)

let proof_system =
  M.Proof_system.create
    ~public_input:[ Curve.typ ~m ]
    main

(* Problems:
   - the run_checked function is not usable.
   - handler should do create_single and such inside
*)

let proof, input =
  let randomness = Scalar.random () in
  let amount = UInt64.random () in
  let handler = 
    (fun (With { request; respond }) ->
        match request with
        | Randomness -> respond (Provide randomness)
        | Amount -> respond  (Provide amount)
        | _ -> failwith "Unhandled request")
    |> Request.Handler.create_single 
    |> Request.Handler.(push fail)
  in
  let commitment =G.to_affine_coordinates (commit_unchecked ~amount ~randomness) in
  (* This doesn't work because the "public_input" type on
     proof_system needs to be polymorphic in the result type *)
  Snark0.set_eval_constraints true;
  (M.Proof_system.prove
    proof_system
    ~handler
    ~public_input:[commitment]
  , commitment)

let () =
  assert (M.Proof_system.verify ~public_input:[input] proof_system proof)

