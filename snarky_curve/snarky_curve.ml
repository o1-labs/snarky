module Field_intf = struct
  module type Basic = sig
    type t

    val ( * ) : t -> t -> t

    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val inv_exn : t -> t

    val negate : t -> t

    val square : t -> t
  end

  module type Constant = sig
    include Basic
  end

  module type Checked = sig
    type field

    type bool

    include Basic

    val if_ : bool -> then_:t -> else_:t -> t

    val scale : t -> field -> t
  end
end

module type Constant_intf = sig
  type field

  type t

  val random : unit -> t

  val to_affine_exn : t -> field * field

  val of_affine : field * field -> t
end

module type Inputs_intf = sig
  module Impl : Snarky.Snark_intf.Run

  module F : sig
    include
      Field_intf.Checked
      with type bool := Impl.Boolean.var
       and type field := Impl.field

    module Constant : Field_intf.Basic

    val assert_square : t -> t -> unit

    val assert_r1cs : t -> t -> t -> unit

    val typ : (t, Constant.t) Impl.Typ.t

    val constant : Constant.t -> t
  end

  module Constant : Constant_intf with type field := F.Constant.t

  module Params : sig
    val one : F.Constant.t * F.Constant.t

    val a : F.Constant.t

    val b : F.Constant.t
  end
end

module Make_checked (Inputs : Inputs_intf) = struct
  open Inputs
  open Impl

  type t = F.t * F.t

  let double ((ax, ay) : t) : t =
    let open F in
    let x_squared = square ax in
    let lambda =
      exists typ
        ~compute:
          As_prover.(
            fun () ->
              let x_squared = read typ x_squared in
              let ay = read typ ay in
              let open F.Constant in
              (x_squared + x_squared + x_squared + Params.a) * inv_exn (ay + ay))
    in
    let bx =
      exists typ
        ~compute:
          As_prover.(
            fun () ->
              let lambda = read typ lambda in
              let ax = read typ ax in
              let open F.Constant in
              square lambda - (ax + ax))
    in
    let by =
      exists typ
        ~compute:
          As_prover.(
            fun () ->
              let lambda = read typ lambda
              and ax = read typ ax
              and ay = read typ ay
              and bx = read typ bx in
              F.Constant.((lambda * (ax - bx)) - ay))
    in
    let two = Field.Constant.of_int 2 in
    assert_r1cs (F.scale lambda two) ay
      (F.scale x_squared (Field.Constant.of_int 3) + F.constant Params.a) ;
    assert_square lambda (bx + F.scale ax two) ;
    assert_r1cs lambda (ax - bx) (by + ay) ;
    (bx, by)

  let add' ~div (ax, ay) (bx, by) : t =
    let open F in
    let lambda = div (by - ay) (bx - ax) in
    let cx =
      exists typ
        ~compute:
          As_prover.(
            fun () ->
              let ax = read typ ax
              and bx = read typ bx
              and lambda = read typ lambda in
              Constant.(square lambda - (ax + bx)))
    in
    (* lambda^2 = cx + ax + bx
            cx = lambda^2 - (ax + bc)
        *)
    assert_square lambda F.(cx + ax + bx) ;
    let cy =
      exists typ
        ~compute:
          As_prover.(
            fun () ->
              let ax = read typ ax
              and ay = read typ ay
              and cx = read typ cx
              and lambda = read typ lambda in
              Constant.((lambda * (ax - cx)) - ay))
    in
    assert_r1cs lambda (ax - cx) (cy + ay) ;
    (cx, cy)

  let add_exn p q = add' ~div:(fun x y -> F.(inv_exn y * x)) p q

  let to_affine_exn x = x

  let negate (x, y) = (x, F.negate y)

  let one =
    let x, y = Params.one in
    F.(constant x, constant y)

  let assert_on_curve (x, y) =
    let open F in
    let x2 = square x in
    let x3 = x2 * x in
    let ax = constant Params.a * x in
    assert_square y (x3 + ax + constant Params.b)

  let typ : (t, Constant.t) Typ.t =
    let unchecked =
      Typ.transport
        Typ.(tuple2 F.typ F.typ)
        ~there:Constant.to_affine_exn ~back:Constant.of_affine
    in
    { unchecked with
      check= (fun t -> make_stateless_checked (fun () -> assert_on_curve t)) }

  open Bitstring_lib.Bitstring

  module Scalar = struct
    type t = Boolean.var Lsb_first.t

    let of_field = Field.unpack_full

    let to_field t = Field.project (Lsb_first.to_list t)
  end

  module type Shifted_intf = sig
    type t

    val zero : t

    val unshift_nonzero : t -> F.t * F.t

    val add : t -> F.t * F.t -> t

    val if_ : Boolean.var -> then_:t -> else_:t -> t
  end

  module Shifted (M : sig
    val shift : t
  end)
  () : Shifted_intf = struct
    type t = F.t * F.t

    let zero = M.shift

    let unshift_nonzero t = add_exn t (negate M.shift)

    let add t pt = add_exn t pt

    let if_ c ~then_:(tx, ty) ~else_:(ex, ey) =
      (F.if_ c ~then_:tx ~else_:ex, F.if_ c ~then_:ty ~else_:ey)
  end

  let shifted () =
    let shift = exists typ ~compute:(fun () -> Constant.random ()) in
    let module S =
      Shifted (struct
          let shift = shift
        end)
        ()
    in
    (module S : Shifted_intf)

  (* This doesn't have great performance because it uses add_exn. Should be optimized *)
  let scale ?init t (c : Boolean.var Bitstring_lib.Bitstring.Lsb_first.t) =
    let (module S) = shifted () in
    let c = Bitstring_lib.Bitstring.Lsb_first.to_list c in
    let rec go i bs0 acc pt =
      match bs0 with
      | [] ->
          acc
      | b :: bs ->
          let acc' =
            let add_pt = S.add acc pt in
            let don't_add_pt = acc in
            S.if_ b ~then_:add_pt ~else_:don't_add_pt
          and pt' = double pt in
          go (i + 1) bs acc' pt'
    in
    let init =
      match init with None -> S.zero | Some init -> S.(add zero init)
    in
    S.unshift_nonzero (go 0 c init t)
end
