module type Intf = sig
  include Snark_intf.Basic

  val state : Runner.state ref
end

type ('field, 'var, 'state) base_intf =
  (module
   Snark_intf.Basic
     with type field = 'field
      and type Var.t = 'var
      and type Runner.state = 'state)

type ('field, 'var) intf =
  (module Intf with type field = 'field and type Var.t = 'var)

module Boolean = struct
  let true_ (type f v) ~(intf : (f, v) intf) =
    let (module I) = intf in
    I.Boolean.true_

  let false_ (type f v) ~(intf : (f, v) intf) =
    let (module I) = intf in
    I.Boolean.false_

  let if_ (type f v) ~(intf : (f, v) intf) b ~then_ ~else_ =
    let (module I) = intf in
    let state, a = I.Runner.run (I.Boolean.if_ b ~then_ ~else_) !I.state in
    I.state := state ;
    a

  let not (type f v) ~(intf : (f, v) intf) b =
    let (module I) = intf in
    I.Boolean.not b

  let ( && ) (type f v) ~(intf : (f, v) intf) b1 b2 =
    let (module I) = intf in
    let state, a = I.Runner.run (I.Boolean.( && ) b1 b2) !I.state in
    I.state := state ;
    a

  let ( || ) (type f v) ~(intf : (f, v) intf) b1 b2 =
    let (module I) = intf in
    let state, a = I.Runner.run (I.Boolean.( || ) b1 b2) !I.state in
    I.state := state ;
    a

  let ( lxor ) (type f v) ~(intf : (f, v) intf) b1 b2 =
    let (module I) = intf in
    let state, a = I.Runner.run (I.Boolean.( lxor ) b1 b2) !I.state in
    I.state := state ;
    a

  let any (type f v) ~(intf : (f, v) intf) bl =
    let (module I) = intf in
    let state, a = I.Runner.run (I.Boolean.any bl) !I.state in
    I.state := state ;
    a

  let all (type f v) ~(intf : (f, v) intf) bl =
    let (module I) = intf in
    let state, a = I.Runner.run (I.Boolean.all bl) !I.state in
    I.state := state ;
    a

  let of_field (type f v) ~(intf : (f, v) intf) f =
    let (module I) = intf in
    let state, a = I.Runner.run (I.Boolean.of_field f) !I.state in
    I.state := state ;
    a

  let var_of_value (type f v) ~(intf : (f, v) intf) b =
    let (module I) = intf in
    I.Boolean.var_of_value b

  let equal (type f v) ~(intf : (f, v) intf) b1 b2 =
    let (module I) = intf in
    let state, a = I.Runner.run (I.Boolean.equal b1 b2) !I.state in
    I.state := state ;
    a

  module Assert = struct
    let ( = ) (type f v) ~(intf : (f, v) intf) b1 b2 =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Boolean.Assert.( = ) b1 b2) !I.state in
      I.state := state ;
      a

    let is_true (type f v) ~(intf : (f, v) intf) b =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Boolean.Assert.is_true b) !I.state in
      I.state := state ;
      a

    let any (type f v) ~(intf : (f, v) intf) bl =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Boolean.Assert.any bl) !I.state in
      I.state := state ;
      a

    let all (type f v) ~(intf : (f, v) intf) bl =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Boolean.Assert.any bl) !I.state in
      I.state := state ;
      a

    let exactly_one (type f v) ~(intf : (f, v) intf) bl =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Boolean.Assert.exactly_one bl) !I.state in
      I.state := state ;
      a
  end
end

module Field = struct
  let size (type f v) ~(intf : (f, v) intf) =
    let (module I) = intf in
    I.Field.size

  let unpack (type f v) ~(intf : (f, v) intf) t =
    let (module I) = intf in
    I.Field.unpack t

  let project (type f v) ~(intf : (f, v) intf) bl =
    let (module I) = intf in
    I.Field.project bl

  module Var = struct
    let to_constant_and_terms (type f v) ~(intf : (f, v) intf) t =
      let (module I) = intf in
      I.Field.Var.to_constant_and_terms t

    let constant (type f v) ~(intf : (f, v) intf) t =
      let (module I) = intf in
      I.Field.Var.constant t

    let to_constant (type f v) ~(intf : (f, v) intf) t =
      let (module I) = intf in
      I.Field.Var.to_constant t

    let linear_combination (type f v) ~(intf : (f, v) intf) l =
      let (module I) = intf in
      I.Field.Var.linear_combination l

    let sum (type f v) ~(intf : (f, v) intf) l =
      let (module I) = intf in
      I.Field.Var.sum l

    let add (type f v) ~(intf : (f, v) intf) x y =
      let (module I) = intf in
      I.Field.Var.add x y

    let sub (type f v) ~(intf : (f, v) intf) x y =
      let (module I) = intf in
      I.Field.Var.sub x y

    let scale (type f v) ~(intf : (f, v) intf) t f =
      let (module I) = intf in
      I.Field.Var.scale t f

    let project (type f v) ~(intf : (f, v) intf) bl =
      let (module I) = intf in
      I.Field.Var.project bl

    let pack (type f v) ~(intf : (f, v) intf) bl =
      let (module I) = intf in
      I.Field.Var.project bl
  end

  module Checked = struct
    let mul (type f v) ~(intf : (f, v) intf) x y =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Field.Checked.mul x y) !I.state in
      I.state := state ;
      a

    let square (type f v) ~(intf : (f, v) intf) x =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Field.Checked.square x) !I.state in
      I.state := state ;
      a

    let div (type f v) ~(intf : (f, v) intf) x y =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Field.Checked.div x y) !I.state in
      I.state := state ;
      a

    let inv (type f v) ~(intf : (f, v) intf) x =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Field.Checked.inv x) !I.state in
      I.state := state ;
      a

    let equal (type f v) ~(intf : (f, v) intf) x y =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Field.Checked.equal x y) !I.state in
      I.state := state ;
      a

    let unpack (type f v) ~(intf : (f, v) intf) x ~length =
      let (module I) = intf in
      let state, a =
        I.Runner.run (I.Field.Checked.unpack x ~length) !I.state
      in
      I.state := state ;
      a

    let unpack_full (type f v) ~(intf : (f, v) intf) x =
      let (module I) = intf in
      let state, a = I.Runner.run (I.Field.Checked.unpack_full x) !I.state in
      I.state := state ;
      a

    let choose_preimage_var (type f v) ~(intf : (f, v) intf) x ~length =
      let (module I) = intf in
      let state, a =
        I.Runner.run (I.Field.Checked.choose_preimage_var x ~length) !I.state
      in
      I.state := state ;
      a

    type 'v comparison_result = {less: 'v; less_or_equal: 'v}

    let compare (type f v) ~(intf : (f, v) intf) ~bit_length x y =
      let (module I) = intf in
      let state, {I.Field.Checked.less; less_or_equal} =
        I.Runner.run (I.Field.Checked.compare ~bit_length x y) !I.state
      in
      I.state := state ;
      {less; less_or_equal}

    let if_ (type f v) ~(intf : (f, v) intf) b ~then_ ~else_ =
      let (module I) = intf in
      let state, a =
        I.Runner.run (I.Field.Checked.if_ b ~then_ ~else_) !I.state
      in
      I.state := state ;
      a

    module Assert = struct
      let lte (type f v) ~(intf : (f, v) intf) ~bit_length x y =
        let (module I) = intf in
        let state, a =
          I.Runner.run (I.Field.Checked.Assert.lte ~bit_length x y) !I.state
        in
        I.state := state ;
        a

      let gte (type f v) ~(intf : (f, v) intf) ~bit_length x y =
        let (module I) = intf in
        let state, a =
          I.Runner.run (I.Field.Checked.Assert.gte ~bit_length x y) !I.state
        in
        I.state := state ;
        a

      let lt (type f v) ~(intf : (f, v) intf) ~bit_length x y =
        let (module I) = intf in
        let state, a =
          I.Runner.run (I.Field.Checked.Assert.lt ~bit_length x y) !I.state
        in
        I.state := state ;
        a

      let gt (type f v) ~(intf : (f, v) intf) ~bit_length x y =
        let (module I) = intf in
        let state, a =
          I.Runner.run (I.Field.Checked.Assert.gt ~bit_length x y) !I.state
        in
        I.state := state ;
        a

      let not_equal (type f v) ~(intf : (f, v) intf) x y =
        let (module I) = intf in
        let state, a =
          I.Runner.run (I.Field.Checked.Assert.not_equal x y) !I.state
        in
        I.state := state ;
        a

      let equal (type f v) ~(intf : (f, v) intf) x y =
        let (module I) = intf in
        let state, a =
          I.Runner.run (I.Field.Checked.Assert.equal x y) !I.state
        in
        I.state := state ;
        a

      let non_zero (type f v) ~(intf : (f, v) intf) x =
        let (module I) = intf in
        let state, a =
          I.Runner.run (I.Field.Checked.Assert.non_zero x) !I.state
        in
        I.state := state ;
        a
    end
  end
end

let assert_ (type f v) ~(intf : (f, v) intf) ?label c =
  let (module I) = intf in
  let state, a = I.Runner.run (I.assert_ ?label c) !I.state in
  I.state := state ;
  a

let assert_all (type f v) ~(intf : (f, v) intf) ?label cl =
  let (module I) = intf in
  let state, a = I.Runner.run (I.assert_all ?label cl) !I.state in
  I.state := state ;
  a

let assert_r1cs (type f v) ~(intf : (f, v) intf) ?label x y z =
  let (module I) = intf in
  let state, a = I.Runner.run (I.assert_r1cs ?label x y z) !I.state in
  I.state := state ;
  a

let assert_square (type f v) ~(intf : (f, v) intf) ?label x y =
  let (module I) = intf in
  let state, a = I.Runner.run (I.assert_square ?label x y) !I.state in
  I.state := state ;
  a

let as_prover (type f v) ~(intf : (f, v) intf) p =
  let (module I) = intf in
  let state, a = I.Runner.run (I.as_prover p) !I.state in
  I.state := state ;
  a

(** TODO: Accept [x] that aren't [Checked.t]s. *)
let with_state (type f v) ~(intf : (f, v) intf) ?and_then p x =
  let (module I) = intf in
  let state, a = I.Runner.run (I.with_state ?and_then p x) !I.state in
  I.state := state ;
  a

let next_auxiliary (type f v) ~(intf : (f, v) intf) =
  let (module I) = intf in
  let state, a = I.Runner.run I.next_auxiliary !I.state in
  I.state := state ;
  a

let request_witness (type f v) ~(intf : (f, v) intf) typ p =
  let (module I) = intf in
  let state, a = I.Runner.run (I.request_witness typ p) !I.state in
  I.state := state ;
  a

let perform (type f v) ~(intf : (f, v) intf) p =
  let (module I) = intf in
  let state, a = I.Runner.run (I.perform p) !I.state in
  I.state := state ;
  a

let exists (type f v) ~(intf : (f, v) intf) ?request ?compute typ =
  let (module I) = intf in
  let state, a = I.Runner.run (I.exists ?request ?compute typ) !I.state in
  I.state := state ;
  a

let unhandled = Request.unhandled

(* NOTE: this is manually implemented! *)
let handle (type f v) ~(intf : (f, v) intf) x h =
  let (module I) = intf in
  let handler = I.Runner.get_handler !I.state in
  I.state := I.Runner.set_handler (Request.Handler.push handler h) !I.state ;
  let a = x ~intf in
  I.state := I.Runner.set_handler handler !I.state ;
  a

(* NOTE: this is manually implemented! *)
let with_label (type f v) ~(intf : (f, v) intf) lbl x =
  let (module I) = intf in
  let stack = I.Runner.get_stack !I.state in
  I.state := I.Runner.set_stack (lbl :: stack) !I.state ;
  let a = x ~intf in
  I.state := I.Runner.set_stack stack !I.state ;
  a

module Perform = struct
  let run (type f v s) ~(intf : (f, v, s) base_intf)
      (k : intf:(f, v) intf -> 'a) state =
    let (module I) = intf in
    let (module I' : Intf
          with type field = f and type Var.t = v and type Runner.state = s) =
      ( module struct
        include I

        let state = ref state
      end )
    in
    let x = k ~intf:(module I') in
    (!I'.state, x)
end
