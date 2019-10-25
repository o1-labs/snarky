type alpha = [`_5 | `_11]

let alpha_of_curve = function Curve.Bn128 -> `_5

let params_of_curve : Curve.t -> string Sponge.Params.t = function
  | Bn128 ->
      Sponge.Params.bn128

module Poseidion_rounds = struct
  module type S = sig
    val rounds_full : int

    val rounds_partial : int
  end

  type t = (module S)

  let alpha_5 : t =
    ( module struct
      let rounds_full = 8

      let rounds_partial = 83
    end )

  let alpha_11 : t =
    ( module struct
      let rounds_full = 8

      let rounds_partial = 33
    end )

  let of_alpha = function `_5 -> alpha_5 | `_11 -> alpha_11
end

module Make
    (Impl : Snarky.Snark_intf.Run) (C : sig
        val curve : Curve.t
    end) =
struct
  open Core_kernel
  open Impl

  module Rounds = (val Poseidion_rounds.of_alpha (alpha_of_curve C.curve))

  module Inputs = struct
    include Rounds

    module Field = struct
      (* The linear combinations involved in computing Poseidon do not involve very many
    variables, but if they are represented as arithmetic expressions (that is, "Cvars"
    which is what Field.t is under the hood) the expressions grow exponentially in
    in the number of rounds. Thus, we compute with Field elements represented by
    a "reduced" linear combination. That is, a coefficient for each variable and an
    constant term.
  *)
      type t = Field.Constant.t Int.Map.t * Field.Constant.t

      let to_cvar ((m, c) : t) : Field.t =
        Map.fold m ~init:(Field.constant c) ~f:(fun ~key ~data acc ->
            let x =
              let v = Snarky.Cvar.Var key in
              if Field.Constant.equal data Field.Constant.one then v
              else Scale (data, v)
            in
            match acc with
            | Constant c when Field.Constant.equal Field.Constant.zero c ->
                x
            | _ ->
                Add (x, acc) )

      let constant c = (Int.Map.empty, c)

      let of_cvar (x : Field.t) =
        match x with
        | Constant c ->
            constant c
        | Var v ->
            (Int.Map.singleton v Field.Constant.one, Field.Constant.zero)
        | x ->
            let c, ts = Field.to_constant_and_terms x in
            ( Int.Map.of_alist_reduce
                (List.map ts ~f:(fun (f, v) -> (Var.index v, f)))
                ~f:Field.Constant.add
            , Option.value ~default:Field.Constant.zero c )

      let ( + ) (t1, c1) (t2, c2) =
        ( Map.merge t1 t2 ~f:(fun ~key:_ t ->
              match t with
              | `Left x ->
                  Some x
              | `Right y ->
                  Some y
              | `Both (x, y) ->
                  Some Field.Constant.(x + y) )
        , Field.Constant.add c1 c2 )

      let ( * ) (t1, c1) (t2, c2) =
        assert (Int.Map.is_empty t1) ;
        (Map.map t2 ~f:(Field.Constant.mul c1), Field.Constant.mul c1 c2)

      let zero = constant Field.Constant.zero
    end

    let to_the_alpha x =
      let open Impl.Field in
      let zero = square in
      let one a = square a * x in
      let one' = x in
      one' |> zero |> one

    let to_the_alpha x = Field.of_cvar (to_the_alpha (Field.to_cvar x))

    module Operations = Sponge.Make_operations (Field)
  end

  include Sponge.Make (Sponge.Poseidon (Inputs))

  type t = Field.t

  let params_constant =
    Sponge.Params.map (params_of_curve C.curve) ~f:Field.Constant.of_string

  let params = Sponge.Params.map params_constant ~f:Inputs.Field.constant

  let hash xs =
    hash params (Array.map xs ~f:Inputs.Field.of_cvar) |> Inputs.Field.to_cvar

  module Constant = struct
    type t = Field.Constant.t

    module Inputs = struct
      include Rounds
      module Field = Field.Constant

      let alpha = 5

      let to_the_alpha x =
        let open Field in
        let res = square x in
        res *= res ; (* x^4 *)
                     res *= x ; res

      module Operations = struct
        let apply_matrix rows v =
          Array.map rows ~f:(fun row ->
              let open Field in
              let res = zero + zero in
              Array.iteri row ~f:(fun i r -> res += (r * v.(i))) ;
              res )

        let add_block ~state block =
          Array.iteri block ~f:(fun i b ->
              let open Field in
              state.(i) += b )

        let copy a = Array.map a ~f:(fun x -> Field.(x + zero))
      end
    end

    include Sponge.Make (Sponge.Poseidon (Inputs))

    let hash xs = hash params_constant xs
  end
end
