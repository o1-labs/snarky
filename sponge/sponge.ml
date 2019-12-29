open Core_kernel
module Intf = Intf

module Params = struct
  include Params

  let bn128 = Constants.params_Bn128

  let mnt4_298 = Constants.params_Mnt4_298

  let mnt4_753 = Constants.params_Mnt4_753

  let bn382_p = Constants.params_Bn382_p

  let bn382_q = Constants.params_Bn382_q
end

module State = Array

let for_ n ~init ~f =
  let rec go i acc = if Int.(i = n) then acc else go (i + 1) (f i acc) in
  go 0 init

module Make_operations (Field : Intf.Field) = struct
  let add_assign ~state i x = state.(i) <- Field.( + ) state.(i) x

  let apply_affine_map (matrix, constants) v =
    let dotv row =
      Array.reduce_exn (Array.map2_exn row v ~f:Field.( * )) ~f:Field.( + )
    in
    let res = Array.map matrix ~f:dotv in
    for i = 0 to Array.length res - 1 do
      res.(i) <- Field.( + ) res.(i) constants.(i)
    done ;
    res

  let copy = Array.copy
end

let m = 3

module Rescue (Inputs : Intf.Inputs.Rescue) = struct
  (*
   We refer below to this paper: https://eprint.iacr.org/2019/426.pdf.

I arrived at this value for the number of rounds in the following way.
As mentioned on page 34, the cost of performing the Grobner basis attack is estimated as

( (n + d) choose d ) ^ omega
where 

- omega is some number which is known to be >= 2
- n = 1 + m*N is the number of variables in the system of equations on page 3
- d is a quantity which they estimate as ((alpha - 1)(m*N + 1) + 1) / 2
- m is the state size, which we can choose
- N is the number of rounds which we can choose

In our case, `alpha = 11`, and I took `m = 3` which is optimal for binary Merkle trees.
Evaluating the above formula with these values and `N = 11` and `omega = 2` yields an attack complexity
of a little over 2^257, which if we take the same factor of 2 security margin as they use in the paper,
gives us a security level of 257/2 ~= 128.

NB: As you can see from the analysis this is really specialized to alpha = 11 and the number of rounds
should be higher for smaller alpha.
*)

  open Inputs
  include Operations
  module Field = Field

  let sbox0, sbox1 = (alphath_root, to_the_alpha)

  let add_block ~state block = Array.iteri block ~f:(add_assign ~state)

  let block_cipher {Params.round_constants; mds} state =
    add_block ~state round_constants.(0) ;
    for_ (2 * rounds) ~init:state ~f:(fun r state ->
        let sbox = if Int.(r mod 2 = 0) then sbox0 else sbox1 in
        Array.map_inplace state ~f:sbox ;
        apply_affine_map (mds, round_constants.(r + 1)) state )
end

module Poseidon (Inputs : Intf.Inputs.Poseidon) = struct
  open Inputs
  include Operations
  module Field = Field

  let half_rounds_full = rounds_full / 2

  let%test "rounds_full" = half_rounds_full * 2 = rounds_full

  let add_block ~state block = Array.iteri block ~f:(add_assign ~state)

  (* Poseidon goes

      ARK_0 -> SBOX -> MDS
   -> ARK_1 -> SBOX -> MDS
   -> ...
   -> ARK_{half_rounds_full - 1} -> SBOX -> MDS
   -> ARK_{half_rounds_full} -> SBOX0 -> MDS
   -> ...
   -> ARK_{half_rounds_full + rounds_partial - 1} -> SBOX0 -> MDS
   -> ARK_{half_rounds_full + rounds_partial} -> SBOX -> MDS
   -> ...
   -> ARK_{half_rounds_full + rounds_partial + half_rounds_full - 1} -> SBOX -> MDS

   It is best to apply the matrix and add the round constants at the same
   time for Marlin constraint efficiency, so that is how this implementation does it.
   Like,

      ARK_0
   -> SBOX -> (MDS -> ARK_1)
   -> SBOX -> (MDS -> ARK_2)
   -> ...
   -> SBOX -> (MDS -> ARK_{half_rounds_full - 1})
   -> SBOX -> (MDS -> ARK_{half_rounds_full})
   -> SBOX0 -> (MDS -> ARK_{half_rounds_full + 1})
   -> ...
   -> SBOX0 -> (MDS -> ARK_{half_rounds_full + rounds_partial - 1})
   -> SBOX0 -> (MDS -> ARK_{half_rounds_full + rounds_partial})
   -> SBOX -> (MDS -> ARK_{half_rounds_full + rounds_partial + 1})
   -> ...
   -> SBOX -> (MDS -> ARK_{half_rounds_full + rounds_partial + half_rounds_full - 1})
   -> SBOX -> MDS
*)
  let block_cipher {Params.round_constants; mds} state =
    let sbox = to_the_alpha in
    let state = ref state in
    add_block ~state:!state round_constants.(0) ;
    for i = 1 to half_rounds_full do
      (* SBOX -> MDS -> ARK *)
      Array.map_inplace !state ~f:sbox ;
      state := apply_affine_map (mds, round_constants.(i)) !state
    done ;
    for i = half_rounds_full + 1 to half_rounds_full + rounds_partial do
      !state.(0) <- sbox !state.(0) ;
      state := apply_affine_map (mds, round_constants.(i)) !state
    done ;
    for
      i = half_rounds_full + rounds_partial + 1
      to rounds_full + rounds_partial - 1
    do
      Array.map_inplace !state ~f:sbox ;
      state := apply_affine_map (mds, round_constants.(i)) !state
    done ;
    Array.map_inplace ~f:sbox !state ;
    apply_affine_map (mds, Array.map !state ~f:(fun _ -> Field.zero)) !state
end

module Make_hash (P : Intf.Permutation) = struct
  open P

  let add_block ~state block = Array.iteri block ~f:(add_assign ~state)

  let sponge perm blocks ~state =
    Array.fold ~init:state blocks ~f:(fun state block ->
        add_block ~state block ; perm state )

  let to_blocks r a =
    let n = Array.length a in
    Array.init
      ((n + r - 1) / r)
      ~f:(fun i ->
        Array.init r ~f:(fun j ->
            let k = (r * i) + j in
            if k < n then a.(k) else Field.zero ) )

  let%test_unit "block" =
    let z = Field.zero in
    [%test_eq: unit array array]
      (Array.map (to_blocks 2 [|z; z; z|]) ~f:(Array.map ~f:ignore))
      [|[|(); ()|]; [|(); ()|]|]

  let r = m - 1

  let update params ~state inputs =
    let state = copy state in
    sponge (block_cipher params) (to_blocks r inputs) ~state

  let digest state = state.(0)

  let initial_state = Array.init m ~f:(fun _ -> Field.zero)

  let hash ?(init = initial_state) params inputs =
    update params ~state:init inputs |> digest
end

module Make_sponge (P : Intf.Permutation) = struct
  open P

  let capacity = 1

  type sponge_state = Absorbed of int | Squeezed of int

  type t =
    { mutable state: Field.t State.t
    ; params: Field.t Params.t
    ; mutable sponge_state: sponge_state }

  let initial_state = Array.init m ~f:(fun _ -> Field.zero)

  let create ?(init = initial_state) params =
    {state= copy init; sponge_state= Absorbed 0; params}

  let rate = m - capacity

  let absorb t x =
    match t.sponge_state with
    | Absorbed n ->
        if n = rate then (
          t.state <- block_cipher t.params t.state ;
          add_assign ~state:t.state 0 x ;
          t.sponge_state <- Absorbed 1 )
        else (
          add_assign ~state:t.state n x ;
          t.sponge_state <- Absorbed (n + 1) )
    | Squeezed _ ->
        add_assign ~state:t.state 0 x ;
        t.sponge_state <- Absorbed 1

  let squeeze t =
    match t.sponge_state with
    | Squeezed n ->
        if n = rate then (
          t.state <- block_cipher t.params t.state ;
          t.sponge_state <- Squeezed 1 ;
          t.state.(0) )
        else (
          t.sponge_state <- Squeezed (n + 1) ;
          t.state.(n) )
    | Absorbed _ ->
        t.state <- block_cipher t.params t.state ;
        t.sponge_state <- Squeezed 1 ;
        t.state.(0)
end

module Make_bit_sponge (Bool : sig
  type t
end) (Field : sig
  type t

  val to_bits : t -> Bool.t list
end)
(S : Intf.Sponge
     with module State := State
      and module Field := Field
      and type digest := Field.t
      and type input := Field.t) =
struct
  type t =
    { underlying: S.t
          (* TODO: Have to be careful about these bits. They aren't perfectly uniform. *)
    ; mutable last_squeezed: Bool.t list }

  let create ?init params =
    {underlying= S.create ?init params; last_squeezed= []}

  let absorb t x =
    S.absorb t.underlying x ;
    t.last_squeezed <- []

  let rec squeeze t ~length =
    if List.length t.last_squeezed >= length then (
      let digest, remaining = List.split_n t.last_squeezed length in
      t.last_squeezed <- remaining ;
      digest )
    else
      let x = S.squeeze t.underlying in
      t.last_squeezed <- t.last_squeezed @ Field.to_bits x ;
      squeeze ~length t
end
