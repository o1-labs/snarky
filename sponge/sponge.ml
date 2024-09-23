open Core_kernel
module Intf = Intf
module Default = Default_sponge

module Params = struct
  include Params

  let bn128 = Constants.params_Bn128

  let mnt4_298 = Constants.params_Mnt4_298

  let mnt4_753 = Constants.params_Mnt4_753

  let bn382_p = Constants.params_Bn382_p

  let bn382_q = Constants.params_Bn382_q

  let tweedle_p = Constants.params_Tweedle_p

  let tweedle_q = Constants.params_Tweedle_q

  let pasta_p_legacy = Constants.params_Pasta_p_legacy

  let pasta_q_legacy = Constants.params_Pasta_q_legacy

  let pasta_p_kimchi = Constants.params_Pasta_p_kimchi

  let pasta_q_kimchi = Constants.params_Pasta_q_kimchi
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

module Bn382_inputs (Field : Intf.Field_mutable) = struct
  let rounds_full = 8

  let initial_ark = true

  let rounds_partial = 30

  module Field = Field

  let alpha = 17

  (* alpha = 17 *)
  let to_the_alpha x =
    let open Field in
    let res = square x in
    Mutable.square res ;
    (* x^4 *)
    Mutable.square res ;
    (* x^8 *)
    Mutable.square res ;
    (* x^16 *)
    res *= x ;
    res

  module Operations = struct
    let add_assign ~state i x = Field.(state.(i) += x)

    (* Sparse pseudo-MDS matrix *)
    let apply_affine_map (_rows, c) v =
      let open Field in
      let res = [| v.(0) + v.(2); v.(0) + v.(1); v.(1) + v.(2) |] in
      Array.iteri res ~f:(fun i ri -> ri += c.(i)) ;
      res

    let copy a = Array.map a ~f:(fun x -> Field.(x + zero))
  end
end

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

For the MNT curves, `alpha = 11`, and I took `m = 3` which is optimal for binary Merkle trees.
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

  let block_cipher { Params.round_constants; mds } state =
    add_block ~state round_constants.(0) ;
    for_ (2 * rounds) ~init:state ~f:(fun r state ->
        let sbox = if Int.(r mod 2 = 0) then sbox0 else sbox1 in
        Array.map_inplace state ~f:sbox ;
        apply_affine_map (mds, round_constants.(r + 1)) state )

  let update =
    let open Default_sponge.F (Field) in
    Fn.compose (sponge ~add_assign) block_cipher

  let update_batch params ~rate =
    List.map ~f:(fun (`State state, input) ->
        let state = copy state in
        update params ~rate ~state input )
end

module Poseidon (Inputs : Intf.Inputs.Poseidon) = struct
  open Inputs
  include Operations
  module Field = Field

  let first_half_rounds_full = rounds_full / 2

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
     -> SBOX -> MDS ->* ARK_{half_rounds_full + rounds_partial + half_rounds_full}

      *this last round is a deviation from standard poseidon made for efficiency reasons.
       clearly it does not impact security to add round constants
  *)
  let block_cipher { Params.round_constants; mds } state =
    let sbox = to_the_alpha in
    let state = ref state in
    let constant_offset =
      if initial_ark then (
        add_block ~state:!state round_constants.(0) ;
        1 )
      else 0
    in
    let range =
      (constant_offset, constant_offset + first_half_rounds_full - 1)
    in
    for i = fst range to snd range do
      (* SBOX -> MDS -> ARK *)
      Array.map_inplace !state ~f:sbox ;
      state := apply_affine_map (mds, round_constants.(i)) !state
    done ;
    let range = (snd range + 1, snd range + rounds_partial) in
    for i = fst range to snd range do
      !state.(0) <- sbox !state.(0) ;
      state := apply_affine_map (mds, round_constants.(i)) !state
    done ;
    let second_half_rounds_full = rounds_full - first_half_rounds_full in
    let range = (snd range + 1, snd range + second_half_rounds_full) in
    for i = fst range to snd range do
      Array.map_inplace !state ~f:sbox ;
      state := apply_affine_map (mds, round_constants.(i)) !state
    done ;
    !state

  let update =
    let open Default_sponge.F (Field) in
    Fn.compose (sponge ~add_assign) block_cipher

  let update_batch params ~rate =
    List.map ~f:(fun (`State state, input) ->
        let state = copy state in
        update params ~rate ~state input )
end

module Make_hash (P : Intf.Permutation) = struct
  open P

  let state_size = m

  let rate = state_size - 1

  (* takes an array of field elements, and spread them into blocks/arrays that can contain [rate] fied elements *)
  let update params ~state inputs =
    let state = copy state in
    update params inputs ~rate ~state

  let digest state = state.(0)

  let initial_state = Array.init state_size ~f:(fun _ -> Field.zero)

  let hash ?(init = initial_state) params inputs =
    update params ~state:init inputs |> digest

  let hash_batch params =
    Fn.compose (List.map ~f:digest) @@ update_batch params ~rate
end

type sponge_state = Absorbed of int | Squeezed of int [@@deriving sexp]

type 'f t =
  { mutable state : 'f State.t
  ; params : 'f Params.t
  ; mutable sponge_state : sponge_state
  ; id : int
  }

let id = ref (-1)

let make ~state ~params ~sponge_state =
  incr id ;
  { state; params; sponge_state; id = !id }

module Make_sponge (P : Intf.Permutation) = struct
  open P

  let make = make

  let capacity = 1

  type sponge_state = Absorbed of int | Squeezed of int [@@deriving sexp]

  type nonrec t = Field.t t

  let state { state; _ } = copy state

  let initial_state = Array.init m ~f:(fun _ -> Field.zero)

  let create ?(init = initial_state) params =
    make ~state:(copy init) ~sponge_state:(Absorbed 0) ~params

  let copy { state; params; sponge_state; id } =
    { state = copy state; params; sponge_state; id }

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
    (* to prevent giving a reference to the internal state *)
    let copy x = (P.copy [| x |]).(0) in
    match t.sponge_state with
    | Squeezed n ->
        if n = rate then (
          t.state <- block_cipher t.params t.state ;
          t.sponge_state <- Squeezed 1 ;
          copy t.state.(0) )
        else (
          t.sponge_state <- Squeezed (n + 1) ;
          copy t.state.(n) )
    | Absorbed _ ->
        t.state <- block_cipher t.params t.state ;
        t.sponge_state <- Squeezed 1 ;
        copy t.state.(0)
end

module Make_debug_sponge (P : sig
  include Intf.Permutation

  module Circuit : Snarky_backendless.Snark_intf.Run

  val sponge_name : string

  val debug_helper_fn : Field.t -> string
end) =
struct
  include Make_sponge (P)

  (* In sponge debug mode, prints a standard sponge debug line, otherwise does nothing.
     Note: standard sponge debug line must match the output of Kimchi's sponge debug mode *)
  let debug (operation : string) (sponge : t) (input : P.Field.t option) =
    match Sys.getenv_opt P.sponge_name with
    | Some s -> (
        match String.lowercase s with
        | "t" | "1" | "true" ->
            P.Circuit.as_prover (fun () ->
                (* Convert sponge_state to match Rust style debug string *)
                let sponge_state =
                  match sponge.sponge_state with
                  | Absorbed n ->
                      Printf.sprintf "Absorbed(%d)" n
                  | Squeezed n ->
                      Printf.sprintf "Squeezed(%d)" n
                in
                (* Print debug header, operation and sponge_state *)
                Format.eprintf "debug_sponge: %s%d %s state %s" P.sponge_name
                  sponge.id operation sponge_state ;
                (* Print sponge's state array *)
                Array.iter sponge.state ~f:(fun fe ->
                    Format.eprintf " %s" (P.debug_helper_fn fe) ) ;
                Format.eprintf "@." ;
                (* Print optional input *)
                match input with
                | Some input ->
                    Format.eprintf "debug_sponge: %s%d %s input %s@."
                      P.sponge_name sponge.id operation
                      (P.debug_helper_fn input)
                | None ->
                    () )
        | _ ->
            () )
    | None ->
        ()

  let make ~state ~params ~sponge_state =
    let t = make ~state ~params ~sponge_state in
    debug "make" t None ; t

  let absorb t x = debug "absorb" t (Some x) ; absorb t x

  let squeeze t = debug "squeeze" t None ; squeeze t
end

module Bit_sponge = struct
  type ('s, 'bool) t =
    { underlying : 's
          (* TODO: Have to be careful about these bits. They aren't perfectly uniform. *)
    ; mutable last_squeezed : 'bool list
    }

  let map (type a b) t ~(f : a -> b) : (b, _) t =
    { t with underlying = f t.underlying }

  let make ?(last_squeezed = []) underlying = { underlying; last_squeezed }

  let underlying { underlying; last_squeezed = _ } = underlying

  module Make
      (Bool : Intf.T) (Field : sig
        type t

        val to_bits : t -> Bool.t list

        val finalize_discarded : Bool.t list -> unit

        val high_entropy_bits : int
      end)
      (Input : Intf.T)
      (S : Intf.Sponge
             with module State := State
              and module Field := Field
              and type digest := Field.t
              and type input := Input.t) =
  struct
    type nonrec t = (S.t, Bool.t) t

    let state t = S.state t.underlying

    let high_entropy_bits = Field.high_entropy_bits

    let create ?init params =
      { underlying = S.create ?init params; last_squeezed = [] }

    let copy { underlying; last_squeezed } =
      { underlying = S.copy underlying; last_squeezed }

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
        let hi =
          let hi, lo = List.split_n (Field.to_bits x) high_entropy_bits in
          Field.finalize_discarded lo ;
          hi
        in
        t.last_squeezed <- t.last_squeezed @ hi ;
        squeeze ~length t

    let squeeze_field t =
      t.last_squeezed <- [] ;
      S.squeeze t.underlying
  end
end
