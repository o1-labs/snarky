open Alcotest
open Core_kernel
module Bignum_bigint = Bigint


module type Constraint_system = sig

  type t

  type constraint_

  val create : unit -> t

  val finalize : t -> unit

  val add_constraint : t -> constraint_ -> unit

  val digest : t -> Md5.t

  val set_primary_input_size : t -> int -> unit

  val set_auxiliary_input_size : t -> int -> unit

  val get_public_input_size : t -> int Core_kernel.Set_once.t

  val get_rows_len : t -> int
end

type constraint = int



(*
module type S = sig
  module Field : Snarky_intf.Field.S

  module Bigint : Snarky_intf.Bigint_intf.Extended with type field := Field.t

  val field_size : Bigint.t

  module Cvar : Cvar_intf with type field := Field.t and type t = Field.t Cvar.t

  module Constraint : sig
    type t [@@deriving sexp]

    val boolean : Cvar.t -> t

    val equal : Cvar.t -> Cvar.t -> t

    val r1cs : Cvar.t -> Cvar.t -> Cvar.t -> t

    val square : Cvar.t -> Cvar.t -> t

    val eval : t -> (Cvar.t -> Field.t) -> bool

    val log_constraint : t -> (Cvar.t -> Field.t) -> string
  end

  module R1CS_constraint_system :
    Constraint_system.S
      with module Field := Field
      with type constraint_ = Constraint.t

  module Run_state :
    Run_state_intf.S
      with type field := Field.t
       and type constraint_ := Constraint.t
end


[@@warning "-32"] 
module Backend (Field: Snarky_intf.Field.Full) : Snarky_backendless.Backend_intf.S = struct
  module Field = Field
  module Bigint = struct 
    include Bigint.Unstable

    let of_field = failwith "of_field not implemented"

    let test_bit = failwith "test_bit not implemented"

    let to_field = failwith "to_field not implemented"

    let of_data = failwith "of_data not implemented"

    let length_in_bytes = failwith "length_in_bytes not implemented"

    let of_decimal_string = failwith "of_decimal_string not implemented"

    let of_numeral = failwith "of_numeral not implemented"
  end
  let field_size = Field.size
  module Cvar = Snarky_backendless.Cvar.Make(Field)
  module Constraint = Snarky_backendless.Constraint
  module Run_state = Snarky_backendless.Run_state

end

*)


module Field = 
  Snarkette.Fields.Make_fp 
    (Snarkette.Nat) 
    (struct let order = Snarkette.Nat.of_int 5 end)



(** 
  For a given prime p where p = 2 mod 3, we can find a solution to fermat's equation
  x^3 + y^3 = z^3. 
  
  Indeed the mapping x -> x^3 is a bijection (since 3 doesn't divide p-1),
  so fixing any (a, c) we can solve for a unique y^3 = c^3 - a^3. To solve 
  for a cubic root, we use the following:

  Define k = (2p - 1) / 3. By Fermat's little theorem, we have x^(p-1) = x mod p. 
  Thus we can compute
    (x^k)^3 = x^(2p - 1)
            = x * x^(2p - 2)
            = x * (x^(p-1))^2
            = x

  So x^k is a cubic root of x.
*)

module Make (Impl : Snarky_backendless.Snark_intf.S) = struct
  type cube_root = Solve_cube_root of Impl.Field.t

  let typ =
    let typ = Impl.Field.typ in
    Impl.Typ.transport typ
      ~there:(fun (Solve_cube_root x) -> x)
      ~back:(fun x -> Solve_cube_root x)

  type _ Snarky_backendless.Request.t +=
    | Cube_root : cube_root Snarky_backendless.Request.t

  (* TODO: Why isn't this defined in the field interface ? *)
  let pow x n =
    let k = Impl.Bigint.length_in_bytes * 8 in
    let rec go acc i =
      if Int.(i < 0) then acc
      else
        let acc = Impl.Field.square acc in
        let acc =
          if Impl.Bigint.test_bit n i then Impl.Field.(acc * x) else acc
        in
        go acc Int.(i - 1)
    in
    go Impl.Field.one Int.(k - 1)

  let cubic_root y_cubed =
    let p = Impl.Field.size in
    let k =
      Impl.Bigint.of_bignum_bigint
        Bigint.(((of_int 2 * p) - of_int 1) / of_int 3)
    in
    pow y_cubed k

  let circuit a c =
    let open Impl.Checked.Let_syntax in
    let%bind b =
      Impl.exists
        ~compute:
          Impl.As_prover.(
            map2 (read_var a) (read_var c) ~f:(fun a c ->
                let cube z =
                  pow z (Impl.Bigint.of_bignum_bigint (Bigint.of_int 3))
                in
                cubic_root Impl.Field.(cube c - cube a) ))
        Impl.Typ.field
    in
    let cube z = Impl.Field.Checked.(mul z z >>= fun z2 -> mul z2 z) in
    Impl.Field.Checked.(
      cube a
      >>= fun a ->
      cube b >>= fun b -> cube c >>= fun c -> Assert.equal (a + b) c)
end

let factors_test () =
  (* Your test code here *)
  check bool "Test description" true true

(* Export the test cases *)
let test_cases = [ test_case "Test name" `Quick factors_test ]
