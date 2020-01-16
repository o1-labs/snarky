open Core_kernel

module type Inputs_intf = sig
  module Field : Field_intf.Extended

  module Var : sig
    include Comparable.S

    include Sexpable.S with type t := t

    val create : int -> t

    val index : t -> int
  end

  module Linear_combination : sig
    type t = Field.t Backend_types.Linear_combination.t

    val create : unit -> t

    val of_var : Var.t -> t

    val of_field : Field.t -> t

    val add_term : t -> Field.t -> Var.t -> unit

    module Term : sig
      type t

      val create : Field.t -> Var.t -> t

      val coeff : t -> Field.t

      val var : t -> Var.t

      module Vector : Vector.S with type elt = t
    end

    val terms : t -> Term.Vector.t
  end

  module R1CS_constraint : sig
    type t = Field.t Backend_types.R1CS_constraint.t

    val create :
      Linear_combination.t -> Linear_combination.t -> Linear_combination.t -> t

    val set_is_square : t -> bool -> unit

    val a : t -> Linear_combination.t

    val b : t -> Linear_combination.t

    val c : t -> Linear_combination.t
  end

  module R1CS_constraint_system : sig
    type t = Field.t Backend_types.R1CS_constraint_system.t

    val create : unit -> t

    val finalize : t -> unit

    val add_constraint : t -> R1CS_constraint.t -> unit

    val set_primary_input_size : t -> int -> unit

    val set_auxiliary_input_size : t -> int -> unit

    val get_primary_input_size : t -> int

    val get_auxiliary_input_size : t -> int

    val report_statistics : t -> unit

    val add_constraint_with_annotation :
      t -> R1CS_constraint.t -> string -> unit

    val check_exn : t -> unit

    val is_satisfied :
         t
      -> primary_input:Field.Vector.t
      -> auxiliary_input:Field.Vector.t
      -> bool

    val digest : t -> Core_kernel.Md5.t

    val iter_constraints : f:(R1CS_constraint.t -> unit) -> t -> unit

    val fold_constraints :
      f:('a -> R1CS_constraint.t -> 'a) -> init:'a -> t -> 'a
  end
end

module Make (Inputs : Inputs_intf) :
  Backend_intf.Constraint_system_intf
  with module Field := Inputs.Field
   and type t = Inputs.R1CS_constraint_system.t = struct
  open Inputs
  include R1CS_constraint_system

  (* TODO: Don't reinstantiate. *)
  module Cvar = Cvar.Make (Field) (Var)

  module Linear_combination = struct
    let of_constant = function
      | None ->
          Linear_combination.create ()
      | Some c ->
          Linear_combination.of_field c

    let of_var (cv : Cvar.t) =
      let constant, terms = Cvar.to_constant_and_terms cv in
      let t = of_constant constant in
      List.iter terms ~f:(fun (c, v) -> Linear_combination.add_term t c v) ;
      t

    let of_field = Linear_combination.of_field

    let zero = of_field Field.zero

    let to_var lc =
      let terms = Linear_combination.terms lc in
      let l =
        List.init (Linear_combination.Term.Vector.length terms) ~f:(fun i ->
            let term = Linear_combination.Term.Vector.get terms i in
            let coeff = Linear_combination.Term.coeff term in
            let var = Linear_combination.Term.var term in
            let index = Var.index var in
            let var =
              if Int.equal index 0 then Cvar.constant Field.one
              else Cvar.Unsafe.of_index (index - 1)
            in
            (coeff, var) )
      in
      Cvar.linear_combination l
  end

  let basic_to_r1cs_constraint : Cvar.t Constraint.basic -> R1CS_constraint.t =
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
        let constr = R1CS_constraint.create (of_var a) (of_var b) (of_var c) in
        R1CS_constraint.set_is_square constr false ;
        constr

  let add_constraint ?label t c =
    let c = basic_to_r1cs_constraint c in
    match label with
    | None ->
        add_constraint t c
    | Some label ->
        add_constraint_with_annotation t c label

  let basic_to_json = function
    | Constraint.Boolean x ->
        let fx = Cvar.to_json x in
        `Assoc [("A", fx); ("B", fx); ("C", fx)]
    | Equal (x, y) ->
        `Assoc
          [ ("A", `Assoc [])
          ; ("B", `Assoc [])
          ; ("C", Cvar.to_json (Cvar.sub x y)) ]
    | Square (a, c) ->
        let fa = Cvar.to_json a in
        `Assoc [("A", fa); ("B", fa); ("C", Cvar.to_json c)]
    | R1CS (a, b, c) ->
        `Assoc
          [("A", Cvar.to_json a); ("B", Cvar.to_json b); ("C", Cvar.to_json c)]

  let constraint_to_json x =
    `List (List.map x ~f:(fun {Constraint.basic; _} -> basic_to_json basic))

  let to_json system =
    let constraints : t -> _ Constraint.t =
      fold_constraints ~init:[] ~f:(fun acc constr ->
          let a = Linear_combination.to_var (R1CS_constraint.a constr) in
          let b = Linear_combination.to_var (R1CS_constraint.b constr) in
          let c = Linear_combination.to_var (R1CS_constraint.c constr) in
          Constraint.create_basic (R1CS (a, b, c)) :: acc )
    in
    let open Base in
    let inputs =
      List.init (get_primary_input_size system) ~f:(fun i ->
          `String (sprintf "input%i" (i + 1)) )
    in
    let auxiliaries =
      List.init (get_auxiliary_input_size system) ~f:(fun i ->
          `String (sprintf "a%i" (i + 1)) )
    in
    `Assoc
      [ ("Variables", `List ((`String "ONE" :: inputs) @ auxiliaries))
      ; ("constraints", constraint_to_json (constraints system)) ]
end
