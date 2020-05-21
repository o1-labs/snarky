open Base

type _ basic = ..

module type S = sig
  type _ t [@@deriving sexp]

  val map : 'a t -> f:('a -> 'b) -> 'b t

  (* TODO: Try making this a functor and seeing how it affects performance *)
  val eval :
    (module Field_intf.S with type t = 'f) -> ('v -> 'f) -> 'v t -> bool
end

module Basic = struct
  type 'v t = 'v basic

  module type S_with_conv = sig
    include S

    val to_basic : 'v t -> 'v basic

    val of_basic : 'v basic -> 'v t
  end

  module Entry = struct
    type t = (module S_with_conv)
  end

  let cases : Entry.t list ref = ref []

  let add_case m = cases := m :: !cases

  let case f =
    List.find_map_exn !cases ~f:(fun m -> Option.try_with (fun () -> f m))

  let sexp_of_t f t = case (fun (module M) -> M.sexp_of_t f (M.of_basic t))

  let t_of_sexp f s = case (fun (module M) -> M.to_basic (M.t_of_sexp f s))

  let eval (type f) (fm : (module Field_intf.S with type t = f)) (f : 'v -> f)
      (t : 'v basic) : bool =
    case (fun (module M) -> M.eval fm f (M.of_basic t))

  let map t ~f = case (fun (module M) -> M.to_basic (M.map (M.of_basic t) ~f))
end

module Add_kind (C : S) : sig
  type 'v basic += T of 'v C.t
end = struct
  type 'v basic += T of 'v C.t

  module M = struct
    include C

    let to_basic x = T x

    let of_basic = function T x -> x | _ -> failwith "different constructor"
  end

  let () = Basic.add_case (module M)
end

(* We special case these for compatibility with existing code. *)
type 'var basic +=
  | Boolean of 'var
  | Equal of 'var * 'var
  | Square of 'var * 'var
  | R1CS of 'var * 'var * 'var

let basic_of_sexp = Basic.t_of_sexp

let sexp_of_basic = Basic.sexp_of_t

let () =
  let unhandled s = Core_kernel.failwithf "%s: non-basic constraint" s () in
  let module Essential = struct
    type 'var t =
      | Boolean of 'var
      | Equal of 'var * 'var
      | Square of 'var * 'var
      | R1CS of 'var * 'var * 'var
    [@@deriving sexp]

    let to_basic : 'v t -> 'v basic = function
      | Boolean x ->
          Boolean x
      | Equal (x, y) ->
          Equal (x, y)
      | Square (x, y) ->
          Square (x, y)
      | R1CS (x, y, z) ->
          R1CS (x, y, z)

    let of_basic : 'v basic -> 'v t = function
      | Boolean x ->
          Boolean x
      | Equal (x, y) ->
          Equal (x, y)
      | Square (x, y) ->
          Square (x, y)
      | R1CS (x, y, z) ->
          R1CS (x, y, z)
      | _ ->
          unhandled "of_basic"
  end in
  let module M = struct
    type 'v t = 'v basic

    let sexp_of_t f t = Essential.(sexp_of_t f (of_basic t))

    let t_of_sexp f s = Essential.(to_basic (t_of_sexp f s))

    let of_basic = Fn.id

    let to_basic = Fn.id

    let map t ~f =
      match t with
      | Boolean v ->
          Boolean (f v)
      | Equal (v1, v2) ->
          Equal (f v1, f v2)
      | R1CS (v1, v2, v3) ->
          R1CS (f v1, f v2, f v3)
      | Square (a, c) ->
          Square (f a, f c)
      | _ ->
          unhandled "map"

    let eval (type f v) (module Field : Field_intf.S with type t = f)
        (get_value : v -> f) (t : v basic) : bool =
      match t with
      | Boolean v ->
          let x = get_value v in
          Field.(equal x zero || equal x one)
      | Equal (v1, v2) ->
          Field.equal (get_value v1) (get_value v2)
      | R1CS (v1, v2, v3) ->
          Field.(equal (mul (get_value v1) (get_value v2)) (get_value v3))
      | Square (a, c) ->
          Field.equal (Field.square (get_value a)) (get_value c)
      | _ ->
          unhandled "eval"
  end in
  Basic.add_case (module M)

type 'v basic_with_annotation = {basic: 'v basic; annotation: string option}
[@@deriving sexp]

type 'v t = 'v basic_with_annotation list [@@deriving sexp]

module T = struct
  let create_basic ?label basic = {basic; annotation= label}

  let override_label {basic; annotation= a} label_opt =
    {basic; annotation= (match label_opt with Some x -> Some x | None -> a)}

  let equal ?label x y = [create_basic ?label (Equal (x, y))]

  let boolean ?label x = [create_basic ?label (Boolean x)]

  let r1cs ?label a b c = [create_basic ?label (R1CS (a, b, c))]

  let square ?label a c = [create_basic ?label (Square (a, c))]

  let annotation (t : 'a t) =
    String.concat ~sep:"; "
      (List.filter_map t ~f:(fun {annotation; _} -> annotation))
end

include T
