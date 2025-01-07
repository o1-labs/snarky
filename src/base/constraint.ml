open Base

type ('var, 'field) basic = ..

module Conv (F : sig
  type (_, _) t
end) =
struct
  type t =
    { to_basic : 'v 'f. ('v, 'f) F.t -> ('v, 'f) basic
    ; of_basic : 'v 'f. ('v, 'f) basic -> ('v, 'f) F.t
    }
end

module type S = sig
  type (_, _) t [@@deriving sexp]

  val map : ('a, 'f) t -> f:('a -> 'b) -> ('b, 'f) t

  (* TODO: Try making this a functor and seeing how it affects performance *)
  val eval :
       (module Snarky_intf.Field.S with type t = 'f)
    -> ('v -> 'f)
    -> ('v, 'f) t
    -> bool
end

module Basic = struct
  type ('v, 'f) t = ('v, 'f) basic

  module type S_with_conv = sig
    include S

    val to_basic : ('v, 'f) t -> ('v, 'f) basic

    val of_basic : ('v, 'f) basic -> ('v, 'f) t option
  end

  module Entry = struct
    type t = (module S_with_conv)
  end

  let cases : Entry.t list ref = ref []

  let add_case m = cases := m :: !cases

  let case f = List.find_map_exn !cases ~f:(fun m -> try f m with _ -> None)

  let sexp_of_t f1 f2 t =
    case (fun (module M) -> M.of_basic t |> Option.map ~f:(M.sexp_of_t f1 f2))

  let t_of_sexp f1 f2 s =
    case (fun (module M) -> Some (M.to_basic (M.t_of_sexp f1 f2 s)))

  let eval (type f) (fm : (module Snarky_intf.Field.S with type t = f))
      (f : 'v -> f) (t : ('v, f) basic) : bool =
    case (fun (module M) -> M.of_basic t |> Option.map ~f:(M.eval fm f))

  let map t ~f =
    case (fun (module M) ->
        M.of_basic t |> Option.map ~f:(fun t -> M.to_basic (M.map t ~f)) )
end

module Add_kind (C : S) : sig
  type ('v, 'f) basic += T of ('v, 'f) C.t
end = struct
  type ('v, 'f) basic += T of ('v, 'f) C.t

  module M = struct
    include C

    let to_basic x = T x

    let of_basic = function T x -> Some x | _ -> None
  end

  let () = Basic.add_case (module M)
end

(* We special case these for compatibility with existing code. *)
type ('var, _) basic +=
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

    let to_basic : 'v t -> ('v, _) basic = function
      | Boolean x ->
          Boolean x
      | Equal (x, y) ->
          Equal (x, y)
      | Square (x, y) ->
          Square (x, y)
      | R1CS (x, y, z) ->
          R1CS (x, y, z)

    let of_basic : ('v, _) basic -> 'v t = function
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
    type ('v, 'f) t = ('v, 'f) basic

    let sexp_of_t f _ t = Essential.(sexp_of_t f (of_basic t))

    let t_of_sexp f _ s = Essential.(to_basic (t_of_sexp f s))

    let of_basic t = Some t

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

    let eval (type f v) (module Field : Snarky_intf.Field.S with type t = f)
        (get_value : v -> f) (t : (v, f) basic) : bool =
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

type ('v, 'f) t = ('v, 'f) basic [@@deriving sexp]

module T = struct
  let equal x y = Equal (x, y)

  let boolean x = Boolean x

  let r1cs a b c = R1CS (a, b, c)

  let square a c = Square (a, c)
end

include T
