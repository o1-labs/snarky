open Core_kernel
open Snarky

module Make
    (Field : Field_intf.Extended) (Coefficients : sig
        val a : Field.t
    end) =
struct
  type t = (Field.t * Field.t) option [@@deriving eq, sexp]

  let to_affine_coordinates t = Option.value_exn t

  let of_affine_coordinates = Option.return

  let zero = None

  let negate = Option.map ~f:(fun (x, y) -> (x, Field.negate y))

  let ( + ) p1 p2 =
    match (p1, p2) with
    | None, _ ->
        p2
    | _, None ->
        p1
    | Some (x1, y1), Some (x2, y2) ->
        if Field.equal x1 x2 && Field.(equal zero (y1 + y2)) then None
        else
          let lambda =
            if equal p1 p2 then
              Field.(((of_int 3 * x1 * x1) + Coefficients.a) / (y1 + y1))
            else Field.((y2 - y1) / (x2 - x1))
          in
          let open Field in
          let x3 = (lambda * lambda) - x1 - x2 in
          Some (x3, (lambda * (x1 - x3)) - y1)
end
