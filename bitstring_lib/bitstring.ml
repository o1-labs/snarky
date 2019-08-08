open Core_kernel

module type S = sig
  type 'a t = private 'a list

  include Container.S1 with type 'a t := 'a t

  val of_list : 'a list -> 'a t

  val init : int -> f:(int -> 'a) -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val pad : padding_length:int -> zero:'a -> 'a t -> 'a t
end

module T = struct
  include List

  let of_list = Fn.id
end

module Msb_first = struct
  include T

  let of_lsb_first = List.rev

  let pad ~padding_length ~zero xs =
    List.init padding_length ~f:(fun _ -> zero) @ xs
end

module Lsb_first = struct
  include T

  let of_msb_first = List.rev

  let pad ~padding_length ~zero xs =
    xs @ List.init padding_length ~f:(fun _ -> zero)
end

let pad_to_triple_list ~default xs =
  let rec go acc = function
    | [] ->
        List.rev acc
    | [x1] ->
        List.rev ((x1, default, default) :: acc)
    | [x1; x2] ->
        List.rev ((x1, x2, default) :: acc)
    | x1 :: x2 :: x3 :: xs ->
        go ((x1, x2, x3) :: acc) xs
  in
  go [] xs
