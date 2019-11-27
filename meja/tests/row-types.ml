module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module type Fixed_rows = sig
  val no_arg : [`A]

  val no_args : [`A | `B]

  val arg : [`A of int]

  val args : [`A of int | `B of bool]

  val arg_noarg : [`A of int | `B]

  val noarg_arg : [`A | `B of bool]
end

module type Open_rows = sig
  val empty : [> ]

  val no_arg : [> `A]

  val no_args : [> `A | `B]

  val arg : [> `A of int]

  val args : [> `A of int | `B of bool]

  val arg_noarg : [> `A of int | `B]

  val noarg_arg : [> `A | `B of bool]
end

module type Closed_rows = sig
  val no_arg : [< `A]

  val no_args : [< `A | `B]

  val arg : [< `A of int]

  val args : [< `A of int | `B of bool]

  val arg_noarg : [< `A of int | `B]

  val noarg_arg : [< `A | `B of bool]
end

module type Bounded_rows = sig
  val fixed_no_args : [< `A | `B > `A `B]

  val fixed_args : [< `A of int | `B of bool > `A `B]

  val no_args : [< `A | `B > `B]

  val args : [< `A of int | `B of bool > `B]
end

module Fixed_rows = struct
  let no_arg (x : [`A]) (y : [`A]) b = if b then x else y

  let no_args (x : [`A | `B]) (y : [`A | `B]) b = if b then x else y

  let arg (x : [`A of int]) (y : [`A of int]) b = if b then x else y

  let args (x : [`A of int | `B of bool]) (y : [`A of int | `B of bool]) b =
    if b then x else y

  let arg_noarg (x : [`A of int | `B]) (y : [`A of int | `B]) b =
    if b then x else y

  let noarg_arg (x : [`A | `B of bool]) (y : [`A | `B of bool]) b =
    if b then x else y
end

module Open_rows = struct
  let empty (x : [> ]) (y : [> ]) b = if b then x else y

  let no_arg (x : [> `A]) (y : [> `A]) b = if b then x else y

  let no_args (x : [> `A | `B]) (y : [> `A | `B]) b = if b then x else y

  let arg (x : [> `A of int]) (y : [> `A of int]) b = if b then x else y

  let args (x : [> `A of int | `B of bool]) (y : [> `A of int | `B of bool]) b
      =
    if b then x else y

  let arg_noarg (x : [> `A of int | `B]) (y : [> `A of int | `B]) b =
    if b then x else y

  let noarg_arg (x : [> `A | `B of bool]) (y : [> `A | `B of bool]) b =
    if b then x else y
end

module Closed_rows = struct
  let no_arg (x : [< `A]) (y : [< `A]) b = if b then x else y

  let no_args (x : [< `A | `B]) (y : [< `A | `B]) b = if b then x else y

  let arg (x : [< `A of int]) (y : [< `A of int]) b = if b then x else y

  let args (x : [< `A of int | `B of bool]) (y : [< `A of int | `B of bool]) b
      =
    if b then x else y

  let arg_noarg (x : [< `A of int | `B]) (y : [< `A of int | `B]) b =
    if b then x else y

  let noarg_arg (x : [< `A | `B of bool]) (y : [< `A | `B of bool]) b =
    if b then x else y
end

module Bounded_rows = struct
  let fixed_no_args (x : [< `A | `B > `A `B]) (y : [< `A | `B > `A `B]) b =
    if b then x else y

  let fixed_args (x : [< `A of int | `B of bool > `A `B])
      (y : [< `A of int | `B of bool > `A `B]) b =
    if b then x else y

  let no_args (x : [< `A | `B > `B]) (y : [< `A | `B > `B]) b =
    if b then x else y

  let args (x : [< `A of int | `B of bool > `B])
      (y : [< `A of int | `B of bool > `B]) b =
    if b then x else y
end

module Mixed_rows = struct
  let fixed_open (x : [`A]) (y : [> ]) b = if b then x else y

  let fixed_closed (x : [`A]) (y : [< `A | `B]) b = if b then x else y

  let fixed_bounded (x : [`A]) (y : [< `A | `B > `A]) b = if b then x else y

  let open_distinct (x : [> `A]) (y : [> `B]) b = if b then x else y

  let open_subset (x : [> `A]) (y : [> `A | `B]) b = if b then x else y

  let open_overlap (x : [> `A | `B]) (y : [> `A | `C]) b = if b then x else y

  let open_closed (x : [> `A | `B]) (y : [< `A | `B | `C]) b =
    if b then x else y

  let open_bounded (x : [> `A | `B]) (y : [< `A | `B | `C > `A]) b =
    if b then x else y
end

module Copy_test = struct
  let f (x1 : [> `A]) (x2 : [> `B]) (x3 : [> `C]) (x4 : [> `D]) b =
    let y1 = if b then x1 else x2 in
    let y2 = if b then x3 else x4 in
    let y3 = if b then x2 else x4 in
    (y1, y2, y3)

  let g (x : [`A | `B | `C | `D | `E]) = f x x x x true

  let h (x : [`A | `B | `C | `D | `F]) = f x x x x true
end
