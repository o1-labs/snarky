module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module type S = sig
  exception X

  exception Y of int

  exception Z : bool -> exn
end

module T = struct
  exception X

  exception Y of int

  exception Z : bool -> exn
end

open T

let (x : exn) = X

let (y : exn) = Y 15

let (z : exn) = Z true
