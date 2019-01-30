open Core
module Snark = Snarky.Snark.Make (Snarky.Backends.Mnt4.GM)

let%bench_fun "projecting 256-bit list" =
  let l = List.init 256 ~f:(Fn.const true) in
  fun () -> Snark.Field.project l |> ignore

let%bench_fun "slow projecting 256-bit list" =
  let l = List.init 256 ~f:(Fn.const true) in
  fun () -> Snark.Field.project_reference l |> ignore
