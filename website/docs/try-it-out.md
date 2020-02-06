---
id: try-it-out
title: Try it out
sidebar_label: Try it out
---

To test your Snarky installation, we'll try a simple example!

Create a new folder (`try_snarky`, for example) and a sub-folder called `src`.
This will be your 'project directory'.

The easiest way to build an OCaml project is with the
[dune](https://dune.build/) build system. Inside your newly-created `src`
directory, create a file called `dune` to tell it how to build our new project:
```dune
(executables
 (names try_me)
 (modes native)
 (preprocess (pps ppx_jane))
 (libraries snarky core))
```

Also inside `src`, make a new file called `try_me.ml`, and enter the following code:
```ocaml
open Core
open Snarky

module Impl = Snark.Run.Make (Backends.Bn128.Default) (Unit)
open Impl

let check_product (l : Field.t list) (total : Field.t) () =
  let open Field in
  let checked_total = List.fold ~init:one l ~f:( * ) in
  Assert.equal checked_total total

let my_list = List.map ~f:Field.Constant.of_int [1; 2; 3; 4; 5]

let total = Field.Constant.of_int 120

let public_input () = Data_spec.[Typ.list ~length:(List.length my_list) Field.typ; Field.typ]

let keypair = generate_keypair ~exposing:(public_input ()) check_product

let proof = prove (Keypair.pk keypair) (public_input ()) check_product () my_list total

let is_valid = verify proof (Keypair.vk keypair) (public_input ()) my_list total

let () = printf "Verified program? %b\n" is_valid
```

Now, in the project directory, create a file called `dune-project`:
```
(lang dune 1.7)
```

You should have a project directory structured like this:
```
dune-project
src/
  dune
  try_me.ml
```

With that, we have everything we need to run our first Snarky program! In the
project directory, run
```sh
dune exec src/try_me.exe
```

## Tutorials and examples

We believe a great way to learn is by doing! You can find our tutorial [on
GitHub](https://github.com/o1-labs/snarky/blob/master/examples/tutorial), as
well as
[some](https://github.com/o1-labs/snarky/tree/master/examples/merkle_update)
[examples](https://github.com/o1-labs/snarky/tree/master/examples/election) to
try out.
