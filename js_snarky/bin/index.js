#!/usr/bin/env node
const fs = require("fs");

/* Drop the first 2 arguments from `/usr/bin/node path/to/this/file.js ...` */
var arguments = process.argv.slice(2);

function fail(err) {
  if (err) {
    console.log("Error: " + err.message);
    process.exit(1);
  };
};

if (arguments.length === 0) {
  console.log("Expected an argument.\nPossible commands are:\n  init\t\tInitialize a js_snarky project");
  process.exit(1);
}
else if (arguments[0] === "init") {
  /* Create dune-project file so that `dune build` doesn't walk out of this
   * directory. */
  if (!fs.existsSync("dune-project")) {
    const dune_project = "(lang dune 1.6)\n";
    fs.writeFile("dune-project", dune_project, fail);
  }
  let mk_src = function(err) {
    fail(err);
    if (!fs.existsSync("src/dune")) {
      const dune = `(executable
 (name run_snarky)
 (modes native)
 (libraries core_kernel snarky)
 (preprocess (pps ppx_snarky ppx_jane)))
`;
      fs.writeFile("src/dune", dune, fail);
    }
    if (!fs.existsSync("src/main.ml")) {
      const main = `open Snarky
module Backend = Backends.Bn128.Default

module Snark =
  Snark.Run.Make
    (Backend)
    (struct
      type t = unit
    end)

open Snark

(* The return type of main. *)
type result = unit

(* The type of main. Note: The final [unit -> unit] is required. *)
type computation = Field.t -> unit -> unit

(* The public inputs to be passed to main, as a type. *)
type public_input = Field.Constant.t -> unit

(* The list of [Typ.t]s describing how to store the public input in the snark.
*)
let public_input = Data_spec.[Typ.field]

(* Convert the string read on the command line to the public inputs. *)
let read_input : string -> (unit, public_input) H_list.t =
 fun s -> H_list.[Field.Constant.of_string s]

(* The main function. This is executed to build a proof *)
let main field_elt () =
  let field_elt_2 = Field.(field_elt * field_elt) in
  let field_elt_inv = Field.inv field_elt in
  let field_elt_inv_2 = Field.(field_elt_inv * field_elt_inv) in
  let product = Field.(field_elt_2 * field_elt_inv_2) in
  Field.Assert.equal product Field.one
`;
      fs.writeFile("src/main.ml", main, fail);
    }
    if (!fs.existsSync("src/run_snarky.ml")) {
      const run_snarky = `module Run =
  Snarky.Toplevel.Make
    (Main.Snark)
    (struct
      include Main

      let compute = main
    end)

let () = Run.main ()
`;
      fs.writeFile("src/run_snarky.ml", run_snarky, fail);
    }
  };
  if (fs.existsSync("src")) {
    mk_src();
  } else {
    fs.mkdir("src", mk_src);
  }
  if (!fs.existsSync("index.js")) {
    /* TODO. */
    const index = "";
    fs.writeFile("index.js", index, fail);
  }
  if (!fs.existsSync("package.json")) {
    const pkg = `{
  "name": "your-package-name",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "build": "dune build src/run_snarky.exe",
    "clean": "dune clean",
    "watch": "dune build -w src/run_snarky.exe"
  },
  "author": "you",
  "license": "UNLICENSED"
}
`;
    fs.writeFile("package.json", pkg, fail);
  }
} else {
  console.log("Unrecognised command " + arguments[0] + ".\nPossible commands are:\n  init\t\tInitialize a js_snarky project");
  process.exit(1);
}
