use kimchi::circuits::{expr::FeatureFlag, lookup::lookups::LookupPattern};
use kimchi::proof::{caml::CamlRecursionChallenge, PointEvaluations};
use ocaml_gen::{decl_fake_generic, decl_func, decl_module, decl_type, decl_type_alias, Env};
use std::fs::File;
use std::io::Write;

//
// we must import all here, to have access to the derived functions
//

use kimchi::snarky::checked_runner::caml::*;
use snarky_rs::*;

/// The header of the generated file
const HEADER: &str = "(* This file is generated automatically with ocaml_gen. *)\n";

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let out_file = args.get(1).expect("expected output file");
    let file = &mut File::create(out_file).expect("could not create output file");

    write!(file, "{}", HEADER).unwrap();

    let env = &mut Env::default();

    decl_fake_generic!(T1, 0);

    decl_type!(file, env, CamlRunState<T1> => "run_state");
}
