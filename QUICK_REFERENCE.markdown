# Installation

After installing the dependencies, as described in the README,
run

```bash
./INSTALL.sh
```

# Example compilation, key generation, proving flow
snarky_cli generate-keys snarky-examples/tutorial_simple_snark.zk --curve Bn128
snarky_cli prove snarky-examples/tutorial_simple_snark.zk --curve Bn128 2 256
snarky_cli verify snarky-examples/tutorial_simple_snark.zk 2 256 --proof tutorial_simple_snark_gen.zkp

# Tutorials
Check out snarky-examples for a bunch of tutorial code.
We recommend looking at them in the following order.

- tutorial_basics.zk
- tutorial_field.zk    
- tutorial_sqrt.zk
- tutorial_simple_snark.zk
- tutorial_boolean.zk
- tutorial_request.zk

- problem_cube_root.zk
- advanced_big_root.zk 
- advanced_is_square.zk

# Standard library
snarky includes the OCaml standard library, which you can find
[here](https://caml.inria.fr/pub/docs/manual-ocaml/libref/).

On top of this it adds snark specific functions, documentation for
which can be found in `snarky-docs/stdlib-reference/`.
