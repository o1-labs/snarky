# RS-encoded IOPs

This folder contains the various RS-encoded IOPs used within Aurora, Fractal, and Ligero.

Aurora and Fractal share the same reduction from R1CS to Lincheck. This reduction is contained in the [r1cs_rs_iop](/libiop/protocols/encoded/r1cs_rs_iop) directory. Switching between Aurora or Fractal depends on if the `holographic` flag is passed to that reduction, which in turn switches between Aurora's lincheck and Fractal's holographic lincheck.

The RS-encoded Ligero doesn't share any common IOPs with Aurora, so its code is all contained within the [ligero](/libiop/protocols/encoded/ligero) directory.