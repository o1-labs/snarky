# R1CS RS IOP

This folder contains the reduction from R1CS to Lincheck, that is used within both Aurora and Fractal. The difference between this folder instantiating Aurora or Fractal comes down to the choice of lincheck that is being reduced to. To use Aurora, the `holographic` flag should be set to false, and it will use Aurora's non-holographic lincheck. To use Fractal, set `holographic = true`, and pass in the indexed oracle handles. The reducer will then use Fractals holographic lincheck.

<!-- The high-level structure of this reduction is as follows:

Recall that an R1CS instance consists of 3 matrices, A, B and C.
The instance is satisfied if there exists a vector z, such that
Az * Bz = Cz.

The reduction to lincheck has the prover low degree extend its variable assignment z, and Az, Bz, Cz from a domain H. We call these LDEs f_z, f_Az, f_Bz, f_Cz.
The prover sends these as oracles to the verifier. The verifier has to check that f_Az, f_Bz, f_Cz are each linearly related to f_z by their respective matrix. Checking this is called lincheck.
Given that they are all linearly related, it remains to check that the instance is satisfied. This is done by a test called rowcheck. Rowcheck can be done without any additional oracles.

There -->