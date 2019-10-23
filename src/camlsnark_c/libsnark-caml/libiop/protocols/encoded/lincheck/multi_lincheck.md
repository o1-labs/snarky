# Lincheck

Aurora creates 3 lincheck instances, each with different f_Mz codewords, and the same f_z codeword.
These lincheck instances all use the same, verifier chosen `alpha` term.
These lincheck codewords are then collectively reduced to the amortized univariate sumcheck protocol.
This suggests that the protocol ought to be a `multi_lincheck`.

Note that each lincheck instance's codeword is of the form:
`p_1(alpha)|L * f_Mz - p_2(alpha, M)|L * f_z`,
where `p_1, p_2` are polynomials constructed from evaluations in the same systematic domain.

`p_2` gets constructed over the codeword domain for every lincheck instance, as it is M dependent.
Since `p_1` is the same across all linchecks, it can simply be computed once.
This means that for the 3 codewords, 4 polynomial interpolations, and 4 FFTs into the codeword domain are required.

There is an additional optimization we can take, using the information that `f_z` is common to all lincheck instances!
The amortization in the univariate sumcheck protocol takes a random linear combination of the provided codewords.
Let `r_A, r_B, r_C` be these random scalar coefficients.
```
r_A * (p_1(alpha) * f_Az - p_2(alpha, A) * f_z)
 + r_B * (p_1(alpha) * f_Bz - p_2(alpha, B) * f_z)
 + r_C * (p_1(alpha) * f_Cz - p_2(alpha, C) * f_z)
= (r_A * f_Az + r_B * f_Bz + r_C * f_Cz) * (p_1(alpha))
  - (r_A * p_2(alpha, A) + r_B * p_2(alpha, B) + r_C * p_2(alpha, C)) * f_z
```

The important part of this is that the second term `(r_A * p_2(alpha, A) + r_B * p_2(alpha, B) + r_C * p_2(alpha, C))` can be computed from its systematic evaluations directly.
This saves the prover 2 interpolations and 2 FFTs into the codeword domain,
and saves the verifier two lagrange interpolations per query point.

This technique can be extended to arbitrarily many lincheck instances that share `f_z`.