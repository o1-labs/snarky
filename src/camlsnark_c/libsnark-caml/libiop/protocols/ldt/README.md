# LDT

This folder contains protocols and infrastructure for low degree testing of oracles.

## Multi LDT

A multi LDT is an LDT implementation which takes as input multiple oracles over the same domain,
and low degree tests each of these oracles at the same rate and proximity.

Multi LDT implementations ought to run the LDTs for the oracles in parallel,
using the same interactive randomness and query positions across all of the oracles.

## LDT instance reducer

An RS-encoded IOP may produce many oracles
which require low degree testing in the compilation to IOP.
Low degree tests are expensive, so we wish to reduce the number of different tests required.
This is precisely what the LDT instance reducer does.

Additionally, to compile the RS-encoded IOP to a normal IOP,
the verifier must be assured that not only are the oracles close to codewords of the correct rate,
but that the rational constraints are satisfied as well.
As discussed in section 8 of the [Aurora](https://eprint.iacr.org/2018/828) paper,
rational constraints can be checked through using virtual oracles and by choosing a sufficiently
small proximity paper for the low degree test.

The proximity parameter is chosen within the LDT instance reducer, it depends on two parameters,
the maximum tested degree bound, and the maximum constraint degree bound.
For each oracle that has to be low degree tested,
its tested degree bound is the claimed degree bound of the oracle.
The constraint degree bound gets its name from the rational constraint.
Recall that a rational constraint consists of two arithmetic circuits,
the numerator and the denominator, where the numerator is a polynomial in the variables
`\alpha, f_1(\alpha), f_2(\alpha), \dots, f_{n}(\alpha)`,
where `f_i` is itself a polynomial of degree `d_i`, and the denominator is a polynomial in `\alpha`.

The degree of the numerator is the smallest `n \in \mathbb{N}` such that there exists a polynomial
q, of degree n, such that for all alpha, `q(\alpha) = Numerator(\alpha, f_1(\alpha), f_2(\alpha), \dots, f_n(\alpha)`.

The constraint degree bound for a rational constraint is `max(degree(Numerator), tested_degree + degree(Denominator)`

### How the LDT instance reducer works internally

The LDT instance reducer takes as input many oracles,
then takes a random linear combination of them to get a single resultant oracle.
There is a soundness error with the random linear combination, so it may produce multiple resultant oracles.
These resultant oracles are then passed into a provided multi LDT.

If the input oracles are purported to be of different degrees,
just taking the random linear combination and testing that would only guarantee
that each constituent oracle was of the max degree.
This does not suffice for many protocols,
so we instead must ensure each constituent oracle is probably near the exact degree it is purported to be.
To solve this,
for each oracle of sub-maximal degree we add the virtual oracle
`x^(max tested degree - oracle degree) * oracle` to the random linear combination.

In the zero-knowledge case,
the prover adds a random polynomial of the maximum tested degree to each of the random linear combinations.
This guarantees that each codeword being tested from an honest prover
is a uniformly distributed codeword of correct rate.
Consequently the verifier can learn the entire polynomial created by the multi LDT,
without loss of zero knowledge, since they only get a fixed number of queries to the random polynomial.

Due to the interactive LDT reducer repetitions needed to hit the soundness target,
the reducer may not reduce proof size for settings that have a field size close to the codeword domain size.
(The soundness error is `|codeword| / |Field|`)
In these situations, consider if there any ways you can increase the field size,
or just directly use the LDT's with re-used interactive / query soundness.

## Supported Multi LDTs
### FRI

We implement the [FRI proximity test](https://eccc.weizmann.ac.il/report/2017/134/), as specified in the original paper.
It re-uses query randomness across FRI's interactive repetitions, as this lowers the overall argument size.
The paper uses the same localization parameter across all reductions.
However smaller argument sizes without additional soundness error can be achieved by using different localization parameters for each reduction.
Consequently supplying a localization parameter array is supported in this implementation.

### Direct LDT

The Direct LDT is described within appendix C.1 of the Aurora paper.
Essentially the prover sends to the verifier the entire polynomial, representing the oracle.
The verifier then queries the oracle at a few points to ensure that the oracle is consistent with the provided polynomial.