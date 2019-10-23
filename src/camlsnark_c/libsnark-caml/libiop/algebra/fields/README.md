# Fields

This folder contains implementations of various binary fields,
and some helper methods to distinguish these binary fields from the supported smooth prime fields from libff.
(These are distinguished as either "additive" or "multiplicative" field types from the utils file)

For the binary fields, the irreducible polynomial of order `d` chosen is the lexicographically smallest
such polynomial of the given degree.
The field implementations provided in this library will utilize Intel intrinsics
if available, and otherwise will fall back to a pure C++ implementation.<sup>[1]</sup>
The implementation of field arithmetic will work for any irreducible polynomial
such that there is no term with degree greater than `d / 2` other than `x^d`.
The reason for this constraint is described in the multiplication section.

<sup>[1]</sup>: More precisely, it depends on if the 128 bit carryless instruction is supported. This has the corresponnding CPUID flag: `PCLMULQDQ`.

## Encoding
An element of a binary field can be represented as `d` bits,
where the `i`th bit counting from the right is `x^i`.
We encode these bits in fixed size arrays of int64s.
The size of the int64 array is `d / 64`.

## Addition / Subtraction

Addition over any field extension is carry-less addition.
In a binary field, this is equivalent to a XOR operation.
Similarly, in a binary field subtraction is equivalent to a XOR as well.

## Multiplication

Multiplication over a binary field is [carry-less multiplication](https://en.wikipedia.org/wiki/Carry-less_product),
with modular reduction.

When taking advantage of Intrinsics, the relevant operation provided is `_mm_clmulepi64_si128`.
This allows carry-less multiplication of two 64 bit numbers into a 128 bit number.
Because there is no term in the irreducible polynomial with degree greater
than `d / 2` other than `x^d`, the number of operations needed for
modular reduction can be reduced. (See section 5.2 of [MS17])

This same multiplication implementation is used for squaring.

## Inversion

Field inversion is implemented as `x^{-1} = x^{2^d -2}`, using an addition chains for this exponentiation.
These addition chains were found using the Bergeron-Berstel-Brlek-Duboc method implemented in
https://github.com/kwantam/addchain.

## References

[MS17] [On Fast Multiplication in Binary Finite Fields and Optimal Primitive Polynomials over GF(2)](https://eprint.iacr.org/2017/889), Alexander Maximov and Helena Sjoberg, 2017.