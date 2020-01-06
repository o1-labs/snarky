# cuda-fixnum
`cuda-fixnum` is a fixed-precision SIMD library that targets CUDA. It provides the apparatus necessary to easily create efficient functions that operate on vectors of _n_-bit integers, where _n_ can be much larger than the size of a usual machine or device register.  Currently supported values of _n_ are 32, 64, 128, 256, 512, 1024, and 2048 (larger values will be possible in a forthcoming release).

The primary use case for fast arithmetic of numbers in the range covered by `cuda-fixnum` is in cryptography and computational number theory; in particular it can form an integral part in accelerating homomorphic encryption primitives as used in privacy-preserving machine learning. As such, special attention is given to support modular arithmetic; this is used in an example implementation of the Paillier additively homomorphic encryption scheme and of elliptic curve scalar multiplication.  Future releases will provide additional support for operations useful to implementing Ring-LWE-based somewhat homomorphic encryption schemes.

Finally, the library is designed to be _fast_. Through exploitation of warp-synchronous programming, vote functions, and deferred carry handling, the primitives of the library are currently competitive with the state-of-the-art in the literature for modular multiplication and modular exponentiation on GPUs.  The design of the library allows transparent substitution of the underlying arithmetic, allowing the user to select whichever performs best on the available hardware. Moreover, several algorithms, both novel and from the literature, will be incoporated shortly that will improve performance by a further 25-50%.

The library is currently at the _alpha_ stage of development.  It has many rough edges, but most features are present and it is performant enough to be competitive.  Comments, questions and contributions are welcome!

## Example

To get a feel for what it's like to use the library, let's consider a simple example. Here is an [implementation](cuda-fixnum/src/functions/paillier_encrypt.cu) of encryption in the [Paillier cryptosystem](https://en.wikipedia.org/wiki/Paillier_cryptosystem):
```cuda
#include "functions/quorem_preinv.cu"
#include "functions/modexp.cu"

using namespace cuFIXNUM;

template< typename fixnum >
class paillier_encrypt {
    fixnum n;                     // public key
    fixnum n_sqr;                 // ciphertext modulus n^2
    modexp<fixnum> pow;           // function x |--> x^n (mod n^2)
    quorem_preinv<fixnum> mod_n2; // function x |--> x (mod n^2)

    // This is a utility whose purpose is to allow calculating 
    // and using n^2 in the constructor initialisation list.
    __device__ fixnum square(fixnum n) {
        fixnum n2;
        fixnum::sqr_lo(n2, n);
        return n2;
    }
    
public:
    /*
     * Construct an encryption functor with public key n_.
     * n_ must be less fixnum::BITS/2 bits in order for n^2
     * to fit in a fixnum.
     */
    __device__ paillier_encrypt(fixnum n_)
        : n(n_)
        , n_sqr(square(n_))
        , pow(n_sqr, n_)
        , mod_n2(n_sqr)
        { }

    /*
     * Encrypt the message m using the public key n and randomness r.
     * Precisely, return
     *
     *   ctxt  <-  (1 + m*n) * r^n  (mod n^2)
     *
     * m and r must be at most fixnum::BITS/2 bits (m is interpreted 
     * modulo n anyhow).
     */
    __device__ void operator()(fixnum &ctxt, fixnum m, fixnum r) const {
        fixnum::mul_lo(m, m, n);             // m  <-  m * n   (lo half mult)
        fixnum::incr_cy(m);                  // m  <-  1
        pow(r, r);                           // r  <-  r^n (mod n^2)
        fixnum c_hi, c_lo;                   // hi and lo halves of wide multiplication
        fixnum::mul_wide(c_hi, c_lo, m, r);  // (c_hi, c_lo)  <-  m * r (wide mult) 
        mod_n2(ctxt, c_hi, c_lo);            // ctxt  <-  (c_hi, c_lo) (mod n^2)
    }
};
```
A few features will be common to most user-defined functions such as the one above: They will be template function objects that rely on a `fixnum`, which will be instantiated with one of the fixnum arithmetic implemententations provided, usually the [`warp_fixnum`](cuda-fixnum/src/fixnum/warp_fixnum.cu).  Functions in the `fixnum` class are static and (usually) return their results in the first one or two parameters. Complicated functions that might perform precomputation, such as [modular exponentiation (`modexp`)](cuda-fixnum/src/functions/modexp.cu) and [quotient & remainder with precomputed inverse (`quorem_preinv`)](cuda-fixnum/src/functions/quorem_preinv.cu) are instance variables in the object that are initialised in the constructor.

Although it is not (yet) the focus of this project to help optimise host-device communication, the [`fixnum_array`](cuda-fixnum/src/array/fixnum_array.h) facility is provided to make it easy to apply user-defined functions to data originating in the host. Using `fixnum_array` will often look something like this:
```C++
using namespace cuFIXNUM;

// In this case we need to wrap paillier_encrypt above to read the 
// public key from memory and pass it to the constructor.
__device__ uint8_t public_key[] = ...; // initialised earlier

template< typename fixnum >
class my_paillier_encrypt {
    paillier_encrypt<fixnum> encrypt;
    
    __device__ fixnum load_pkey() {
        fixnum pkey;
        fixnum::from_bytes(pkey, public_key, public_key_len);
        return pkey;
    }
    
public:
    __device__ my_paillier_encrypt()
        : encrypt(load_pkey())
        { }
    
    __device__ void operator()(fixnum &ctxt, fixnum m, fixnum r) const {
        fixnum c;  // Always read into a register, then set the result.
        encrypt(c, m, r);
        ctxt = c;
    }
};

void host_function() {
    ...
    // fixnum represents 256-byte numbers, using a 64-bit "basic fixnum".
    typedef warp_fixnum<256, u64_fixnum> fixnum;
    typedef fixnum_array<fixnum> fixnum_array;

    fixnum_array *ctxts, *ptxts, *rnds, *pkeys;

    int nelts = ...;  // can be as much as 1e6 to 1e8
    // Usually this could be as much as fixnum::BYTES == 256, however
    // in this application it must be at most fixnum::BYTES/2 = 128.
    int message_bytes = ...;

    // Input plaintexts
    ptxts = fixnum_array::create(input_array, message_bytes, nelts);
    // Randomness
    rands = fixnum_array::create(random_data, fixnum::BYTES/2, nelts);
    // Ciphertexts will be put here
    ctxts = fixnum_array::create(ptxts->length());

    // Map  ctxts  <-  [paillier_encrypt(p, r) for p, r in zip(ptxts, rands)]
    fixnum_array::template map<my_paillier_encrypt>(ctxts, rands, ptxts);

    // Access results.
    ctxts->retrieve_all(byte_buffer, buflen);
   ...
}
```

## Building

```
mkdir build
cd build
cmake ..
make bench
```

The build system for cuda-fixnum uses CMake.  Create a working directory to build in and run cmake from there.
Then you can run `make bench` to build the benchmarking program, or `make check` to build and the test suite.
The test suite requires the [Google Test framework](https://github.com/google/googletest) to be installed.

CMake attempts to detect and set the proper flags for you GPU architecture, but you can override them or cross compile if needed.

## Benchmarks

Here is the output from a recent run of the benchmark with a GTX Titan X (Maxwell, 1GHz clock, 3072 cores):

```
$ bin/bench 5000000
Function: mul_lo, #elts: 5000e3
fixnum digit  total data   time       Kops/s
 bits  bits     (MiB)    (seconds)
   32    32      19.1     0.000    24630541.9
   64    32      38.1     0.000    11547344.1
  128    32      76.3     0.001     5091649.7
  256    32     152.6     0.003     1775568.2
  512    32     305.2     0.008      619578.7
 1024    32     610.4     0.030      166855.8

   64    64      38.1     0.000    14619883.0
  128    64      76.3     0.001     7824726.1
  256    64     152.6     0.002     2908667.8
  512    64     305.2     0.006      829875.5
 1024    64     610.4     0.023      221749.2
 2048    64    1220.7     0.087       57611.0


Function: mul_wide, #elts: 5000e3
fixnum digit  total data   time       Kops/s
 bits  bits     (MiB)    (seconds)
   32    32      19.1     0.000    25906735.8
   64    32      38.1     0.000    10775862.1
  128    32      76.3     0.001     3861003.9
  256    32     152.6     0.005      985998.8
  512    32     305.2     0.018      271164.4
 1024    32     610.4     0.060       83847.6

   64    64      38.1     0.000    14662756.6
  128    64      76.3     0.001     6765899.9
  256    64     152.6     0.003     1904036.6
  512    64     305.2     0.009      530278.9
 1024    64     610.4     0.036      140024.6
 2048    64    1220.7     0.129       38680.5


Function: modexp, #elts: 50e3
fixnum digit  total data   time       Kops/s
 bits  bits     (MiB)    (seconds)
   32    32       0.2     0.000      292397.7
   64    32       0.4     0.001       68306.0
  128    32       0.8     0.003       16388.1
  256    32       1.5     0.017        3015.5
  512    32       3.1     0.108         463.6
 1024    32       6.1     0.748          66.9

   64    64       0.4     0.000      113378.7
  128    64       0.8     0.002       20798.7
  256    64       1.5     0.015        3403.2
  512    64       3.1     0.105         476.8
 1024    64       6.1     0.658          76.0
 2048    64      12.2     4.959          10.1
```

It is interesting to note that performance is consistently better with 64-bit
integer arithmetic, even though 64-bit registers are simulated on nVidia
devices. Looks like my 32-bit integer arithmetic code could use some work!

## Sources

The main sources for the algorithms in this library are

 - Brent, R. and Zimmermann, P., [_Modern Computer Arithmetic_](https://members.loria.fr/PZimmermann/mca/pub226.html), Cambridge University Press, 2010.
 - Menezes, A. J., van Oorschot, P. C. and Vanstone, S. A., [_Handbook of Applied Cryptography_](http://cacr.uwaterloo.ca/hac/), CRC Press, 5th printing, 2001. Chapter 14.
 - Granlund, T. and "the GMP development team", [_GNU MP: The GNU Multiple Precision Arithmetic Library_](https://gmplib.org), version 6.1.2.
 
## Author and licence

The principal author of `cuda-fixnum` is Dr Hamish Ivey-Law (@unzvfu). Email: _hamish.ivey-law (at) data61.csiro.au_

`cuda-fixnum` is copyright (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO).

`cuda-fixnum` is released under a modified MIT/BSD Open Source Licence (see [LICENCE](LICENCE) for details).
