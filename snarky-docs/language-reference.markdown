This is an overview of the syntax, constructs and library functions available in snarky. I will be updating it as the workshop continues
so don't forget to pull!

# Library functions
The following modules are in the standard library:

- Field (documented in [snarky-docs/stdlib-reference/field.zki]).
- Boolean (documented in [snarky-docs/stdlib-reference/field.zki]).

# Computing pedersen hashes outside the SNARK

Use the program "crypto_utils" to compute pedersen hashes. Run it with

```bash
dune exec crypto_util/crypto_util.exe -- -help
```
for usage info.
