opam = struct(
    opam_version = "2.0",
    pins = {
        "async_kernel" : "src/external/async_kernel",
        "base58"       : "src/external/coda_base58",
        "graphql_ppx"  : "src/external/graphql_ppx",
        "ocaml-extlib" : "src/external/ocaml-extlib",
        "rpc_parallel" : "src/external/rpc_parallel",
        "sodium"       : "src/external/ocaml-sodium"
    },
    packages = {
        "async": "v0.12.0",
        "bignum": "v0.12.0",
        "bin_prot": "v0.12.0",
        "bisect_ppx": "2.4.1",
        "bisect_ppx.runtime": "2.4.1",
        "core": "v0.12.1",
        "core_kernel": "v0.12.3",
        "ctypes": "0.17.1",
        "ctypes.foreign": "0.17.1", # opam: "0.4.0",
        # "ctypes.stubs": "0.17.1", # opam: "0.4.0",
        "digestif": "0.9.0",
        # "digestif.c": "0.9.0",
        "fieldslib": "v0.12.0",
        "num": "1.1",
        "ppx_assert": "v0.12.0",
        "ppx_assert.runtime-lib": "v0.12.0",
        "ppx_bench": "v0.12.0",
        "ppx_bench.runtime-lib": "v0.12.0",
        "ppx_bin_prot": "v0.12.1",
        "ppx_compare": "v0.12.0",
        "ppx_compare.runtime-lib": "v0.12.0",
        "ppx_custom_printf": "v0.12.0",
        "ppx_deriving": "4.4.1",
        "ppx_deriving.enum": "n/a", # opam: "4.4.1",
        "ppx_deriving.eq": "n/a", # opam: "4.4.1",
        "ppx_deriving.std": "n/a", # opam:  "4.4.1",
        "ppx_deriving.runtime": "n/a", # opam:  "4.4.1",
        "ppx_deriving_yojson": "3.5.2",
        "ppx_deriving_yojson.runtime": "n/a", # opam: "3.5.2",
        "ppx_enumerate": "v0.12.0",
        "ppx_enumerate.runtime-lib": "v0.12.0",
        "ppx_expect": "v0.12.0",
        "ppx_expect.collector": "v0.12.0",
        "ppx_fields_conv": "v0.12.0",
        "ppx_hash": "v0.12.0",
        "ppx_hash.runtime-lib": "v0.12.0",
        "ppx_inline_test": "v0.12.0",
        "ppx_inline_test.runtime-lib": "v0.12.0",
        "ppx_jane": "v0.12.0",
        "ppx_let": "v0.12.0",
        "ppx_module_timer": "v0.12.0",
        "ppx_module_timer.runtime": "v0.12.0",
        "ppx_sexp_conv": "v0.12.0",
        "ppx_sexp_conv.runtime-lib": "v0.12.0",
        "ppx_tools": "5.1+4.06.0",
        "ppxlib": "0.8.1",
        "ppxlib.metaquot": "0.8.1",
        "yojson": "1.7.0",
    }
)
