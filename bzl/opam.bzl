PACKAGES = {
    "async": ["v0.12.0"],
    "bignum": ["v0.12.0"], # WARNING: depends on zarith which depends on libgmp-dev on local system
    "bin_prot": ["v0.12.0"],
    "core": ["v0.12.1"],
    "core_kernel": ["v0.12.3"],
    "digestif": ["0.9.0"],
    "fieldslib": ["v0.12.0"],
    "num": ["1.1"],
    "ppx_assert": ["v0.12.0", ["ppx_assert.runtime-lib"]],
    "ppx_bench": ["v0.12.0", ["ppx_bench.runtime-lib"]],
    "ppx_bin_prot": ["v0.12.1"],
    "ppx_compare": ["v0.12.0", ["ppx_compare.runtime-lib"]],
    "ppx_custom_printf": ["v0.12.0"],
    "ppx_deriving": ["4.4.1", [
        "ppx_deriving.enum",
        "ppx_deriving.eq",
        "ppx_deriving.std",
        "ppx_deriving.runtime"
    ]],
    "ppx_deriving_yojson": ["3.5.2", ["ppx_deriving_yojson.runtime"]],
    "ppx_enumerate": ["v0.12.0", ["ppx_enumerate.runtime-lib"]],
    "ppx_expect": ["v0.12.0", ["ppx_expect.collector"]],
    "ppx_fields_conv": ["v0.12.0"],
    "ppx_hash": ["v0.12.0", ["ppx_hash.runtime-lib"]],
    "ppx_inline_test": ["v0.12.0", ["ppx_inline_test.runtime-lib"]],
    "ppx_jane": ["v0.12.0"],
    "ppx_let": ["v0.12.0"],
    "ppx_module_timer": ["v0.12.0", ["ppx_module_timer.runtime"]],
    "ppx_sexp_conv": ["v0.12.0", ["ppx_sexp_conv.runtime-lib"]],
    "ppx_tools": ["5.1+4.06.0"],
    "ppxlib": ["0.8.1", ["ppxlib.metaquot"]],
    "yojson": ["1.7.0"]
}

opam = struct(
    opam_version = "2.0",
    switches  = {
        "mina-0.1.0": struct(
            default  = True,
            compiler = "4.07.1",
            packages = PACKAGES
        ),
        "4.07.1": struct(
            compiler = "4.07.1",
            packages = PACKAGES
        ),
    }
)
