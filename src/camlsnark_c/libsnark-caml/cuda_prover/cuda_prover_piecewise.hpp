#pragma once
#include <string>
#include <chrono>

#define NDEBUG 1

#include <prover_reference_libsnark.hpp>

void mnt4753_cuda_prove(
        mnt4753_libsnark::G1 **A_out,
        mnt4753_libsnark::G2 **B_out,
        mnt4753_libsnark::G1 **C_out,
        size_t primary_input_size,
        size_t d,
        size_t m,
        const var *w,
        // const var *A_mults,
        const var *B1_mults,
        const var *B2_mults,
        const var *L_mults,
        mnt4753_libsnark::groth16_params *params,
        mnt4753_libsnark::groth16_input *inputs);

void mnt6753_cuda_prove(
        mnt6753_libsnark::G1 **A_out,
        mnt6753_libsnark::G2 **B_out,
        mnt6753_libsnark::G1 **C_out,
        size_t primary_input_size,
        size_t d,
        size_t m,
        const var *w,
        // const var *A_mults,
        const var *B1_mults,
        const var *B2_mults,
        const var *L_mults,
        mnt6753_libsnark::groth16_params *params,
        mnt6753_libsnark::groth16_input *inputs);

struct CudaFree {
    void operator()(var *mem);
};

typedef std::unique_ptr<var, CudaFree> var_ptr;

var_ptr load_scalars(size_t n, FILE *inputs);

extern "C" {

var *mnt4753_cuda_load_points_affine(size_t n, FILE *inputs);

var *mnt4753_cuda_load_extension_points_affine(size_t n, FILE *inputs);

var *mnt6753_cuda_load_points_affine(size_t n, FILE *inputs);

var *mnt6753_cuda_load_extension_points_affine(size_t n, FILE *inputs);

}
