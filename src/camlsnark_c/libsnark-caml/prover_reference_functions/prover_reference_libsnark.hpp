#pragma once

// Header exposing the full structure underlying
// mnt4753_libsnark/mnt6753_libsnark.
//
// The version in cuda_reference_functions.hpp is safe to pass to CUDA, but
// makes it impossible to retrieve the underlying libsnark/libff objects; this
// header can be used to bridge the gap and re-extract the objects once CUDA is
// done with them.

#include <vector>
#include <libff/algebra/curves/mnt753/mnt4753/mnt4753_pp.hpp>
#include <libff/algebra/curves/mnt753/mnt6753/mnt6753_pp.hpp>
#include <omp.h>
#include <libsnark/serialization.hpp>

typedef std::uint64_t var;

class mnt4753_libsnark {
public:
  class groth16_input {
  public:
    std::shared_ptr<std::vector<Fr<mnt4753_pp>>> w;
    std::shared_ptr<std::vector<Fr<mnt4753_pp>>> ca, cb, cc;
    Fr<mnt4753_pp> r;

    groth16_input(FILE *inputs, size_t d, size_t m);

    groth16_input(const std::vector<Fr<mnt4753_pp>> *public_input,
                  const std::vector<Fr<mnt4753_pp>> *auxiliary_input,
                  const r1cs_constraint_system<Fr<mnt4753_pp>> *r1cs);
  };

  class groth16_params {
  public:
    size_t d;
    size_t m;
    std::shared_ptr<std::vector<libff::G1<mnt4753_pp>>> A, B1, L, H;
    std::shared_ptr<std::vector<libff::G2<mnt4753_pp>>> B2;

    groth16_params(FILE *params, size_t dd, size_t mm);

    groth16_params(libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt4753_pp> *pk);
  };

  struct evaluation_domain {
    std::shared_ptr<libfqfft::evaluation_domain<Fr<mnt4753_pp>>> data;
  };

  struct field {
    Fr<mnt4753_pp> data;
  };

  struct G1 {
    libff::G1<mnt4753_pp> data;
  };

  struct G2 {
    libff::G2<mnt4753_pp> data;
  };

  struct vector_Fr {
    std::shared_ptr<std::vector<Fr<mnt4753_pp>>> data;
    size_t offset;
  };

  struct vector_G1 {
    std::shared_ptr<std::vector<libff::G1<mnt4753_pp>>> data;
  };

  struct vector_G2 {
    std::shared_ptr<std::vector<libff::G2<mnt4753_pp>>> data;
  };

    static G1 *read_pt_ECp(const var *mem);
    static G2 *read_pt_ECpe(const var *mem);

  static void init_public_params();

  static void print_G1(G1 *a);
  static void print_G1(vector_G1 *a);
  static void print_G2(G2 *a);
  static void print_G2(vector_G2 *a, size_t i=0);

  static evaluation_domain *get_evaluation_domain(size_t d);

  static int G1_equal(G1 *a, G1 *b);
  static int G2_equal(G2 *a, G2 *b);
  static G1 *G1_add(G1 *a, G1 *b);
  static G1 *G1_scale(field *a, G1 *b);

  static void vector_Fr_muleq(vector_Fr *a, vector_Fr *b, size_t size);
  static void vector_Fr_subeq(vector_Fr *a, vector_Fr *b, size_t size);
  static vector_Fr *vector_Fr_offset(vector_Fr *a, size_t offset);
  static vector_G2 *vector_G2_offset(vector_G2 *a, size_t offset);
  static void vector_Fr_copy_into(vector_Fr *src, vector_Fr *dst,
                                  size_t length);
  static vector_Fr *vector_Fr_zeros(size_t length);

  static void domain_iFFT(evaluation_domain *domain, vector_Fr *a);
  static void domain_cosetFFT(evaluation_domain *domain, vector_Fr *a);
  static void domain_icosetFFT(evaluation_domain *domain, vector_Fr *a);
  static void domain_divide_by_Z_on_coset(evaluation_domain *domain,
                                          vector_Fr *a);
  static size_t domain_get_m(evaluation_domain *domain);

  static G1 *multiexp_G1(vector_Fr *scalar_start, vector_G1 *g_start,
                         size_t length);
  static G2 *multiexp_G2(vector_Fr *scalar_start, vector_G2 *g_start,
                         size_t length);

  static groth16_input *read_input(FILE *inputs, size_t d, size_t m);

  static vector_Fr *input_w(groth16_input *input);
  static vector_Fr *input_ca(groth16_input *input);
  static vector_Fr *input_cb(groth16_input *input);
  static vector_Fr *input_cc(groth16_input *input);
  static field *input_r(groth16_input *input);

  static groth16_params *read_params(FILE *params, size_t d, size_t m);

  static size_t params_d(groth16_params *params);
  static size_t params_m(groth16_params *params);
  static vector_G1 *params_A(groth16_params *params);
  static vector_G1 *params_B1(groth16_params *params);
  static vector_G1 *params_L(groth16_params *params);
  static vector_G1 *params_H(groth16_params *params);
  static vector_G2 *params_B2(groth16_params *params);

  static void delete_G1(G1 *a);
  static void delete_G2(G2 *a);
  static void delete_vector_Fr(vector_Fr *a);
  static void delete_vector_G1(vector_G1 *a);
  static void delete_vector_G2(vector_G2 *a);
  static void delete_groth16_input(groth16_input *a);
  static void delete_groth16_params(groth16_params *a);
  static void delete_evaluation_domain(evaluation_domain *a);

  static void groth16_output_write(G1 *A, G2 *B, G1 *C,
                                   const char *output_path);
};
class mnt6753_libsnark {
public:
  class groth16_input {
  public:
    std::shared_ptr<std::vector<Fr<mnt6753_pp>>> w;
    std::shared_ptr<std::vector<Fr<mnt6753_pp>>> ca, cb, cc;
    Fr<mnt6753_pp> r;

    groth16_input(FILE *inputs, size_t d, size_t m);

    groth16_input(const std::vector<Fr<mnt6753_pp>> *public_input,
                  const std::vector<Fr<mnt6753_pp>> *auxiliary_input,
                  const r1cs_constraint_system<Fr<mnt6753_pp>> *r1cs);
  };

  class groth16_params {
  public:
    size_t d;
    size_t m;
    std::shared_ptr<std::vector<libff::G1<mnt6753_pp>>> A, B1, L, H;
    std::shared_ptr<std::vector<libff::G2<mnt6753_pp>>> B2;

    groth16_params(FILE *params, size_t dd, size_t mm);

    groth16_params(libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt6753_pp> *pk);
  };

  struct evaluation_domain {
    std::shared_ptr<libfqfft::evaluation_domain<Fr<mnt6753_pp>>> data;
  };

  struct field {
    Fr<mnt6753_pp> data;
  };

  struct G1 {
    libff::G1<mnt6753_pp> data;
  };

  struct G2 {
    libff::G2<mnt6753_pp> data;
  };

  struct vector_Fr {
    std::shared_ptr<std::vector<Fr<mnt6753_pp>>> data;
    size_t offset;
  };

  struct vector_G1 {
    std::shared_ptr<std::vector<libff::G1<mnt6753_pp>>> data;
  };

  struct vector_G2 {
    std::shared_ptr<std::vector<libff::G2<mnt6753_pp>>> data;
  };

    static G1 *read_pt_ECp(const var *mem);
    static G2 *read_pt_ECpe(const var *mem);

  static void init_public_params();

  static void print_G1(G1 *a);
  static void print_G1(vector_G1 *a);
  static void print_G2(G2 *a);
  static void print_G2(vector_G2 *a, size_t i=0);

  static evaluation_domain *get_evaluation_domain(size_t d);

  static int G1_equal(G1 *a, G1 *b);
  static int G2_equal(G2 *a, G2 *b);
  static G1 *G1_add(G1 *a, G1 *b);
  static G1 *G1_scale(field *a, G1 *b);

  static void vector_Fr_muleq(vector_Fr *a, vector_Fr *b, size_t size);
  static void vector_Fr_subeq(vector_Fr *a, vector_Fr *b, size_t size);
  static vector_Fr *vector_Fr_offset(vector_Fr *a, size_t offset);
  static vector_G2 *vector_G2_offset(vector_G2 *a, size_t offset);
  static void vector_Fr_copy_into(vector_Fr *src, vector_Fr *dst,
                                  size_t length);
  static vector_Fr *vector_Fr_zeros(size_t length);

  static void domain_iFFT(evaluation_domain *domain, vector_Fr *a);
  static void domain_cosetFFT(evaluation_domain *domain, vector_Fr *a);
  static void domain_icosetFFT(evaluation_domain *domain, vector_Fr *a);
  static void domain_divide_by_Z_on_coset(evaluation_domain *domain,
                                          vector_Fr *a);
  static size_t domain_get_m(evaluation_domain *domain);

  static G1 *multiexp_G1(vector_Fr *scalar_start, vector_G1 *g_start,
                         size_t length);
  static G2 *multiexp_G2(vector_Fr *scalar_start, vector_G2 *g_start,
                         size_t length);

  static groth16_input *read_input(FILE *inputs, size_t d, size_t m);

  static vector_Fr *input_w(groth16_input *input);
  static vector_Fr *input_ca(groth16_input *input);
  static vector_Fr *input_cb(groth16_input *input);
  static vector_Fr *input_cc(groth16_input *input);
  static field *input_r(groth16_input *input);

  static groth16_params *read_params(FILE *params, size_t d, size_t m);

  static size_t params_d(groth16_params *params);
  static size_t params_m(groth16_params *params);
  static vector_G1 *params_A(groth16_params *params);
  static vector_G1 *params_B1(groth16_params *params);
  static vector_G1 *params_L(groth16_params *params);
  static vector_G1 *params_H(groth16_params *params);
  static vector_G2 *params_B2(groth16_params *params);

  static void delete_G1(G1 *a);
  static void delete_G2(G2 *a);
  static void delete_vector_Fr(vector_Fr *a);
  static void delete_vector_G1(vector_G1 *a);
  static void delete_vector_G2(vector_G2 *a);
  static void delete_groth16_input(groth16_input *a);
  static void delete_groth16_params(groth16_params *a);
  static void delete_evaluation_domain(evaluation_domain *a);

  static void groth16_output_write(G1 *A, G2 *B, G1 *C,
                                   const char *output_path);
};

template<typename G>
void generate_multiples(int C, const std::vector<G> &vec, std::vector<G> *multiples) {
    // If vec = [P0, ..., Pn], then multiples holds an array
    //
    // [    P0, ...,     Pn,
    //     2P0, ...,    2Pn,
    //     3P0, ...,    3Pn,
    //          ...,
    //  2^(C-1) P0, ..., 2^(C-1) Pn]
    size_t len = vec.size();
    multiples->resize(len * ((1U << C) - 1));
    std::copy(vec.begin(), vec.end(), multiples->begin());

    for (size_t i = 1; i < (1U << C) - 1; ++i) {
        size_t prev_row_offset = (i-1)*len;
        size_t curr_row_offset = i*len;
#ifdef MULTICORE
#pragma omp parallel for
#endif
        for (size_t j = 0; j < len; ++j)
           (*multiples)[curr_row_offset + j] = vec[j] + (*multiples)[prev_row_offset + j];
    }

    if (multiples->size() != ((1U << C) - 1)*len) {
        fprintf(stderr, "Broken preprocessing table: got %zu, expected %zu\n",
                multiples->size(), ((1U << C) - 1) * len);
        abort();
    }
}
