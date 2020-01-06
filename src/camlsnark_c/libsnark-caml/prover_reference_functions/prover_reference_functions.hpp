#pragma once

#include <vector>

typedef std::uint64_t var;

class mnt4753_libsnark {
public:
  class groth16_input;

  class groth16_params;

  struct evaluation_domain;

  struct field;

  struct G1;

  struct G2;

    static G1 *read_pt_ECp(const var *mem);
    static G2 *read_pt_ECpe(const var *mem);

  struct vector_Fr;

  struct vector_G1;

  struct vector_G2;

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
  class groth16_input;

  class groth16_params;

  struct evaluation_domain;

  struct field;

  struct G1;

  struct G2;

    static G1 *read_pt_ECp(const var *mem);
    static G2 *read_pt_ECpe(const var *mem);

  struct vector_Fr;

  struct vector_G1;

  struct vector_G2;

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
