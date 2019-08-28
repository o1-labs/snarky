#include "cuda_prover_piecewise.hpp"

#include "libsnark/zk_proof_systems/ppzksnark/r1cs_gg_ppzksnark/r1cs_gg_ppzksnark.hpp"

template<typename ppT>
void copy_fq(void** output, libff::Fq<ppT> x) {
  std::memcpy(*output, (void *) x.mont_repr.data, num_limbs * sizeof(mp_size_t));
  *output += num_limbs * sizeof(mp_size_t);
}

template<typename ppT>
void copy_fr(void** output, Fr<ppT> x) {
  std::memcpy(*output, (void *) x.mont_repr.data, num_limbs * sizeof(mp_size_t));
  *output += num_limbs * sizeof(mp_size_t);
}

template<typename ppT>
void copy_fqe(void** output, libff::Fqe<ppT> x) {
  std::vector<Fq<ppT>> v = x.all_base_field_elements();
  size_t deg = Fqe<ppT>::extension_degree();
  for (size_t i = 0; i < deg; ++i) {
    copy_fq<ppT>(output, v[i]);
  }
}

template<typename ppT>
void copy_g1(void** output, libff::G1<ppT> g) {
  if (g.is_zero())  {
    copy_fq<ppT>(output, Fq<ppT>::zero());
    copy_fq<ppT>(output, Fq<ppT>::zero());
    return;
  }

  g.to_affine_coordinates();
  copy_fq<ppT>(output, g.X());
  copy_fq<ppT>(output, g.Y());
}

template<typename ppT>
void copy_g2(void** output, libff::G2<ppT> g) {
  if (g.is_zero())  {
    copy_fqe<ppT>(output, Fqe<ppT>::zero());
    copy_fqe<ppT>(output, Fqe<ppT>::zero());
    return;
  }

  g.to_affine_coordinates();
  copy_fqe<ppT>(output, g.X());
  copy_fqe<ppT>(output, g.Y());
}

template<typename ppT>
void copy_g1_vec(void** output, std::vector<libff::G1<ppT>> &g) {
    for (size_t i = 0; i < g.size(); ++i) {
        copy_g1<ppT>(output, g[i]);
    }
}

template<typename ppT>
void copy_g2_vec(void** output, std::vector<libff::G2<ppT>> &g) {
    for (size_t i = 0; i < g.size(); ++i) {
        copy_g2<ppT>(output, g[i]);
    }
}

template<typename ppT>
void copy_fr_vec(void** output, std::vector<libff::Fr<ppT>> &f) {
    for (size_t i = 0; i < f.size(); ++i) {
        copy_fr<ppT>(output, f[i]);
    }
}

extern "C" {

void cuda_release_var_ptr(var *x) {
    CudaFree a;
    a(x);
}

libsnark::r1cs_gg_ppzksnark_proof<libff::mnt4753_pp> *mnt4753_cuda_make_proof(
        // const var *A_mults,
        const var *B1_mults,
        const var *B2_mults,
        const var *L_mults,
        std::vector<libff::Fr<libff::mnt4753_pp>> *public_input,
        std::vector<libff::Fr<libff::mnt4753_pp>> *auxiliary_input,
        libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt4753_pp> *pk)
{
    mnt4753_libsnark::groth16_input inputs(
        public_input,
        auxiliary_input,
        &pk->constraint_system);

    size_t constexpr fr_raw_size = num_limbs * sizeof(mp_size_t);
    var *w = (var *) malloc(fr_raw_size * (public_input->size() + auxiliary_input->size() + 1));
    void *w_out = (void *) w;
    copy_fr<libff::mnt4753_pp>(&w_out, libff::Fr<libff::mnt4753_pp>::one());
    copy_fr_vec<libff::mnt4753_pp>(&w_out, *public_input);
    copy_fr_vec<libff::mnt4753_pp>(&w_out, *auxiliary_input);

    mnt4753_libsnark::groth16_params params(pk);
    mnt4753_libsnark::G1 *A_out = NULL, *C_out = NULL;
    mnt4753_libsnark::G2 *B_out = NULL;
    mnt4753_cuda_prove(
        &A_out,
        &B_out,
        &C_out,
        pk->constraint_system.primary_input_size,
        params.d,
        params.m,
        w,
        // A_mults,
        B1_mults,
        B2_mults,
        L_mults,
        &params,
        &inputs);

    free(w);

    const libff::Fr<libff::mnt4753_pp> r = inputs.r;

    const libff::Fr<libff::mnt4753_pp> s = libff::Fr<libff::mnt4753_pp>::random_element();

    /* A = alpha + sum_i(a_i*A_i(t)) + r*delta */
    libff::G1<libff::mnt4753_pp> g1_A = pk->alpha_g1 + A_out->data + r * pk->delta_g1;

    /* B = beta + sum_i(a_i*B_i(t)) + s*delta */
    libff::G2<libff::mnt4753_pp> g2_B = pk->beta_g2 + B_out->data + s * pk->delta_g2;

    /* C = sum_i(a_i*((beta*A_i(t) + alpha*B_i(t) + C_i(t)) + H(t)*Z(t))/delta) + A*s + r*b - r*s*delta */
    libff::G1<libff::mnt4753_pp> g1_C = C_out->data + s * g1_A + r * pk->beta_g1;

    libsnark::r1cs_gg_ppzksnark_proof<libff::mnt4753_pp> *proof = new libsnark::r1cs_gg_ppzksnark_proof<libff::mnt4753_pp>(std::move(g1_A), std::move(g2_B), std::move(g1_C));

    return proof;
}

mnt4753_libsnark::groth16_params *mnt4753_groth16_params(
        const char *filename)
{
    FILE *params = fopen(filename, "r");
    size_t d = read_size_t(params);
    size_t m = read_size_t(params);
    rewind(params);
    mnt4753_libsnark::groth16_params *ret = mnt4753_libsnark::read_params(params, d, m);
    fclose(params);
    return ret;
}

mnt4753_libsnark::groth16_input *mnt4753_groth16_inputs(
        size_t d,
        size_t m,
        const char *filename)
{
    FILE *inputs = fopen(filename, "r");
    rewind(inputs);
    mnt4753_libsnark::groth16_input *ret = mnt4753_libsnark::read_input(inputs, d, m);
    fclose(inputs);
    return ret;
}

size_t mnt4753_params_d(mnt4753_libsnark::groth16_params *params)
{
    return params->d;
}
size_t mnt4753_params_m(mnt4753_libsnark::groth16_params *params)
{
    return params->m;
}

var *mnt4753_load_scalars(const size_t m, const char *filename)
{
    FILE *inputs = fopen(filename, "r");
    auto w_ = load_scalars(m + 1, inputs);
    fclose(inputs);
    return w_.release();
}

const void *mnt4753_get_input_witness(mnt4753_libsnark::groth16_input *inputs)
{
    return inputs->w.get();
}

var *mnt4753_reduce_g1_vector(
    std::vector<libff::G1<libff::mnt4753_pp>> *vec)
{
    size_t constexpr g1_raw_size = num_limbs * sizeof(mp_size_t) * 2;
    void *ret = (var *) malloc(g1_raw_size * vec->size());
    void *ret_write = ret;
    copy_g1_vec<libff::mnt4753_pp>(&ret_write, *vec);
    return (var *)ret;
}

var *mnt4753_reduce_g2_vector(
    std::vector<libff::G2<libff::mnt4753_pp>> *vec)
{
    size_t constexpr g2_raw_size = num_limbs * sizeof(mp_size_t) * 2;
    void *ret = (void *) malloc(g2_raw_size * vec->size() * Fqe<libff::mnt4753_pp>::extension_degree());
    void *ret_write = ret;
    copy_g2_vec<libff::mnt4753_pp>(&ret_write, *vec);
    return (var *)ret;
}

std::vector<libff::G1<libff::mnt4753_pp>> *mnt4753_preprocess_A(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt4753_pp> *pk)
{
    std::vector<libff::G1<libff::mnt4753_pp>> *ret = new std::vector<libff::G1<libff::mnt4753_pp>>();
    generate_multiples<libff::G1<libff::mnt4753_pp>>(C, pk->A_query, ret);
    return ret;
}

std::vector<libff::G1<libff::mnt4753_pp>> *mnt4753_preprocess_B1(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt4753_pp> *pk)
{
    std::vector<libff::G1<libff::mnt4753_pp>> *ret = new std::vector<libff::G1<libff::mnt4753_pp>>();
    std::vector<libff::G1<libff::mnt4753_pp>> B1;
    for (size_t i = 0; i < pk->B_query.size(); i++) {
        B1.emplace_back(pk->B_query[i].h);
    }
    /* Pad to the size of A_query. */
    for (size_t i = pk->B_query.size(); i < pk->A_query.size(); i++) {
        B1.emplace_back(pk->B_query[i].h);
    }
    generate_multiples<libff::G1<libff::mnt4753_pp>>(C, B1, ret);
    return ret;
}

std::vector<libff::G2<libff::mnt4753_pp>> *mnt4753_preprocess_B2(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt4753_pp> *pk)
{
    std::vector<libff::G2<libff::mnt4753_pp>> *ret = new std::vector<libff::G2<libff::mnt4753_pp>>();
    std::vector<libff::G2<libff::mnt4753_pp>> B2;
    for (size_t i = 0; i < pk->B_query.size(); i++) {
        B2.emplace_back(pk->B_query[i].g);
    }
    /* Pad to the size of A_query. */
    for (size_t i = pk->B_query.size(); i < pk->A_query.size(); i++) {
        B2.emplace_back(pk->B_query[i].g);
    }
    generate_multiples<libff::G2<libff::mnt4753_pp>>(C, B2, ret);
    return ret;
}

std::vector<libff::G1<libff::mnt4753_pp>> *mnt4753_preprocess_L(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt4753_pp> *pk)
{
    std::vector<libff::G1<libff::mnt4753_pp>> *ret = new std::vector<libff::G1<libff::mnt4753_pp>>();
    generate_multiples<libff::G1<libff::mnt4753_pp>>(C, pk->L_query, ret);
    return ret;
}

std::vector<libff::G1<libff::mnt4753_pp>> *mnt4753_preprocess_H(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt4753_pp> *pk)
{
    std::vector<libff::G1<libff::mnt4753_pp>> *ret = new std::vector<libff::G1<libff::mnt4753_pp>>();
    generate_multiples<libff::G1<libff::mnt4753_pp>>(C, pk->H_query, ret);
    return ret;
}

libsnark::r1cs_gg_ppzksnark_proof<libff::mnt6753_pp> *mnt6753_cuda_make_proof(
        // const var *A_mults,
        const var *B1_mults,
        const var *B2_mults,
        const var *L_mults,
        std::vector<libff::Fr<libff::mnt6753_pp>> *public_input,
        std::vector<libff::Fr<libff::mnt6753_pp>> *auxiliary_input,
        libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt6753_pp> *pk)
{
    mnt6753_libsnark::groth16_input inputs(
        public_input,
        auxiliary_input,
        &pk->constraint_system);

    size_t constexpr fr_raw_size = num_limbs * sizeof(mp_size_t);
    var *w = (var *) malloc(fr_raw_size * (public_input->size() + auxiliary_input->size() + 1));
    void *w_out = (void *) w;
    copy_fr<libff::mnt6753_pp>(&w_out, libff::Fr<libff::mnt6753_pp>::one());
    copy_fr_vec<libff::mnt6753_pp>(&w_out, *public_input);
    copy_fr_vec<libff::mnt6753_pp>(&w_out, *auxiliary_input);

    mnt6753_libsnark::groth16_params params(pk);
    mnt6753_libsnark::G1 *A_out = NULL, *C_out = NULL;
    mnt6753_libsnark::G2 *B_out = NULL;
    mnt6753_cuda_prove(
        &A_out,
        &B_out,
        &C_out,
        pk->constraint_system.primary_input_size,
        params.d,
        params.m,
        w,
        // A_mults,
        B1_mults,
        B2_mults,
        L_mults,
        &params,
        &inputs);

    free(w);

    const libff::Fr<libff::mnt6753_pp> r = inputs.r;

    const libff::Fr<libff::mnt6753_pp> s = libff::Fr<libff::mnt6753_pp>::random_element();

    /* A = alpha + sum_i(a_i*A_i(t)) + r*delta */
    libff::G1<libff::mnt6753_pp> g1_A = pk->alpha_g1 + A_out->data + r * pk->delta_g1;

    /* B = beta + sum_i(a_i*B_i(t)) + s*delta */
    libff::G2<libff::mnt6753_pp> g2_B = pk->beta_g2 + B_out->data + s * pk->delta_g2;

    /* C = sum_i(a_i*((beta*A_i(t) + alpha*B_i(t) + C_i(t)) + H(t)*Z(t))/delta) + A*s + r*b - r*s*delta */
    libff::G1<libff::mnt6753_pp> g1_C = C_out->data + s * g1_A + r * pk->beta_g1;

    libsnark::r1cs_gg_ppzksnark_proof<libff::mnt6753_pp> *proof = new libsnark::r1cs_gg_ppzksnark_proof<libff::mnt6753_pp>(std::move(g1_A), std::move(g2_B), std::move(g1_C));

    return proof;
}

mnt6753_libsnark::groth16_params *mnt6753_groth16_params(
        const char *filename)
{
    FILE *params = fopen(filename, "r");
    size_t d = read_size_t(params);
    size_t m = read_size_t(params);
    rewind(params);
    mnt6753_libsnark::groth16_params *ret = mnt6753_libsnark::read_params(params, d, m);
    fclose(params);
    return ret;
}

mnt6753_libsnark::groth16_input *mnt6753_groth16_inputs(
        size_t d,
        size_t m,
        const char *filename)
{
    FILE *inputs = fopen(filename, "r");
    rewind(inputs);
    mnt6753_libsnark::groth16_input *ret = mnt6753_libsnark::read_input(inputs, d, m);
    fclose(inputs);
    return ret;
}

size_t mnt6753_params_d(mnt6753_libsnark::groth16_params *params)
{
    return params->d;
}
size_t mnt6753_params_m(mnt6753_libsnark::groth16_params *params)
{
    return params->m;
}

var *mnt6753_load_scalars(const size_t m, const char *filename)
{
    FILE *inputs = fopen(filename, "r");
    auto w_ = load_scalars(m + 1, inputs);
    fclose(inputs);
    return w_.release();
}

const void *mnt6753_get_input_witness(mnt6753_libsnark::groth16_input *inputs)
{
    return inputs->w.get();
}

var *mnt6753_reduce_g1_vector(
    std::vector<libff::G1<libff::mnt6753_pp>> *vec)
{
    size_t constexpr g1_raw_size = num_limbs * sizeof(mp_size_t) * 2;
    void *ret = (var *) malloc(g1_raw_size * vec->size());
    void *ret_write = ret;
    copy_g1_vec<libff::mnt6753_pp>(&ret_write, *vec);
    return (var *)ret;
}

var *mnt6753_reduce_g2_vector(
    std::vector<libff::G2<libff::mnt6753_pp>> *vec)
{
    size_t constexpr g2_raw_size = num_limbs * sizeof(mp_size_t) * 2;
    void *ret = (void *) malloc(g2_raw_size * vec->size() * Fqe<libff::mnt6753_pp>::extension_degree());
    void *ret_write = ret;
    copy_g2_vec<libff::mnt6753_pp>(&ret_write, *vec);
    return (var *)ret;
}

std::vector<libff::G1<libff::mnt6753_pp>> *mnt6753_preprocess_A(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt6753_pp> *pk)
{
    std::vector<libff::G1<libff::mnt6753_pp>> *ret = new std::vector<libff::G1<libff::mnt6753_pp>>();
    generate_multiples<libff::G1<libff::mnt6753_pp>>(C, pk->A_query, ret);
    return ret;
}

std::vector<libff::G1<libff::mnt6753_pp>> *mnt6753_preprocess_B1(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt6753_pp> *pk)
{
    std::vector<libff::G1<libff::mnt6753_pp>> *ret = new std::vector<libff::G1<libff::mnt6753_pp>>();
    std::vector<libff::G1<libff::mnt6753_pp>> B1;
    for (size_t i = 0; i < pk->B_query.size(); i++) {
        B1.emplace_back(pk->B_query[i].h);
    }
    /* Pad to the size of A_query. */
    for (size_t i = pk->B_query.size(); i < pk->A_query.size(); i++) {
        B1.emplace_back(pk->B_query[i].h);
    }
    generate_multiples<libff::G1<libff::mnt6753_pp>>(C, B1, ret);
    return ret;
}

std::vector<libff::G2<libff::mnt6753_pp>> *mnt6753_preprocess_B2(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt6753_pp> *pk)
{
    std::vector<libff::G2<libff::mnt6753_pp>> *ret = new std::vector<libff::G2<libff::mnt6753_pp>>();
    std::vector<libff::G2<libff::mnt6753_pp>> B2;
    for (size_t i = 0; i < pk->B_query.size(); i++) {
        B2.emplace_back(pk->B_query[i].g);
    }
    /* Pad to the size of A_query. */
    for (size_t i = pk->B_query.size(); i < pk->A_query.size(); i++) {
        B2.emplace_back(pk->B_query[i].g);
    }
    generate_multiples<libff::G2<libff::mnt6753_pp>>(C, B2, ret);
    return ret;
}

std::vector<libff::G1<libff::mnt6753_pp>> *mnt6753_preprocess_L(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt6753_pp> *pk)
{
    std::vector<libff::G1<libff::mnt6753_pp>> *ret = new std::vector<libff::G1<libff::mnt6753_pp>>();
    generate_multiples<libff::G1<libff::mnt6753_pp>>(C, pk->L_query, ret);
    return ret;
}

std::vector<libff::G1<libff::mnt6753_pp>> *mnt6753_preprocess_H(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt6753_pp> *pk)
{
    std::vector<libff::G1<libff::mnt6753_pp>> *ret = new std::vector<libff::G1<libff::mnt6753_pp>>();
    generate_multiples<libff::G1<libff::mnt6753_pp>>(C, pk->H_query, ret);
    return ret;
}

}
