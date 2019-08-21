#include <cassert>
#include <cstdio>
#include <fstream>
#include <libff/algebra/scalar_multiplication/multiexp.hpp>
#include <libff/common/profiling.hpp>
#include <libff/common/rng.hpp>
#include <libff/common/utils.hpp>

#include <libsnark/knowledge_commitment/kc_multiexp.hpp>
#include <libsnark/knowledge_commitment/knowledge_commitment.hpp>
#include <libsnark/reductions/r1cs_to_qap/r1cs_to_qap.hpp>

#include <libsnark/zk_proof_systems/ppzksnark/r1cs_gg_ppzksnark/r1cs_gg_ppzksnark.hpp>

#include <libfqfft/evaluation_domain/domains/basic_radix2_domain.hpp>

#include "prover_reference_libsnark.hpp"

using namespace libff;
using namespace libsnark;

const multi_exp_method method = multi_exp_method_bos_coster;
//const multi_exp_method method = multi_exp_method_BDLO12;


template< typename EC >
struct ff_dict;

template<>
struct ff_dict<mnt4753_libsnark::G1> {
    typedef Fq<mnt4753_pp> FF;
    typedef G1<mnt4753_pp> EC;
    static constexpr size_t ff_bytes = mnt4753_q_limbs * sizeof(mp_limb_t);

    static FF read_ff(const char *&mem) {
        FF x;
        memcpy(x.mont_repr.data, mem, ff_bytes);
        mem += ff_bytes;
        return x;
    }

    static void write_ff(char *&mem, const FF &x) {
        memcpy(mem, x.mont_repr.data, ff_bytes);
        mem += ff_bytes;
    }
};

template<>
struct ff_dict<mnt4753_libsnark::G2> {
    typedef Fqe<mnt4753_pp> FF;
    typedef G2<mnt4753_pp> EC;
    static constexpr size_t ff_bytes = 2 * ff_dict<mnt4753_libsnark::G1>::ff_bytes;

    static FF read_ff(const char *&mem) {
        auto c0 = ff_dict<mnt4753_libsnark::G1>::read_ff(mem);
        auto c1 = ff_dict<mnt4753_libsnark::G1>::read_ff(mem);
        return FF(c0, c1);
    }

    static void write_ff(char *&mem, const FF &x) {
        ff_dict<mnt4753_libsnark::G1>::write_ff(mem, x.c0);
        ff_dict<mnt4753_libsnark::G1>::write_ff(mem, x.c1);
    }
};

template<>
struct ff_dict<mnt6753_libsnark::G1> {
    typedef Fq<mnt6753_pp> FF;
    typedef G1<mnt6753_pp> EC;
    static constexpr size_t ff_bytes = mnt6753_q_limbs * sizeof(mp_limb_t);

    static FF read_ff(const char *&mem) {
        FF x;
        memcpy(x.mont_repr.data, mem, ff_bytes);
        mem += ff_bytes;
        return x;
    }
    static void write_ff(char *&mem, const FF &x) {
        memcpy(mem, x.mont_repr.data, ff_bytes);
        mem += ff_bytes;
    }
};

template<>
struct ff_dict<mnt6753_libsnark::G2> {
    typedef Fqe<mnt6753_pp> FF;
    typedef G2<mnt6753_pp> EC;
    static constexpr size_t ff_bytes = 3 * ff_dict<mnt6753_libsnark::G1>::ff_bytes;

    static FF read_ff(const char *&mem) {
        auto c0 = ff_dict<mnt6753_libsnark::G1>::read_ff(mem);
        auto c1 = ff_dict<mnt6753_libsnark::G1>::read_ff(mem);
        auto c2 = ff_dict<mnt6753_libsnark::G1>::read_ff(mem);
        return FF(c0, c1, c2);
    }
    static void write_ff(char *&mem, const FF &x) {
        ff_dict<mnt6753_libsnark::G1>::write_ff(mem, x.c0);
        ff_dict<mnt6753_libsnark::G1>::write_ff(mem, x.c1);
        ff_dict<mnt6753_libsnark::G1>::write_ff(mem, x.c2);
    }
};

template< typename EC >
typename ff_dict<EC>::EC read_pt(const char *&mem) {
    typedef ff_dict<EC> D;
    typedef typename D::FF FF;
    FF x = D::read_ff(mem);
    FF y = D::read_ff(mem);
    FF z = D::read_ff(mem);
    // Convert point representation from Jacobian to projective
    return typename D::EC(x * z, y, z * z * z);
}

template <typename G, typename Fr>
G multiexp(typename std::vector<Fr>::const_iterator scalar_start,
           typename std::vector<G>::const_iterator g_start, size_t length) {
#ifdef MULTICORE
  const size_t chunks =
      omp_get_max_threads(); // to override, set OMP_NUM_THREADS env var or call
                             // omp_set_num_threads()
#else
  const size_t chunks = 1;
#endif

  return libff::multi_exp_with_mixed_addition<G, Fr, method>(
      g_start, g_start + length, scalar_start, scalar_start + length, chunks);
}

mnt4753_libsnark::groth16_input::groth16_input(FILE *inputs, size_t d, size_t m) {
  w = std::make_shared<std::vector<libff::Fr<mnt4753_pp>>>(
      std::vector<libff::Fr<mnt4753_pp>>());
  ca = std::make_shared<std::vector<libff::Fr<mnt4753_pp>>>(
      std::vector<libff::Fr<mnt4753_pp>>());
  cb = std::make_shared<std::vector<libff::Fr<mnt4753_pp>>>(
      std::vector<libff::Fr<mnt4753_pp>>());
  cc = std::make_shared<std::vector<libff::Fr<mnt4753_pp>>>(
      std::vector<libff::Fr<mnt4753_pp>>());

  for (size_t i = 0; i < m + 1; ++i) {
    w->emplace_back(read_fr<mnt4753_pp>(inputs));
  }
  for (size_t i = 0; i < d + 1; ++i) {
    ca->emplace_back(read_fr<mnt4753_pp>(inputs));
  }
  for (size_t i = 0; i < d + 1; ++i) {
    cb->emplace_back(read_fr<mnt4753_pp>(inputs));
  }
  for (size_t i = 0; i < d + 1; ++i) {
    cc->emplace_back(read_fr<mnt4753_pp>(inputs));
  }

  r = read_fr<mnt4753_pp>(inputs);
}

mnt4753_libsnark::groth16_input::groth16_input(
    const std::vector<Fr<mnt4753_pp>> *public_input,
    const std::vector<Fr<mnt4753_pp>> *auxiliary_input,
    const r1cs_constraint_system<Fr<mnt4753_pp>> *r1cs)
{
  w = std::make_shared<std::vector<libff::Fr<mnt4753_pp>>>(
      std::vector<libff::Fr<mnt4753_pp>>());
  r = libff::Fr<libff::mnt4753_pp>::random_element();

  w->emplace_back(Fr<mnt4753_pp>::one());
  w->insert(w->end(), public_input->begin(), public_input->end());
  w->insert(w->end(), auxiliary_input->begin(), auxiliary_input->end());

  r1cs_variable_assignment<Fr<mnt4753_pp>> variable_assignment(*public_input);
  variable_assignment.insert(variable_assignment.end(), auxiliary_input->begin(), auxiliary_input->end());

  // Retrieve the actual size of the evaluation domain that will be used for
  // the FFTs, which may be bigger than d + 1.
  auto domain = libfqfft::get_evaluation_domain<libff::Fr<mnt4753_pp>>(r1cs->num_constraints() + r1cs->primary_input_size + 1);
  size_t d_plus_1 = domain->m;

  ca = std::make_shared<std::vector<libff::Fr<mnt4753_pp>>>(
      std::vector<libff::Fr<mnt4753_pp>>(d_plus_1, Fr<mnt4753_pp>::zero()));
  cb = std::make_shared<std::vector<libff::Fr<mnt4753_pp>>>(
      std::vector<libff::Fr<mnt4753_pp>>(d_plus_1, Fr<mnt4753_pp>::zero()));
  cc = std::make_shared<std::vector<libff::Fr<mnt4753_pp>>>(
      std::vector<libff::Fr<mnt4753_pp>>(d_plus_1, Fr<mnt4753_pp>::zero()));

  for (size_t i = 0; i <= r1cs->primary_input_size; ++i)
  {
      (*ca)[i+r1cs->num_constraints()] = (i > 0 ? (variable_assignment)[i-1] : Fr<mnt4753_pp>::one());
  }
  for (size_t i = 0; i < r1cs->num_constraints(); ++i)
  {
      (*ca)[i] += r1cs->constraints[i].a.evaluate(variable_assignment);
      (*cb)[i] += r1cs->constraints[i].b.evaluate(variable_assignment);
      (*cc)[i] += r1cs->constraints[i].c.evaluate(variable_assignment);
  }
}

mnt4753_libsnark::groth16_params::groth16_params(FILE *params, size_t dd, size_t mm) {
  d = read_size_t(params);
  m = read_size_t(params);
  if (d != dd || m != mm) {
      fputs("Bad size read", stderr);
      abort();
  }

  A = std::make_shared<std::vector<libff::G1<mnt4753_pp>>>(
      std::vector<libff::G1<mnt4753_pp>>());
  B1 = std::make_shared<std::vector<libff::G1<mnt4753_pp>>>(
      std::vector<libff::G1<mnt4753_pp>>());
  L = std::make_shared<std::vector<libff::G1<mnt4753_pp>>>(
      std::vector<libff::G1<mnt4753_pp>>());
  H = std::make_shared<std::vector<libff::G1<mnt4753_pp>>>(
      std::vector<libff::G1<mnt4753_pp>>());
  B2 = std::make_shared<std::vector<libff::G2<mnt4753_pp>>>(
      std::vector<libff::G2<mnt4753_pp>>());
  for (size_t i = 0; i <= m; ++i) {
    A->emplace_back(read_g1<mnt4753_pp>(params));
  }
  for (size_t i = 0; i <= m; ++i) {
    B1->emplace_back(read_g1<mnt4753_pp>(params));
  }
  for (size_t i = 0; i <= m; ++i) {
    B2->emplace_back(read_g2<mnt4753_pp>(params));
  }
  for (size_t i = 0; i < m - 1; ++i) {
    L->emplace_back(read_g1<mnt4753_pp>(params));
  }
  for (size_t i = 0; i < d; ++i) {
    H->emplace_back(read_g1<mnt4753_pp>(params));
  }
}

mnt4753_libsnark::groth16_params::groth16_params(libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt4753_pp> *pk) {
  // Retrieve the actual size of the evaluation domain that will be used for
  // the FFTs, which may be bigger than d + 1.
  auto domain = libfqfft::get_evaluation_domain<libff::Fr<mnt4753_pp>>(pk->constraint_system.num_constraints() + pk->constraint_system.primary_input_size + 1);
  size_t d_plus_1 = domain->m;
  d = d_plus_1 - 1;

  m = pk->constraint_system.num_variables();
  A = std::make_shared<std::vector<libff::G1<mnt4753_pp>>>(
      std::vector<libff::G1<mnt4753_pp>>(pk->A_query));
  L = std::make_shared<std::vector<libff::G1<mnt4753_pp>>>(
      std::vector<libff::G1<mnt4753_pp>>(pk->L_query));
  H = std::make_shared<std::vector<libff::G1<mnt4753_pp>>>(
      std::vector<libff::G1<mnt4753_pp>>(pk->H_query));

  B1 = std::make_shared<std::vector<libff::G1<mnt4753_pp>>>(
      std::vector<libff::G1<mnt4753_pp>>());
  B2 = std::make_shared<std::vector<libff::G2<mnt4753_pp>>>(
      std::vector<libff::G2<mnt4753_pp>>());
  for (size_t i = 0; i <= m; ++i) {
    B1->emplace_back(pk->B_query[i].h);
    B2->emplace_back(pk->B_query[i].g);
  }
}

void mnt4753_libsnark::init_public_params() {
  mnt4753_pp::init_public_params();
}

void mnt4753_libsnark::print_G1(mnt4753_libsnark::G1 *a) { a->data.print(); }

void mnt4753_libsnark::print_G1(mnt4753_libsnark::vector_G1 *a) { a->data->at(0).print(); }

void mnt4753_libsnark::print_G2(mnt4753_libsnark::G2 *a) { a->data.print(); }

void mnt4753_libsnark::print_G2(mnt4753_libsnark::vector_G2 *a, size_t i) { a->data->at(i).print(); }

mnt4753_libsnark::evaluation_domain *
mnt4753_libsnark::get_evaluation_domain(size_t d) {
  return new evaluation_domain{
      .data = libfqfft::get_evaluation_domain<Fr<mnt4753_pp>>(d)};
}

int mnt4753_libsnark::G1_equal(mnt4753_libsnark::G1 *a, mnt4753_libsnark::G1 *b) {
  return a->data == b->data;
}

int mnt4753_libsnark::G2_equal(mnt4753_libsnark::G2 *a, mnt4753_libsnark::G2 *b) {
  return a->data == b->data;
}

mnt4753_libsnark::G1 *mnt4753_libsnark::G1_add(mnt4753_libsnark::G1 *a,
                                               mnt4753_libsnark::G1 *b) {
  return new mnt4753_libsnark::G1{.data = a->data + b->data};
}

mnt4753_libsnark::G1 *mnt4753_libsnark::G1_scale(field *a, G1 *b) {
  return new G1{.data = a->data * b->data};
}

void mnt4753_libsnark::vector_Fr_muleq(mnt4753_libsnark::vector_Fr *a,
                                       mnt4753_libsnark::vector_Fr *b,
                                       size_t size) {
  size_t a_off = a->offset, b_off = b->offset;
#ifdef MULTICORE
#pragma omp parallel for
#endif
  for (size_t i = 0; i < size; i++) {
    a->data->at(i + a_off) = a->data->at(i + a_off) * b->data->at(i + b_off);
  }
}

void mnt4753_libsnark::vector_Fr_subeq(mnt4753_libsnark::vector_Fr *a,
                                       mnt4753_libsnark::vector_Fr *b,
                                       size_t size) {
  size_t a_off = a->offset, b_off = b->offset;
#ifdef MULTICORE
#pragma omp parallel for
#endif
  for (size_t i = 0; i < size; i++) {
    a->data->at(i + a_off) = a->data->at(i + a_off) - b->data->at(i + b_off);
  }
}

mnt4753_libsnark::vector_Fr *
mnt4753_libsnark::vector_Fr_offset(mnt4753_libsnark::vector_Fr *a,
                                   size_t offset) {
  return new vector_Fr{.data = a->data, .offset = offset};
}

void mnt4753_libsnark::vector_Fr_copy_into(mnt4753_libsnark::vector_Fr *src,
                                           mnt4753_libsnark::vector_Fr *dst,
                                           size_t length) {
  std::cerr << "length is " << length << ", offset is " << src->offset
            << ", size of src is " << src->data->size() << ", size of dst is "
            << dst->data->size() << std::endl;
#ifdef MULTICORE
#pragma omp parallel for
#endif
  for (size_t i = 0; i < length; i++) {
    // std::cerr << "doing iteration " << i << std::endl;
    dst->data->at(i) = src->data->at(i);
  }
  // std::copy(src->data->begin(), src->data->end(), dst->data->begin() );
}

mnt4753_libsnark::vector_Fr *mnt4753_libsnark::vector_Fr_zeros(size_t length) {
  std::vector<Fr<mnt4753_pp>> data(length, Fr<mnt4753_pp>::zero());
  return new mnt4753_libsnark::vector_Fr{
      .data = std::make_shared<std::vector<Fr<mnt4753_pp>>>(data),
          .offset = 0};
}

void mnt4753_libsnark::domain_iFFT(mnt4753_libsnark::evaluation_domain *domain,
                                   mnt4753_libsnark::vector_Fr *a) {
  std::vector<Fr<mnt4753_pp>> &data = *a->data;
  domain->data->iFFT(data);
}
void mnt4753_libsnark::domain_cosetFFT(
    mnt4753_libsnark::evaluation_domain *domain,
    mnt4753_libsnark::vector_Fr *a) {
  domain->data->cosetFFT(*a->data, Fr<mnt4753_pp>::multiplicative_generator);
}
void mnt4753_libsnark::domain_icosetFFT(
    mnt4753_libsnark::evaluation_domain *domain,
    mnt4753_libsnark::vector_Fr *a) {
  domain->data->icosetFFT(*a->data, Fr<mnt4753_pp>::multiplicative_generator);
}
void mnt4753_libsnark::domain_divide_by_Z_on_coset(
    mnt4753_libsnark::evaluation_domain *domain,
    mnt4753_libsnark::vector_Fr *a) {
  domain->data->divide_by_Z_on_coset(*a->data);
}
size_t
mnt4753_libsnark::domain_get_m(mnt4753_libsnark::evaluation_domain *domain) {
  return domain->data->m;
}

mnt4753_libsnark::G1 *
mnt4753_libsnark::multiexp_G1(mnt4753_libsnark::vector_Fr *scalar_start,
                              mnt4753_libsnark::vector_G1 *g_start,
                              size_t length) {

  return new mnt4753_libsnark::G1{
      multiexp<libff::G1<mnt4753_pp>, Fr<mnt4753_pp>>(
          scalar_start->data->begin() + scalar_start->offset,
          g_start->data->begin(), length)};
}
mnt4753_libsnark::G2 *
mnt4753_libsnark::multiexp_G2(mnt4753_libsnark::vector_Fr *scalar_start,
                              mnt4753_libsnark::vector_G2 *g_start,
                              size_t length) {
  return new mnt4753_libsnark::G2{
      multiexp<libff::G2<mnt4753_pp>, Fr<mnt4753_pp>>(
          scalar_start->data->begin() + scalar_start->offset,
          g_start->data->begin(), length)};
}

mnt4753_libsnark::groth16_input *
mnt4753_libsnark::read_input(FILE *inputs, size_t d, size_t m) {
  return new mnt4753_libsnark::groth16_input(inputs, d, m);
}

mnt4753_libsnark::vector_Fr *
mnt4753_libsnark::input_w(mnt4753_libsnark::groth16_input *input) {
  return new mnt4753_libsnark::vector_Fr{.data = input->w, .offset = 0};
}

mnt4753_libsnark::vector_G1 *
mnt4753_libsnark::params_A(mnt4753_libsnark::groth16_params *params) {
  return new mnt4753_libsnark::vector_G1{.data = params->A};
}
mnt4753_libsnark::vector_G1 *
mnt4753_libsnark::params_B1(mnt4753_libsnark::groth16_params *params) {
  return new mnt4753_libsnark::vector_G1{.data = params->B1};
}
mnt4753_libsnark::vector_G1 *
mnt4753_libsnark::params_L(mnt4753_libsnark::groth16_params *params) {
  return new mnt4753_libsnark::vector_G1{.data = params->L};
}
mnt4753_libsnark::vector_G1 *
mnt4753_libsnark::params_H(mnt4753_libsnark::groth16_params *params) {
  return new mnt4753_libsnark::vector_G1{.data = params->H};
}
mnt4753_libsnark::vector_G2 *
mnt4753_libsnark::params_B2(mnt4753_libsnark::groth16_params *params) {
  return new mnt4753_libsnark::vector_G2{.data = params->B2};
}

mnt4753_libsnark::vector_Fr *
mnt4753_libsnark::input_ca(mnt4753_libsnark::groth16_input *input) {
  return new mnt4753_libsnark::vector_Fr{.data = input->ca, .offset = 0};
}
mnt4753_libsnark::vector_Fr *mnt4753_libsnark::input_cb(groth16_input *input) {
  return new mnt4753_libsnark::vector_Fr{.data = input->cb, .offset = 0};
}
mnt4753_libsnark::vector_Fr *mnt4753_libsnark::input_cc(groth16_input *input) {
  return new vector_Fr{.data = input->cc, .offset = 0};
}
mnt4753_libsnark::field *mnt4753_libsnark::input_r(groth16_input *input) {
  return new mnt4753_libsnark::field{.data = input->r};
}

mnt4753_libsnark::groth16_params *
mnt4753_libsnark::read_params(FILE *params, size_t d, size_t m) {
    return new mnt4753_libsnark::groth16_params(params, d, m);
}

void mnt4753_libsnark::delete_G1(mnt4753_libsnark::G1 *a) { delete a; }
void mnt4753_libsnark::delete_G2(mnt4753_libsnark::G2 *a) { delete a; }
void mnt4753_libsnark::delete_vector_Fr(mnt4753_libsnark::vector_Fr *a) {
  delete a;
}
void mnt4753_libsnark::delete_vector_G1(mnt4753_libsnark::vector_G1 *a) {
  delete a;
}
void mnt4753_libsnark::delete_vector_G2(mnt4753_libsnark::vector_G2 *a) {
  delete a;
}
void mnt4753_libsnark::delete_groth16_input(
    mnt4753_libsnark::groth16_input *a) {
  delete a;
}
void mnt4753_libsnark::delete_groth16_params(
    mnt4753_libsnark::groth16_params *a) {
  delete a;
}
void mnt4753_libsnark::delete_evaluation_domain(
    mnt4753_libsnark::evaluation_domain *a) {
  delete a;
}

void mnt4753_libsnark::groth16_output_write(mnt4753_libsnark::G1 *A,
                                            mnt4753_libsnark::G2 *B,
                                            mnt4753_libsnark::G1 *C,
                                            const char *output_path) {
  FILE *out = fopen(output_path, "w");
  write_g1<mnt4753_pp>(out, A->data);
  write_g2<mnt4753_pp>(out, B->data);
  write_g1<mnt4753_pp>(out, C->data);
  fclose(out);
}
mnt6753_libsnark::groth16_input::groth16_input(FILE *inputs, size_t d, size_t m) {
  w = std::make_shared<std::vector<libff::Fr<mnt6753_pp>>>(
      std::vector<libff::Fr<mnt6753_pp>>());
  ca = std::make_shared<std::vector<libff::Fr<mnt6753_pp>>>(
      std::vector<libff::Fr<mnt6753_pp>>());
  cb = std::make_shared<std::vector<libff::Fr<mnt6753_pp>>>(
      std::vector<libff::Fr<mnt6753_pp>>());
  cc = std::make_shared<std::vector<libff::Fr<mnt6753_pp>>>(
      std::vector<libff::Fr<mnt6753_pp>>());

  for (size_t i = 0; i < m + 1; ++i) {
    w->emplace_back(read_fr<mnt6753_pp>(inputs));
  }
  for (size_t i = 0; i < d + 1; ++i) {
    ca->emplace_back(read_fr<mnt6753_pp>(inputs));
  }
  for (size_t i = 0; i < d + 1; ++i) {
    cb->emplace_back(read_fr<mnt6753_pp>(inputs));
  }
  for (size_t i = 0; i < d + 1; ++i) {
    cc->emplace_back(read_fr<mnt6753_pp>(inputs));
  }

  r = read_fr<mnt6753_pp>(inputs);
}

mnt6753_libsnark::groth16_input::groth16_input(
    const std::vector<Fr<mnt6753_pp>> *public_input,
    const std::vector<Fr<mnt6753_pp>> *auxiliary_input,
    const r1cs_constraint_system<Fr<mnt6753_pp>> *r1cs)
{
  w = std::make_shared<std::vector<libff::Fr<mnt6753_pp>>>(
      std::vector<libff::Fr<mnt6753_pp>>());
  r = libff::Fr<libff::mnt6753_pp>::random_element();

  w->emplace_back(Fr<mnt6753_pp>::one());
  w->insert(w->end(), public_input->begin(), public_input->end());
  w->insert(w->end(), auxiliary_input->begin(), auxiliary_input->end());

  r1cs_variable_assignment<Fr<mnt6753_pp>> variable_assignment(*public_input);
  variable_assignment.insert(variable_assignment.end(), auxiliary_input->begin(), auxiliary_input->end());

  // Retrieve the actual size of the evaluation domain that will be used for
  // the FFTs, which may be bigger than d + 1.
  auto domain = libfqfft::get_evaluation_domain<libff::Fr<mnt6753_pp>>(r1cs->num_constraints() + r1cs->primary_input_size + 1);
  size_t d_plus_1 = domain->m;

  ca = std::make_shared<std::vector<libff::Fr<mnt6753_pp>>>(
      std::vector<libff::Fr<mnt6753_pp>>(d_plus_1, Fr<mnt6753_pp>::zero()));
  cb = std::make_shared<std::vector<libff::Fr<mnt6753_pp>>>(
      std::vector<libff::Fr<mnt6753_pp>>(d_plus_1, Fr<mnt6753_pp>::zero()));
  cc = std::make_shared<std::vector<libff::Fr<mnt6753_pp>>>(
      std::vector<libff::Fr<mnt6753_pp>>(d_plus_1, Fr<mnt6753_pp>::zero()));

  for (size_t i = 0; i <= r1cs->primary_input_size; ++i)
  {
      (*ca)[i+r1cs->num_constraints()] = (i > 0 ? (variable_assignment)[i-1] : Fr<mnt6753_pp>::one());
  }
  for (size_t i = 0; i < r1cs->num_constraints(); ++i)
  {
      (*ca)[i] += r1cs->constraints[i].a.evaluate(variable_assignment);
      (*cb)[i] += r1cs->constraints[i].b.evaluate(variable_assignment);
      (*cc)[i] += r1cs->constraints[i].c.evaluate(variable_assignment);
  }
}

mnt6753_libsnark::groth16_params::groth16_params(FILE *params, size_t dd, size_t mm) {
  d = read_size_t(params);
  m = read_size_t(params);
  if (d != dd || m != mm) {
      fputs("Bad size read", stderr);
      abort();
  }

  A = std::make_shared<std::vector<libff::G1<mnt6753_pp>>>(
      std::vector<libff::G1<mnt6753_pp>>());
  B1 = std::make_shared<std::vector<libff::G1<mnt6753_pp>>>(
      std::vector<libff::G1<mnt6753_pp>>());
  L = std::make_shared<std::vector<libff::G1<mnt6753_pp>>>(
      std::vector<libff::G1<mnt6753_pp>>());
  H = std::make_shared<std::vector<libff::G1<mnt6753_pp>>>(
      std::vector<libff::G1<mnt6753_pp>>());
  B2 = std::make_shared<std::vector<libff::G2<mnt6753_pp>>>(
      std::vector<libff::G2<mnt6753_pp>>());
  for (size_t i = 0; i <= m; ++i) {
    A->emplace_back(read_g1<mnt6753_pp>(params));
  }
  for (size_t i = 0; i <= m; ++i) {
    B1->emplace_back(read_g1<mnt6753_pp>(params));
  }
  for (size_t i = 0; i <= m; ++i) {
    B2->emplace_back(read_g2<mnt6753_pp>(params));
  }
  for (size_t i = 0; i < m - 1; ++i) {
    L->emplace_back(read_g1<mnt6753_pp>(params));
  }
  for (size_t i = 0; i < d; ++i) {
    H->emplace_back(read_g1<mnt6753_pp>(params));
  }
}

mnt6753_libsnark::groth16_params::groth16_params(libsnark::r1cs_gg_ppzksnark_proving_key<libff::mnt6753_pp> *pk) {
  // Retrieve the actual size of the evaluation domain that will be used for
  // the FFTs, which may be bigger than d + 1.
  auto domain = libfqfft::get_evaluation_domain<libff::Fr<mnt6753_pp>>(pk->constraint_system.num_constraints() + pk->constraint_system.primary_input_size + 1);
  size_t d_plus_1 = domain->m;
  d = d_plus_1 - 1;

  m = pk->constraint_system.num_variables();
  A = std::make_shared<std::vector<libff::G1<mnt6753_pp>>>(
      std::vector<libff::G1<mnt6753_pp>>(pk->A_query));
  L = std::make_shared<std::vector<libff::G1<mnt6753_pp>>>(
      std::vector<libff::G1<mnt6753_pp>>(pk->L_query));
  H = std::make_shared<std::vector<libff::G1<mnt6753_pp>>>(
      std::vector<libff::G1<mnt6753_pp>>(pk->H_query));

  B1 = std::make_shared<std::vector<libff::G1<mnt6753_pp>>>(
      std::vector<libff::G1<mnt6753_pp>>());
  B2 = std::make_shared<std::vector<libff::G2<mnt6753_pp>>>(
      std::vector<libff::G2<mnt6753_pp>>());
  for (size_t i = 0; i <= m; ++i) {
    B1->emplace_back(pk->B_query[i].h);
    B2->emplace_back(pk->B_query[i].g);
  }
}

void mnt6753_libsnark::init_public_params() {
  mnt6753_pp::init_public_params();
}

void mnt6753_libsnark::print_G1(mnt6753_libsnark::G1 *a) { a->data.print(); }

void mnt6753_libsnark::print_G1(mnt6753_libsnark::vector_G1 *a) { a->data->at(0).print(); }

void mnt6753_libsnark::print_G2(mnt6753_libsnark::G2 *a) { a->data.print(); }

void mnt6753_libsnark::print_G2(mnt6753_libsnark::vector_G2 *a, size_t i) { a->data->at(i).print(); }

mnt6753_libsnark::evaluation_domain *
mnt6753_libsnark::get_evaluation_domain(size_t d) {
  return new evaluation_domain{
      .data = libfqfft::get_evaluation_domain<Fr<mnt6753_pp>>(d)};
}

int mnt6753_libsnark::G1_equal(mnt6753_libsnark::G1 *a, mnt6753_libsnark::G1 *b) {
  return a->data == b->data;
}

int mnt6753_libsnark::G2_equal(mnt6753_libsnark::G2 *a, mnt6753_libsnark::G2 *b) {
  return a->data == b->data;
}

mnt6753_libsnark::G1 *mnt6753_libsnark::G1_add(mnt6753_libsnark::G1 *a,
                                               mnt6753_libsnark::G1 *b) {
  return new mnt6753_libsnark::G1{.data = a->data + b->data};
}

mnt6753_libsnark::G1 *mnt6753_libsnark::G1_scale(field *a, G1 *b) {
  return new G1{.data = a->data * b->data};
}

void mnt6753_libsnark::vector_Fr_muleq(mnt6753_libsnark::vector_Fr *a,
                                       mnt6753_libsnark::vector_Fr *b,
                                       size_t size) {
  size_t a_off = a->offset, b_off = b->offset;
#ifdef MULTICORE
#pragma omp parallel for
#endif
  for (size_t i = 0; i < size; i++) {
    a->data->at(i + a_off) = a->data->at(i + a_off) * b->data->at(i + b_off);
  }
}

void mnt6753_libsnark::vector_Fr_subeq(mnt6753_libsnark::vector_Fr *a,
                                       mnt6753_libsnark::vector_Fr *b,
                                       size_t size) {
  size_t a_off = a->offset, b_off = b->offset;
#ifdef MULTICORE
#pragma omp parallel for
#endif
  for (size_t i = 0; i < size; i++) {
    a->data->at(i + a_off) = a->data->at(i + a_off) - b->data->at(i + b_off);
  }
}

mnt6753_libsnark::vector_Fr *
mnt6753_libsnark::vector_Fr_offset(mnt6753_libsnark::vector_Fr *a,
                                   size_t offset) {
  return new vector_Fr{.data = a->data, .offset = offset};
}

mnt4753_libsnark::vector_G2 *
mnt4753_libsnark::vector_G2_offset(mnt4753_libsnark::vector_G2 *a, size_t offset) {

    // Holy hell. :/
  std::shared_ptr<std::vector<libff::G2<mnt4753_pp>>> v =
      std::make_shared<std::vector<libff::G2<mnt4753_pp>>>(std::vector<libff::G2<mnt4753_pp>>());

  auto x = a->data->begin() + offset;
  auto end = a->data->end();
  while (x != end)
      v->push_back(*x++);
  return new vector_G2{.data = v};
}

mnt6753_libsnark::vector_G2 *
mnt6753_libsnark::vector_G2_offset(mnt6753_libsnark::vector_G2 *a, size_t offset) {

    // Holy hell. :/
  std::shared_ptr<std::vector<libff::G2<mnt6753_pp>>> v =
      std::make_shared<std::vector<libff::G2<mnt6753_pp>>>(std::vector<libff::G2<mnt6753_pp>>());

  auto x = a->data->begin() + offset;
  auto end = a->data->end();
  while (x != end)
      v->push_back(*x++);
  return new vector_G2{.data = v};
}


void mnt6753_libsnark::vector_Fr_copy_into(mnt6753_libsnark::vector_Fr *src,
                                           mnt6753_libsnark::vector_Fr *dst,
                                           size_t length) {
  std::copy(src->data->begin() + src->offset,
            src->data->begin() + src->offset + length, dst->data->begin());
}

mnt6753_libsnark::vector_Fr *mnt6753_libsnark::vector_Fr_zeros(size_t length) {
  return new mnt6753_libsnark::vector_Fr{
      .data = std::make_shared<std::vector<Fr<mnt6753_pp>>>(
          length, Fr<mnt6753_pp>::zero()), .offset = 0 };
}

void mnt6753_libsnark::domain_iFFT(mnt6753_libsnark::evaluation_domain *domain,
                                   mnt6753_libsnark::vector_Fr *a) {
  std::vector<Fr<mnt6753_pp>> &data = *a->data;
  domain->data->iFFT(data);
}
void mnt6753_libsnark::domain_cosetFFT(
    mnt6753_libsnark::evaluation_domain *domain,
    mnt6753_libsnark::vector_Fr *a) {
  domain->data->cosetFFT(*a->data, Fr<mnt6753_pp>::multiplicative_generator);
}
void mnt6753_libsnark::domain_icosetFFT(
    mnt6753_libsnark::evaluation_domain *domain,
    mnt6753_libsnark::vector_Fr *a) {
  domain->data->icosetFFT(*a->data, Fr<mnt6753_pp>::multiplicative_generator);
}
void mnt6753_libsnark::domain_divide_by_Z_on_coset(
    mnt6753_libsnark::evaluation_domain *domain,
    mnt6753_libsnark::vector_Fr *a) {
  domain->data->divide_by_Z_on_coset(*a->data);
}
size_t
mnt6753_libsnark::domain_get_m(mnt6753_libsnark::evaluation_domain *domain) {
  return domain->data->m;
}

mnt6753_libsnark::G1 *
mnt6753_libsnark::multiexp_G1(mnt6753_libsnark::vector_Fr *scalar_start,
                              mnt6753_libsnark::vector_G1 *g_start,
                              size_t length) {

  return new mnt6753_libsnark::G1{
      multiexp<libff::G1<mnt6753_pp>, Fr<mnt6753_pp>>(
          scalar_start->data->begin() + scalar_start->offset,
          g_start->data->begin(), length)};
}
mnt6753_libsnark::G2 *
mnt6753_libsnark::multiexp_G2(mnt6753_libsnark::vector_Fr *scalar_start,
                              mnt6753_libsnark::vector_G2 *g_start,
                              size_t length) {
  return new mnt6753_libsnark::G2{
      multiexp<libff::G2<mnt6753_pp>, Fr<mnt6753_pp>>(
          scalar_start->data->begin() + scalar_start->offset,
          g_start->data->begin(), length)};
}

mnt6753_libsnark::groth16_input *
mnt6753_libsnark::read_input(FILE *inputs, size_t d, size_t m) {
  return new mnt6753_libsnark::groth16_input(inputs, d, m);
}

mnt6753_libsnark::vector_Fr *
mnt6753_libsnark::input_w(mnt6753_libsnark::groth16_input *input) {
  return new mnt6753_libsnark::vector_Fr{.data = input->w, .offset = 0};
}

mnt6753_libsnark::vector_G1 *
mnt6753_libsnark::params_A(mnt6753_libsnark::groth16_params *params) {
  return new mnt6753_libsnark::vector_G1{.data = params->A};
}
mnt6753_libsnark::vector_G1 *
mnt6753_libsnark::params_B1(mnt6753_libsnark::groth16_params *params) {
  return new mnt6753_libsnark::vector_G1{.data = params->B1};
}
mnt6753_libsnark::vector_G1 *
mnt6753_libsnark::params_L(mnt6753_libsnark::groth16_params *params) {
  return new mnt6753_libsnark::vector_G1{.data = params->L};
}
mnt6753_libsnark::vector_G1 *
mnt6753_libsnark::params_H(mnt6753_libsnark::groth16_params *params) {
  return new mnt6753_libsnark::vector_G1{.data = params->H};
}
mnt6753_libsnark::vector_G2 *
mnt6753_libsnark::params_B2(mnt6753_libsnark::groth16_params *params) {
  return new mnt6753_libsnark::vector_G2{.data = params->B2};
}

mnt6753_libsnark::vector_Fr *
mnt6753_libsnark::input_ca(mnt6753_libsnark::groth16_input *input) {
  return new mnt6753_libsnark::vector_Fr{.data = input->ca, .offset = 0};
}
mnt6753_libsnark::vector_Fr *mnt6753_libsnark::input_cb(groth16_input *input) {
  return new mnt6753_libsnark::vector_Fr{.data = input->cb, .offset = 0};
}
mnt6753_libsnark::vector_Fr *mnt6753_libsnark::input_cc(groth16_input *input) {
  return new vector_Fr{.data = input->cc, .offset = 0};
}
mnt6753_libsnark::field *mnt6753_libsnark::input_r(groth16_input *input) {
  return new mnt6753_libsnark::field{.data = input->r};
}

mnt6753_libsnark::groth16_params *
mnt6753_libsnark::read_params(FILE *params, size_t d, size_t m) {
    return new mnt6753_libsnark::groth16_params(params, d, m);
}

void mnt6753_libsnark::delete_G1(mnt6753_libsnark::G1 *a) { delete a; }
void mnt6753_libsnark::delete_G2(mnt6753_libsnark::G2 *a) { delete a; }
void mnt6753_libsnark::delete_vector_Fr(mnt6753_libsnark::vector_Fr *a) {
  delete a;
}
void mnt6753_libsnark::delete_vector_G1(mnt6753_libsnark::vector_G1 *a) {
  delete a;
}
void mnt6753_libsnark::delete_vector_G2(mnt6753_libsnark::vector_G2 *a) {
  delete a;
}
void mnt6753_libsnark::delete_groth16_input(
    mnt6753_libsnark::groth16_input *a) {
  delete a;
}
void mnt6753_libsnark::delete_groth16_params(
    mnt6753_libsnark::groth16_params *a) {
  delete a;
}
void mnt6753_libsnark::delete_evaluation_domain(
    mnt6753_libsnark::evaluation_domain *a) {
  delete a;
}

void mnt6753_libsnark::groth16_output_write(mnt6753_libsnark::G1 *A,
                                            mnt6753_libsnark::G2 *B,
                                            mnt6753_libsnark::G1 *C,
                                            const char *output_path) {
  FILE *out = fopen(output_path, "w");
  write_g1<mnt6753_pp>(out, A->data);
  write_g2<mnt6753_pp>(out, B->data);
  write_g1<mnt6753_pp>(out, C->data);
  fclose(out);
}

mnt4753_libsnark::G1 *
mnt4753_libsnark::read_pt_ECp(const var *mem) {
    const char *cmem = reinterpret_cast<const char *>(mem);
    return new mnt4753_libsnark::G1{ .data = read_pt<mnt4753_libsnark::G1>(cmem) };
}

mnt4753_libsnark::G2 *
mnt4753_libsnark::read_pt_ECpe(const var *mem) {
    const char *cmem = reinterpret_cast<const char *>(mem);
    return new mnt4753_libsnark::G2{ .data =read_pt<mnt4753_libsnark::G2>(cmem) };
}

mnt6753_libsnark::G1 *
mnt6753_libsnark::read_pt_ECp(const var *mem) {
    const char *cmem = reinterpret_cast<const char *>(mem);
    return new mnt6753_libsnark::G1{ .data = read_pt<mnt6753_libsnark::G1>(cmem) };
}

mnt6753_libsnark::G2 *
mnt6753_libsnark::read_pt_ECpe(const var *mem) {
    const char *cmem = reinterpret_cast<const char *>(mem);
    return new mnt6753_libsnark::G2{ .data = read_pt<mnt6753_libsnark::G2>(cmem) };
}
