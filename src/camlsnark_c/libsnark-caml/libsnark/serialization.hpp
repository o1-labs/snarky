#include <cassert>
#include <cstdio>
#include <fstream>
#include <fstream>
#include <libff/common/profiling.hpp>
#include <libff/common/utils.hpp>

#include <libff/algebra/curves/mnt753/mnt4753/mnt4753_pp.hpp>
#include <libff/algebra/curves/mnt753/mnt6753/mnt6753_pp.hpp>
#include <omp.h>
#include <libff/algebra/scalar_multiplication/multiexp.hpp>
#include <libsnark/knowledge_commitment/kc_multiexp.hpp>
#include <libsnark/knowledge_commitment/knowledge_commitment.hpp>
#include <libsnark/reductions/r1cs_to_qap/r1cs_to_qap.hpp>

#include <libsnark/zk_proof_systems/ppzksnark/r1cs_gg_ppzksnark/r1cs_gg_ppzksnark.hpp>

#include <libfqfft/evaluation_domain/domains/basic_radix2_domain.hpp>
using namespace libff;
using namespace libsnark;

const size_t num_limbs = 12;

template<typename ppT>
void write_fq(FILE* output, Fq<ppT> x) {
  fwrite((void *) x.mont_repr.data, num_limbs * sizeof(mp_size_t), 1, output);
}

template<typename ppT>
void write_fr(FILE* output, Fr<ppT> x) {
  fwrite((void *) x.mont_repr.data, num_limbs * sizeof(mp_size_t), 1, output);
}

template<typename ppT>
void write_fqe(FILE* output, Fqe<ppT> x) {
  std::vector<Fq<ppT>> v = x.all_base_field_elements();
  size_t deg = Fqe<ppT>::extension_degree();
  for (size_t i = 0; i < deg; ++i) {
    write_fq<ppT>(output, v[i]);
  }
}

template<typename ppT>
void write_g1(FILE* output, G1<ppT> g) {
  if (g.is_zero())  {
    write_fq<ppT>(output, Fq<ppT>::zero());
    write_fq<ppT>(output, Fq<ppT>::zero());
    return;
  }

  g.to_affine_coordinates();
  write_fq<ppT>(output, g.X());
  write_fq<ppT>(output, g.Y());
}

template<typename ppT>
void write_g2(FILE* output, G2<ppT> g) {
  if (g.is_zero())  {
    write_fqe<ppT>(output, Fqe<ppT>::zero());
    write_fqe<ppT>(output, Fqe<ppT>::zero());
    return;
  }

  g.to_affine_coordinates();
  write_fqe<ppT>(output, g.X());
  write_fqe<ppT>(output, g.Y());
}

template<typename ppT>
Fq<ppT> read_fq(FILE* input) {
  Fq<ppT> x;
  fread((void *) x.mont_repr.data, num_limbs * sizeof(mp_size_t), 1, input);
  return x;
}

template<typename ppT>
Fr<ppT> read_fr(FILE* input) {
  Fr<ppT> x;
  fread((void *) x.mont_repr.data, num_limbs * sizeof(mp_size_t), 1, input);
  return x;
}

template<typename ppT>
G1<ppT> read_g1(FILE* input) {
  Fq<ppT> x = read_fq<ppT>(input);
  Fq<ppT> y = read_fq<ppT>(input);
  if (y == Fq<ppT>::zero()) {
    return G1<ppT>::zero();
  }
  return G1<ppT>(x, y, Fq<ppT>::one());
}

template<typename ppT>
Fqe<ppT> read_fqe(FILE* input) {
  std::vector<Fq<ppT>> elts;
  size_t deg = Fqe<ppT>::extension_degree();
  for (size_t i = 0; i < deg; ++i) {
    elts.emplace_back(read_fq<ppT>(input));
  }
  return Fqe<ppT>(elts);
}

template<typename ppT>
G2<ppT> read_g2(FILE* input) {
  Fqe<ppT> x = read_fqe<ppT>(input);
  Fqe<ppT> y = read_fqe<ppT>(input);
  if (y == Fqe<ppT>::zero()) {
    return G2<ppT>::zero();
  }
  return G2<ppT>(x, y, Fqe<ppT>::one());
}

size_t read_size_t(FILE* input);

void write_size_t(FILE* output, size_t n);
