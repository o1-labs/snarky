#include <libsnark/caml/caml_mnt6753.hpp>

extern "C" {
using namespace libsnark;

// verification key

void camlsnark_mnt6753_emplace_bits_of_field(std::vector<bool>* v, FieldT &x) {
  size_t field_size = FieldT::size_in_bits();
  auto n = x.as_bigint();
  for (size_t i = 0; i < field_size; ++i) {
    v->emplace_back(n.test_bit(i));
  }
}

// Start g1 ops code

libff::G1<ppT>* camlsnark_mnt6753_g1_of_coords (libff::Fq<ppT>* x, libff::Fq<ppT>* y) {
  return new libff::G1<ppT>(*x, *y);
}

libff::G1<ppT>* camlsnark_mnt6753_g1_negate (libff::G1<ppT>* a) {
  return new libff::G1<ppT>(- *a);
}

libff::G1<ppT>* camlsnark_mnt6753_g1_double (libff::G1<ppT>* a) {
  return new libff::G1<ppT>(a->dbl());
}

libff::G1<ppT>* camlsnark_mnt6753_g1_add (libff::G1<ppT>* a, libff::G1<ppT>* b) {
  return new libff::G1<ppT>((*a) + (*b));
}

libff::G1<ppT>* camlsnark_mnt6753_g1_scale (libff::bigint<libff::mnt6753_r_limbs> *a, libff::G1<ppT>* x) {
  return new libff::G1<ppT>((*a) * (*x));
}

libff::G1<ppT>* camlsnark_mnt6753_g1_scale_field (FieldT *a, libff::G1<ppT>* x) {
  return new libff::G1<ppT>((*a) * (*x));
}

libff::G1<ppT>* camlsnark_mnt6753_g1_zero () {
  return new libff::G1<ppT>(libff::G1<ppT>::zero());
}

libff::G1<ppT>* camlsnark_mnt6753_g1_one () {
  return new libff::G1<ppT>(libff::G1<ppT>::one());
}

void camlsnark_mnt6753_g1_print(libff::G1<ppT>* x) {
  x->print();
}

bool camlsnark_mnt6753_g1_equal(libff::G1<ppT>* a, libff::G1<ppT>* b) {
  return *a == *b;
}

void camlsnark_mnt6753_g1_delete(libff::G1<ppT>* a) {
  delete a;
}

libff::G1<ppT>* camlsnark_mnt6753_g1_random() {
  return new libff::G1<ppT>(libff::G1<ppT>::random_element());
}

void camlsnark_mnt6753_g1_to_affine_coordinates(libff::G1<ppT>* a) {
  a->to_affine_coordinates();
}

libff::Fq<ppT>* camlsnark_mnt6753_g1_x(libff::G1<ppT>* a) {
  assert(a->Z() == libff::Fq<ppT>::one());
  return new libff::Fq<ppT>(a->X());
}

libff::Fq<ppT>* camlsnark_mnt6753_g1_y(libff::G1<ppT>* a) {
  assert(a->Z() == libff::Fq<ppT>::one());
  return new libff::Fq<ppT>(a->Y());
}

std::vector<libff::G1<ppT>>* camlsnark_mnt6753_g1_vector_create() {
  return new std::vector<libff::G1<ppT>>();
}

int camlsnark_mnt6753_g1_vector_length(std::vector<libff::G1<ppT>> *v) {
  return v->size();
}

void camlsnark_mnt6753_g1_vector_emplace_back(std::vector<libff::G1<ppT>>* v, libff::G1<ppT>* x) {
  v->emplace_back(*x);
}

libff::G1<ppT>* camlsnark_mnt6753_g1_vector_get(std::vector<libff::G1<ppT>>* v, int i) {
  libff::G1<ppT> res = (*v)[i];
  return new libff::G1<ppT>(res);
}

void camlsnark_mnt6753_g1_vector_delete(std::vector<libff::G1<ppT>>* v) {
  delete v;
}

void camlsnark_mnt6753_g2_delete(libff::G2<ppT>* a) {
  delete a;
}

void camlsnark_mnt6753_g2_to_affine_coordinates(libff::G2<ppT>* a) {
  a->to_affine_coordinates();
}

std::vector<libff::Fq<ppT>>* camlsnark_mnt6753_g2_x(libff::G2<ppT>* a) {
  assert(a->Z() == libff::Fqe<ppT>::one());
  return new std::vector< libff::Fq<ppT> >(a->X().all_base_field_elements());
}

std::vector<libff::Fq<ppT>>* camlsnark_mnt6753_g2_y(libff::G2<ppT>* a) {
  assert(a->Z() == libff::Fqe<ppT>::one());
  return new std::vector< libff::Fq<ppT> >(a->Y().all_base_field_elements());
}

void camlsnark_mnt6753_fqk_delete(libff::Fqk<ppT>* a) {
  delete a;
}

std::vector<libff::Fq<ppT>>* camlsnark_mnt6753_fqk_to_elts(libff::Fqk<ppT>* a) {
  return new std::vector<libff::Fq<ppT>>(a->all_base_field_elements());
}

}
