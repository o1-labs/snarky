#include <libsnark/caml/caml_mnt4.hpp>

extern "C" {
using namespace libsnark;

void camlsnark_mnt4_emplace_bits_of_field(std::vector<bool>* v, FieldT &x) {
  size_t field_size = FieldT::size_in_bits();
  auto n = x.as_bigint();
  for (size_t i = 0; i < field_size; ++i) {
    v->emplace_back(n.test_bit(i));
  }
}

// Start g1 code

libff::Fq<ppT>* camlsnark_mnt4_g1_coeff_a () {
  return &libff::G1<ppT>::coeff_a;
}

libff::Fq<ppT>* camlsnark_mnt4_g1_coeff_b () {
  return &libff::G1<ppT>::coeff_b;
}

libff::G1<ppT>* camlsnark_mnt4_g1_of_coords (libff::Fq<ppT>* x, libff::Fq<ppT>* y) {
  return new libff::G1<ppT>(*x, *y);
}

libff::G1<ppT>* camlsnark_mnt4_g1_negate (libff::G1<ppT>* a) {
  return new libff::G1<ppT>(- *a);
}

libff::G1<ppT>* camlsnark_mnt4_g1_double (libff::G1<ppT>* a) {
  return new libff::G1<ppT>(a->dbl());
}

libff::G1<ppT>* camlsnark_mnt4_g1_add (libff::G1<ppT>* a, libff::G1<ppT>* b) {
  return new libff::G1<ppT>((*a) + (*b));
}

libff::G1<ppT>* camlsnark_mnt4_g1_scale (libff::bigint<libff::mnt4_r_limbs> *a, libff::G1<ppT>* x) {
  return new libff::G1<ppT>((*a) * (*x));
}

libff::G1<ppT>* camlsnark_mnt4_g1_scale_field (FieldT *a, libff::G1<ppT>* x) {
  return new libff::G1<ppT>((*a) * (*x));
}

libff::G1<ppT>* camlsnark_mnt4_g1_zero () {
  return new libff::G1<ppT>(libff::G1<ppT>::zero());
}

libff::G1<ppT>* camlsnark_mnt4_g1_one () {
  return new libff::G1<ppT>(libff::G1<ppT>::one());
}

void camlsnark_mnt4_g1_print(libff::G1<ppT>* x) {
  x->print();
}

bool camlsnark_mnt4_g1_equal(libff::G1<ppT>* a, libff::G1<ppT>* b) {
  return *a == *b;
}

void camlsnark_mnt4_g1_delete(libff::G1<ppT>* a) {
  delete a;
}

libff::G1<ppT>* camlsnark_mnt4_g1_random() {
  return new libff::G1<ppT>(libff::G1<ppT>::random_element());
}

void camlsnark_mnt4_g1_to_affine_coordinates(libff::G1<ppT>* a) {
  a->to_affine_coordinates();
}

libff::Fq<ppT>* camlsnark_mnt4_g1_x(libff::G1<ppT>* a) {
  assert(a->Z() == libff::Fq<ppT>::one());
  return new libff::Fq<ppT>(a->X());
}

libff::Fq<ppT>* camlsnark_mnt4_g1_y(libff::G1<ppT>* a) {
  assert(a->Z() == libff::Fq<ppT>::one());
  return new libff::Fq<ppT>(a->Y());
}

std::vector<libff::G1<ppT>>* camlsnark_mnt4_g1_vector_create() {
  return new std::vector<libff::G1<ppT>>();
}

int camlsnark_mnt4_g1_vector_length(std::vector<libff::G1<ppT>> *v) {
  return v->size();
}

void camlsnark_mnt4_g1_vector_emplace_back(std::vector<libff::G1<ppT>>* v, libff::G1<ppT>* x) {
  v->emplace_back(*x);
}

libff::G1<ppT>* camlsnark_mnt4_g1_vector_get(std::vector<libff::G1<ppT>>* v, int i) {
  libff::G1<ppT> res = (*v)[i];
  return new libff::G1<ppT>(res);
}

void camlsnark_mnt4_g1_vector_delete(std::vector<libff::G1<ppT>>* v) {
  delete v;
}

// End g1 code

// Start g2 code
libff::G2<ppT>* camlsnark_mnt4_g2_of_coords (
    std::vector<libff::Fq<ppT>>* x,
    std::vector<libff::Fq<ppT>>* y) {
  return new libff::G2<ppT>(
      libff::Fqe<ppT>(*x), libff::Fqe<ppT>(*y), libff::Fqe<ppT>::one() );
}

libff::G2<ppT>* camlsnark_mnt4_g2_negate (libff::G2<ppT>* a) {
  return new libff::G2<ppT>(- *a);
}

libff::G2<ppT>* camlsnark_mnt4_g2_double (libff::G2<ppT>* a) {
  return new libff::G2<ppT>(a->dbl());
}

libff::G2<ppT>* camlsnark_mnt4_g2_add (libff::G2<ppT>* a, libff::G2<ppT>* b) {
  return new libff::G2<ppT>((*a) + (*b));
}

libff::G2<ppT>* camlsnark_mnt4_g2_scale (libff::bigint<libff::mnt4_r_limbs> *a, libff::G2<ppT>* x) {
  return new libff::G2<ppT>((*a) * (*x));
}

libff::G2<ppT>* camlsnark_mnt4_g2_scale_field (FieldT *a, libff::G2<ppT>* x) {
  return new libff::G2<ppT>((*a) * (*x));
}

libff::G2<ppT>* camlsnark_mnt4_g2_zero () {
  return new libff::G2<ppT>(libff::G2<ppT>::zero());
}

libff::G2<ppT>* camlsnark_mnt4_g2_one () {
  return new libff::G2<ppT>(libff::G2<ppT>::one());
}

void camlsnark_mnt4_g2_print(libff::G2<ppT>* x) {
  x->print();
}

bool camlsnark_mnt4_g2_equal(libff::G2<ppT>* a, libff::G2<ppT>* b) {
  return *a == *b;
}

void camlsnark_mnt4_g2_delete(libff::G2<ppT>* a) {
  delete a;
}

libff::G2<ppT>* camlsnark_mnt4_g2_random() {
  return new libff::G2<ppT>(libff::G2<ppT>::random_element());
}

void camlsnark_mnt4_g2_to_affine_coordinates(libff::G2<ppT>* a) {
  a->to_affine_coordinates();
}

std::vector<libff::Fq<ppT>>* camlsnark_mnt4_g2_x(libff::G2<ppT>* a) {
  assert(a->Z() == libff::Fqe<ppT>::one());
  return new std::vector< libff::Fq<ppT> >(a->X().all_base_field_elements());
}

std::vector<libff::Fq<ppT>>* camlsnark_mnt4_g2_y(libff::G2<ppT>* a) {
  assert(a->Z() == libff::Fqe<ppT>::one());
  return new std::vector< libff::Fq<ppT> >(a->Y().all_base_field_elements());
}

std::vector<libff::G2<ppT>>* camlsnark_mnt4_g2_vector_create() {
  return new std::vector<libff::G2<ppT>>();
}

int camlsnark_mnt4_g2_vector_length(std::vector<libff::G2<ppT>> *v) {
  return v->size();
}

void camlsnark_mnt4_g2_vector_emplace_back(std::vector<libff::G2<ppT>>* v, libff::G2<ppT>* x) {
  v->emplace_back(*x);
}

libff::G2<ppT>* camlsnark_mnt4_g2_vector_get(std::vector<libff::G2<ppT>>* v, int i) {
  libff::G2<ppT> res = (*v)[i];
  return new libff::G2<ppT>(res);
}

void camlsnark_mnt4_g2_vector_delete(std::vector<libff::G2<ppT>>* v) {
  delete v;
}

// End g2 code

libff::Fqk<ppT>* camlsnark_mnt4_fqk_one(libff::Fqk<ppT>* a) {
  return new libff::Fqk<ppT>(libff::Fqk<ppT>::one());
}

void camlsnark_mnt4_fqk_delete(libff::Fqk<ppT>* a) {
  delete a;
}

std::vector<libff::Fq<ppT>>* camlsnark_mnt4_fqk_to_elts(libff::Fqk<ppT>* a) {
  return new std::vector<libff::Fq<ppT>>(a->all_base_field_elements());
}

}
