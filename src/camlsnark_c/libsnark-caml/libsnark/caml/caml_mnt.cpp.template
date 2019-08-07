extern "C" {
using namespace libsnark;
// Fqe
libff::Fqe<ppT>* CURVE_PREFIX(fqe_sqrt)(libff::Fqe<ppT>* x) {
  return new libff::Fqe<ppT>(x->sqrt());
}

libff::Fqe<ppT>* CURVE_PREFIX(fqe_random)() {
  return new libff::Fqe<ppT>(libff::Fqe<ppT>::random_element());
}

libff::Fqe<ppT>* CURVE_PREFIX(fqe_inv)(libff::Fqe<ppT>* x) {
  return new libff::Fqe<ppT>(x->inverse());
}

libff::Fqe<ppT>* CURVE_PREFIX(fqe_square)(libff::Fqe<ppT>* x) {
  return new libff::Fqe<ppT>(x->squared());
}

libff::Fqe<ppT>* CURVE_PREFIX(fqe_add)(libff::Fqe<ppT>* x, libff::Fqe<ppT>* y) {
  return new libff::Fqe<ppT>(*x + *y);
}

libff::Fqe<ppT>* CURVE_PREFIX(fqe_mul)(libff::Fqe<ppT>* x, libff::Fqe<ppT>* y) {
  return new libff::Fqe<ppT>(*x * *y);
}

libff::Fqe<ppT>* CURVE_PREFIX(fqe_sub)(libff::Fqe<ppT>* x, libff::Fqe<ppT>* y) {
  return new libff::Fqe<ppT>(*x - *y);
}

void CURVE_PREFIX(fqe_delete)(libff::Fqe<ppT>* f) {
  delete f;
}

void CURVE_PREFIX(fqe_print)(libff::Fqe<ppT>* f) {
  f->print();
}

std::vector<libff::Fq<ppT>>* CURVE_PREFIX(fqe_to_vector)(libff::Fqe<ppT>* f) {
  return new std::vector<libff::Fq<ppT>>(f->all_base_field_elements());
}

libff::Fqe<ppT>* CURVE_PREFIX(fqe_of_vector)(std::vector<libff::Fq<ppT>>* v) {
  return new libff::Fqe<ppT>(*v);
}

libff::Fqe<ppT>* CURVE_PREFIX(fqe_create_zero)() {
  return new libff::Fqe<ppT>(libff::Fqe<ppT>::zero());
}

bool CURVE_PREFIX(fqe_equal)(libff::Fqe<ppT>* x1, libff::Fqe<ppT>* x2) {
  return *x1 == *x2;
}

// G2
libff::Fqe<ppT>* CURVE_PREFIX(g2_coeff_a)() {
  return &libff::G2<ppT>::coeff_a;
}

libff::Fqe<ppT>* CURVE_PREFIX(g2_coeff_b)() {
  return &libff::G2<ppT>::coeff_b;
}

libff::Fqe<ppT>* CURVE_PREFIX(g2_x)(libff::G2<ppT>* a) {
  assert(a->Z() == libff::Fqe<ppT>::one());
  return new libff::Fqe<ppT>(a->X());
}

libff::Fqe<ppT>* CURVE_PREFIX(g2_y)(libff::G2<ppT>* a) {
  assert(a->Z() == libff::Fqe<ppT>::one());
  return new libff::Fqe<ppT>(a->Y());
}

libff::G2<ppT>* CURVE_PREFIX(g2_of_coords)(
    libff::Fqe<ppT>* x,
    libff::Fqe<ppT>* y) {
  return new libff::G2<ppT>(*x, *y, libff::Fqe<ppT>::one());
}

r1cs_gg_ppzksnark_verification_key<ppT>* CURVE_PREFIX(verification_key_dummy)(int input_size) {
  return new r1cs_gg_ppzksnark_verification_key<ppT>(
    libff::Fqk<ppT>(
      std::vector<libff::Fq<ppT>>(
      libff::Fqk<ppT>::extension_degree(),
      libff::Fq<ppT>::one())),
    libff::G2<ppT>::one(),
    accumulation_vector<libff::G1<ppT>>(libff::G1<ppT>::one(), libff::G1_vector<ppT>(input_size, libff::G1<ppT>::one()))
  );
}

r1cs_se_ppzksnark_verification_key<ppT>* CURVE_PREFIX(gm_verification_key_dummy)(int input_size) {
  return new r1cs_se_ppzksnark_verification_key<ppT>(
      libff::G2<ppT>::one(),
      libff::G1<ppT>::one(),
      libff::G2<ppT>::one(),
      libff::G1<ppT>::one(),
      libff::G2<ppT>::one(),
      std::vector<libff::G1<ppT>>(input_size + 1, libff::G1<ppT>::one()));
}

r1cs_bg_ppzksnark_verification_key<ppT>* CURVE_PREFIX(bg_verification_key_dummy)(int input_size) {
  return new r1cs_bg_ppzksnark_verification_key<ppT>(
    libff::Fqk<ppT>(
      std::vector<libff::Fq<ppT>>(
      libff::Fqk<ppT>::extension_degree(),
      libff::Fq<ppT>::one())),
    libff::G2<ppT>::one(),
    accumulation_vector<libff::G1<ppT>>(libff::G1<ppT>::one(), libff::G1_vector<ppT>(input_size, libff::G1<ppT>::one()))
  );
}

}
