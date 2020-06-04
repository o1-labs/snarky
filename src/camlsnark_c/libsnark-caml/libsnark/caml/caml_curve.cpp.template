extern "C" {
using namespace libsnark;

linear_combination<FieldT> CURVE_PREFIX(linear_combination_renumber)(
    linear_combination<FieldT> &lc,
    std::vector< linear_combination<FieldT> > &changes,
    int aux_shift) {
  linear_combination<FieldT> result = linear_combination<FieldT>();

  int num_terms = lc.terms.size();
  int num_changes = changes.size();

  for (int i = 0; i < num_terms; ++i) {
    linear_term<FieldT>& term = lc.terms[i];
    int term_index = term.index - 1;
    if (term_index >= 0) {
      if (term_index < num_changes) {
        FieldT coeff = term.coeff;
        linear_combination<FieldT>& subst_lc = changes[term_index];
        std::vector<linear_term<FieldT>>& subst = subst_lc.terms;
        int subst_size = subst.size();
        for (int j = 0; j < subst_size; ++j) {
          linear_term<FieldT>& subst_term = subst[j];
          result.add_term(
              variable<FieldT>(subst_term.index), coeff * subst_term.coeff);
        }
      } else {
        int new_index = term.index;
        new_index += aux_shift;
        result.add_term(
            variable<FieldT>(new_index),
            term.coeff);
      }
    } else {
      result.add_term(variable<FieldT>(term.index), term.coeff);
    }
  }

  return result;
}

void CURVE_PREFIX(init_public_params)() {
  ppT::init_public_params();
}

int CURVE_PREFIX(field_size_in_bits)() {
  auto n = FieldT::size_in_bits();
  return n;
}

libff::bigint<R_LIMBS>* CURVE_PREFIX(field_size)() {
  libff::bigint<R_LIMBS>* x = new libff::bigint<R_LIMBS>(FieldT::field_char());
  return x;
}

variable<FieldT>* CURVE_PREFIX(var_create)(int i) {
  return new variable<FieldT>(i);
}

void CURVE_PREFIX(var_delete)(variable<FieldT>* v) {
  delete v;
}

size_t CURVE_PREFIX(var_index)(variable<FieldT>* v) {
  return v->index;
}

bool CURVE_PREFIX(field_is_square)(FieldT* x) {
  FieldT y = *x ^ FieldT::euler;
  return y == FieldT::one();
}

FieldT* CURVE_PREFIX(field_sqrt)(FieldT* x) {
  return new FieldT(x->sqrt());
}

FieldT* CURVE_PREFIX(field_random)() {
  return new FieldT(FieldT::random_element());
}

FieldT* CURVE_PREFIX(field_of_int)(long n) {
  return new FieldT(n);
}

FieldT* CURVE_PREFIX(field_inv)(FieldT* x) {
  return new FieldT(x->inverse());
}

FieldT* CURVE_PREFIX(field_square)(FieldT* x) {
  return new FieldT(x->squared());
}

FieldT* CURVE_PREFIX(field_add)(FieldT* x, FieldT* y) {
  return new FieldT(*x + *y);
}

FieldT* CURVE_PREFIX(field_mul)(FieldT* x, FieldT* y) {
  return new FieldT(*x * *y);
}

FieldT* CURVE_PREFIX(field_sub)(FieldT* x, FieldT* y) {
  return new FieldT(*x - *y);
}

void CURVE_PREFIX(field_mut_add)(FieldT* x, FieldT* y) {
  *x += *y;
}

void CURVE_PREFIX(field_mut_mul)(FieldT* x, FieldT* y) {
  *x *= *y;
}

void CURVE_PREFIX(field_mut_sub)(FieldT* x, FieldT* y) {
  *x -= *y;
}

void CURVE_PREFIX(field_copy)(FieldT* x, FieldT* y) {
  mpn_copyi(x->mont_repr.data, y->mont_repr.data, x->num_limbs);
}

FieldT* CURVE_PREFIX(field_rng)(int i) {
  return new FieldT(libff::SHA512_rng<FieldT>(i));
}

void CURVE_PREFIX(field_delete)(FieldT* f) {
  delete f;
}

void CURVE_PREFIX(field_print)(FieldT* f) {
  f->print();
}

// bigint r
libff::bigint<R_LIMBS>* CURVE_PREFIX(bigint_r_of_field)(FieldT* x) {
  return new libff::bigint<R_LIMBS>(x->as_bigint());
}

libff::bigint<R_LIMBS>* CURVE_PREFIX(bigint_r_of_decimal_string)(char* x) {
  return new libff::bigint<R_LIMBS>(x);
}

int CURVE_PREFIX(bigint_r_num_limbs)() {
  return R_LIMBS;
}

char* CURVE_PREFIX(bigint_r_to_data)(libff::bigint<R_LIMBS>* x) {
  return (char *) x->data;
}

libff::bigint<R_LIMBS>* CURVE_PREFIX(bigint_r_of_data)(char* s) {
  libff::bigint<R_LIMBS>* result = new libff::bigint<R_LIMBS>();

  mp_limb_t* arr = (mp_limb_t *) s;

  for (int i = 0; i < R_LIMBS; ++i) {
    result->data[i] = arr[i];
  }

  return result;
}

int CURVE_PREFIX(bigint_r_bytes_per_limb)() {
  return sizeof(mp_limb_t);
}

libff::bigint<R_LIMBS>* CURVE_PREFIX(bigint_r_div)(
  libff::bigint<R_LIMBS>* x,
  libff::bigint<R_LIMBS>* y) {
  mpz_t n; mpz_init(n); x->to_mpz(n);
  mpz_t d; mpz_init(d); y->to_mpz(d);
  mpz_t q; mpz_init(q);

  mpz_fdiv_q(q, n, d);

  return new libff::bigint<R_LIMBS>(q);
}

FieldT* CURVE_PREFIX(bigint_r_to_field)(libff::bigint<R_LIMBS>* n) {
  return new FieldT(*n);
}

libff::bigint<R_LIMBS>* CURVE_PREFIX(bigint_r_of_numeral)(const unsigned char* s, int s_length, int base) {
  libff::bigint<R_LIMBS>* res = new libff::bigint<R_LIMBS>();

  assert (base >= 2 && base <= 256);

  mp_size_t limbs_written = mpn_set_str(res->data, s, s_length, base);
  assert(limbs_written <= R_LIMBS);

  return res;
}

int CURVE_PREFIX(bigint_r_compare)(
    libff::bigint<R_LIMBS>* n1,
    libff::bigint<R_LIMBS>* n2) {
  return mpn_cmp(n1->data, n2->data, R_LIMBS);
}

bool CURVE_PREFIX(bigint_r_test_bit)(libff::bigint<R_LIMBS>* n, int i) {
  return n->test_bit(i);
}

void CURVE_PREFIX(bigint_r_delete)(libff::bigint<R_LIMBS>* n) {
  delete n;
}

void CURVE_PREFIX(bigint_r_print)(libff::bigint<R_LIMBS>* n) {
  n->print();
}

std::vector<long>* CURVE_PREFIX(bigint_r_find_wnaf)(
    size_t window_size, libff::bigint<R_LIMBS>* scalar) {
  return new std::vector<long>(libff::find_wnaf(window_size, *scalar));
}

// bigint q
bool CURVE_PREFIX(bigint_q_test_bit)(libff::bigint<Q_LIMBS>* n, int i) {
  return n->test_bit(i);
}

void CURVE_PREFIX(bigint_q_delete)(libff::bigint<Q_LIMBS>* n) {
  delete n;
}

void CURVE_PREFIX(bigint_q_print)(libff::bigint<Q_LIMBS>* n) {
  n->print();
}

std::vector<long>* CURVE_PREFIX(bigint_q_find_wnaf)(
    size_t window_size, libff::bigint<Q_LIMBS>* scalar) {
  return new std::vector<long>(libff::find_wnaf(window_size, *scalar));
}

bool CURVE_PREFIX(field_equal)(FieldT* x1, FieldT* x2) {
  return *x1 == *x2;
}

// begin linear_combination_vector
std::vector<linear_combination<FieldT>>* CURVE_PREFIX(linear_combination_vector_create)() {
  return new std::vector<linear_combination<FieldT>>();
}

void CURVE_PREFIX(linear_combination_vector_delete)(std::vector<linear_combination<FieldT>>* v) {
  delete v;
}

int CURVE_PREFIX(linear_combination_vector_length)(std::vector<linear_combination<FieldT>> *v) {
  return v->size();
}

void CURVE_PREFIX(linear_combination_vector_emplace_back)(std::vector<linear_combination<FieldT>>* v, linear_combination<FieldT>* x) {
  v->emplace_back(*x);
}

linear_combination<FieldT>* CURVE_PREFIX(linear_combination_vector_get)(std::vector<linear_combination<FieldT>>* v, int i) {
  linear_combination<FieldT> res = (*v)[i];
  return new linear_combination<FieldT>(res);
}
// end linear_combination_vector

linear_combination<FieldT>* CURVE_PREFIX(linear_combination_create)() {
  return new linear_combination<FieldT>();
}

void CURVE_PREFIX(linear_combination_add_term)(linear_combination<FieldT>* lc, FieldT* coeff, variable<FieldT>* v) {
  lc->add_term(*v, *coeff);
}

linear_combination<FieldT>* CURVE_PREFIX(linear_combination_of_var)(variable<FieldT>* v) {
  return new linear_combination<FieldT>(*v);
}

linear_combination<FieldT>* CURVE_PREFIX(linear_combination_of_int)(int n) {
  return new linear_combination<FieldT>(n);
}

linear_combination<FieldT>* CURVE_PREFIX(linear_combination_of_field)(FieldT* x) {
  return new linear_combination<FieldT>(*x);
}

void CURVE_PREFIX(linear_combination_delete)(linear_combination<FieldT>* lc) {
  delete lc;
}

void CURVE_PREFIX(linear_combination_print)(linear_combination<FieldT>* lc) {
  lc->print();
}

std::vector<linear_term<FieldT> >* CURVE_PREFIX(linear_combination_terms)(linear_combination<FieldT>* lc) {
  return new std::vector<linear_term<FieldT>>(lc->terms);
}

linear_combination<FieldT>* CURVE_PREFIX(linear_combination_var_add)(variable<FieldT>* v, linear_combination<FieldT>* other) {
  linear_combination<FieldT>* result = new linear_combination<FieldT>();

  result->add_term(*v);
  result->terms.insert(result->terms.begin(), other->terms.begin(), other->terms.end());
  return result;
}

linear_combination<FieldT>* CURVE_PREFIX(linear_combination_var_sub)(variable<FieldT>* v, linear_combination<FieldT>* other) {
  auto neg = -(*other);
  return CURVE_PREFIX(linear_combination_var_add)(v, &neg);
}

linear_combination<FieldT>* CURVE_PREFIX(linear_combination_of_terms)(std::vector<linear_term<FieldT>>* v) {
  return new linear_combination<FieldT>(*v);
}

linear_term<FieldT>* CURVE_PREFIX(linear_combination_term_create)(FieldT* x, variable<FieldT>* v) {
  return new linear_term<FieldT>(*v, *x);
}

void CURVE_PREFIX(linear_combination_term_delete)(linear_term<FieldT>* lt) {
  delete lt;
}

FieldT* CURVE_PREFIX(linear_combination_term_coeff)(linear_term<FieldT>* lt) {
  return new FieldT(lt->coeff);
}

int CURVE_PREFIX(linear_combination_term_index)(linear_term<FieldT>* lt) {
  return lt->index;
}

std::vector<linear_term<FieldT>>* CURVE_PREFIX(linear_combination_term_vector_create)() {
  return new std::vector<linear_term<FieldT>>();
}

void CURVE_PREFIX(linear_combination_term_vector_delete)(std::vector<linear_term<FieldT>>* v) {
  delete v;
}

int CURVE_PREFIX(linear_combination_term_vector_length)(std::vector<linear_term<FieldT>> *v) {
  return v->size();
}

// Not too sure what's going on here memory-wise...
void CURVE_PREFIX(linear_combination_term_vector_emplace_back)(std::vector<linear_term<FieldT>>* v, linear_term<FieldT>* x) {
  v->emplace_back(*x);
}

linear_term<FieldT>* CURVE_PREFIX(linear_combination_term_vector_get)(std::vector<linear_term<FieldT>>* v, int i) {
  linear_term<FieldT> res = (*v)[i];
  return new linear_term<FieldT>(res);
}

r1cs_constraint<FieldT>* CURVE_PREFIX(r1cs_constraint_create)(
    linear_combination<FieldT>* a,
    linear_combination<FieldT>* b,
    linear_combination<FieldT>* c) {
  return new r1cs_constraint<FieldT>(*a, *b, *c);
}

void CURVE_PREFIX(r1cs_constraint_delete)(r1cs_constraint<FieldT>* c) {
  delete c;
}

void CURVE_PREFIX(r1cs_constraint_set_is_square)(r1cs_constraint<FieldT>* c, bool is_square) {
  c->is_square = is_square;
}

linear_combination<FieldT>* CURVE_PREFIX(r1cs_constraint_a)(r1cs_constraint<FieldT>* c) {
  return &c->a;
}

linear_combination<FieldT>* CURVE_PREFIX(r1cs_constraint_b)(r1cs_constraint<FieldT>* c) {
  return &c->b;
}

linear_combination<FieldT>* CURVE_PREFIX(r1cs_constraint_c)(r1cs_constraint<FieldT>* c) {
  return &c->c;
}

r1cs_constraint_system<FieldT>* CURVE_PREFIX(r1cs_constraint_system_create)() {
  return new r1cs_constraint_system<FieldT>();
}

void CURVE_PREFIX(r1cs_constraint_system_clear)(r1cs_constraint_system<FieldT>* sys) {
  sys->primary_input_size = 0;
  sys->auxiliary_input_size = 0;
  sys->num_square_constraints = 0;
  sys->constraints.clear();
}

void CURVE_PREFIX(linear_combination_update_digest)(
    linear_combination<FieldT>& lc,
    MD5_CTX* ctx) {
  long coeff_size_in_bytes = R_LIMBS * sizeof(mp_limb_t);

  std::vector<linear_term<FieldT>>& terms = lc.terms;
  for (size_t i = 0; i < terms.size(); ++i) {
    size_t index = terms[i].index;
    FieldT coeff = terms[i].coeff;
    MD5_Update(ctx, (void*) &index, (sizeof index));
    MD5_Update(ctx, coeff.as_bigint().data, coeff_size_in_bytes);
  }
}

std::string* CURVE_PREFIX(r1cs_constraint_system_digest)(
    r1cs_constraint_system<FieldT>* sys) {
  MD5_CTX ctx;
  MD5_Init(&ctx);

  std::vector<r1cs_constraint<FieldT>>& cs = sys->constraints;

  for (size_t i = 0; i < cs.size(); ++i) {
    r1cs_constraint<FieldT> c = cs[i];
    CURVE_PREFIX(linear_combination_update_digest)(c.a, &ctx);
    CURVE_PREFIX(linear_combination_update_digest)(c.b, &ctx);
    CURVE_PREFIX(linear_combination_update_digest)(c.c, &ctx);
  }

  std::string* result = new std::string(MD5_DIGEST_LENGTH, '\0');
  MD5_Final((unsigned char *) result->c_str(), &ctx);
  return result;
}

bool CURVE_PREFIX(r1cs_constraint_system_is_satisfied)(
    r1cs_constraint_system<FieldT>* sys,
    const r1cs_primary_input<FieldT>* primary_input,
    const r1cs_auxiliary_input<FieldT>* auxiliary_input
    ) {
  return sys->is_satisfied(*primary_input, *auxiliary_input);
}

bool CURVE_PREFIX(linear_combination_check)(
    size_t total_input_size,
    linear_combination<FieldT>& lc) {
  std::vector<linear_term<FieldT>>& terms = lc.terms;
  for (size_t i = 0; i < terms.size(); ++i) {
    if (terms[i].index > total_input_size) {
      return false;
    }
  }
  return true;
}

bool CURVE_PREFIX(r1cs_constraint_system_check)(r1cs_constraint_system<FieldT>* sys) {
  std::vector<r1cs_constraint<FieldT>>& cs = sys->constraints;
  size_t total_input_size = sys->num_variables();
  for (size_t i = 0; i < cs.size(); ++i) {
    r1cs_constraint<FieldT> c = cs[i];
    if (!CURVE_PREFIX(linear_combination_check)(total_input_size, c.a)) { return false; }
    if (!CURVE_PREFIX(linear_combination_check)(total_input_size, c.b)) { return false; }
    if (!CURVE_PREFIX(linear_combination_check)(total_input_size, c.c)) { return false; }
  }
  return true;
}

void CURVE_PREFIX(r1cs_constraint_system_delete)(r1cs_constraint_system<FieldT>* sys) {
  delete sys;
}

void CURVE_PREFIX(r1cs_constraint_system_report_statistics)(r1cs_constraint_system<FieldT>* sys) {
  sys->report_linear_constraint_statistics();
}

void CURVE_PREFIX(r1cs_constraint_system_swap_AB_if_beneficial)(
    r1cs_constraint_system<FieldT>* sys) {
  sys->swap_AB_if_beneficial();
}

void CURVE_PREFIX(r1cs_constraint_system_add_constraint)(
    r1cs_constraint_system<FieldT>* sys,
    r1cs_constraint<FieldT>* c) {
  sys->add_constraint(*c);
}

void CURVE_PREFIX(r1cs_constraint_system_add_constraint_with_annotation)(
    r1cs_constraint_system<FieldT>* sys,
    r1cs_constraint<FieldT>* c,
    char* s) {
  std::string str(s);
  sys->add_constraint(*c, str);
}

void CURVE_PREFIX(r1cs_constraint_system_set_primary_input_size)(
    r1cs_constraint_system<FieldT>* sys,
    int n) {
  sys->primary_input_size = n;
}

void CURVE_PREFIX(r1cs_constraint_system_set_auxiliary_input_size)(
    r1cs_constraint_system<FieldT>* sys,
    int n) {
  sys->auxiliary_input_size = n;
}

int CURVE_PREFIX(r1cs_constraint_system_get_primary_input_size)(
    r1cs_constraint_system<FieldT>* sys) {
  return sys->primary_input_size;
}

int CURVE_PREFIX(r1cs_constraint_system_get_auxiliary_input_size)(
    r1cs_constraint_system<FieldT>* sys) {
  return sys->auxiliary_input_size;
}

void CURVE_PREFIX(r1cs_constraint_system_iter)(
    r1cs_constraint_system<FieldT>* sys,
    void (*f)(const r1cs_constraint<FieldT> *)) {
  std::vector<r1cs_constraint<FieldT>>& cs = sys->constraints;
  for (std::vector<r1cs_constraint<FieldT>>::const_iterator i = cs.cbegin(); i != cs.cend(); i++) {
    f(&*i);
  }
}

std::vector<FieldT>* CURVE_PREFIX(field_vector_create)() {
  return new std::vector<FieldT>();
}

int CURVE_PREFIX(field_vector_length)(std::vector<FieldT> *v) {
  return v->size();
}

// Not too sure what's going on here memory-wise...
void CURVE_PREFIX(field_vector_emplace_back)(std::vector<FieldT>* v, FieldT* x) {
  v->emplace_back(*x);
}

FieldT* CURVE_PREFIX(field_vector_get)(std::vector<FieldT>* v, int i) {
  FieldT res = (*v)[i];
  return new FieldT(res);
}

void CURVE_PREFIX(field_vector_delete)(std::vector<FieldT>* v) {
  delete v;
}

// Begin ppzksnark specific code
r1cs_constraint_system<FieldT>* CURVE_PREFIX(proving_key_r1cs_constraint_system)(
    r1cs_gg_ppzksnark_proving_key<ppT>* pk) {
  return &pk->constraint_system;
}

void CURVE_PREFIX(proving_key_set_constraint_system)(
    r1cs_gg_ppzksnark_proving_key<ppT>* pk, r1cs_constraint_system<FieldT>* r1cs) {
  pk->constraint_system = *r1cs;
}

std::string* CURVE_PREFIX(proving_key_to_string)(r1cs_gg_ppzksnark_proving_key<ppT>* pk) {
  std::stringstream stream;
  stream << *pk;
  return new std::string(stream.str());
}

r1cs_gg_ppzksnark_proving_key<ppT>* CURVE_PREFIX(proving_key_of_string)(std::string* s) {
  r1cs_gg_ppzksnark_proving_key<ppT>*  pk = new r1cs_gg_ppzksnark_proving_key<ppT>();
  std::stringstream stream(*s);
  stream >> *pk;
  return pk;
}

}

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

extern "C" {

std::vector<libff::G1<ppT>> *CURVE_PREFIX(proving_key_preprocess_A)(
    int C,
    r1cs_gg_ppzksnark_proving_key<ppT> *pk)
{
    std::vector<libff::G1<ppT>> *ret = new std::vector<libff::G1<ppT>>();
    generate_multiples<libff::G1<ppT>>(C, pk->A_query, ret);
    return ret;
}

std::vector<libff::G1<ppT>> *CURVE_PREFIX(proving_key_preprocess_B1)(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<ppT> *pk)
{
    std::vector<libff::G1<ppT>> *ret = new std::vector<libff::G1<ppT>>();
    std::vector<libff::G1<ppT>> B1;
    for (size_t i = 0; i < pk->B_query.size(); i++) {
        B1.emplace_back(pk->B_query[i].h);
    }
    /* Pad to the size of A_query.

       TODO(Matt): Dig into what exactly is going on here.
       We're reading past the end of the vector, but we *have to* to get the
       values that we need. Omitting them or just padding with zeros both fail.
    */
    for (size_t i = pk->B_query.size(); i < pk->A_query.size(); i++) {
        B1.emplace_back(pk->B_query[i].h);
    }
    generate_multiples<libff::G1<ppT>>(C, B1, ret);
    return ret;
}

std::vector<libff::G2<ppT>> *CURVE_PREFIX(proving_key_preprocess_B2)(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<ppT> *pk)
{
    std::vector<libff::G2<ppT>> *ret = new std::vector<libff::G2<ppT>>();
    std::vector<libff::G2<ppT>> B2;
    for (size_t i = 0; i < pk->B_query.size(); i++) {
        B2.emplace_back(pk->B_query[i].g);
    }
    /* Pad to the size of A_query.

       TODO(Matt): Dig into what exactly is going on here.
       We're reading past the end of the vector, but we *have to* to get the
       values that we need. Omitting them or just padding with zeros both fail.
    */
    for (size_t i = pk->B_query.size(); i < pk->A_query.size(); i++) {
        B2.emplace_back(pk->B_query[i].g);
    }
    generate_multiples<libff::G2<ppT>>(C, B2, ret);
    return ret;
}

std::vector<libff::G1<ppT>> *CURVE_PREFIX(proving_key_preprocess_L)(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<ppT> *pk)
{
    std::vector<libff::G1<ppT>> *ret = new std::vector<libff::G1<ppT>>();
    generate_multiples<libff::G1<ppT>>(C, pk->L_query, ret);
    return ret;
}

std::vector<libff::G1<ppT>> *CURVE_PREFIX(proving_key_preprocess_H)(
    int C,
    libsnark::r1cs_gg_ppzksnark_proving_key<ppT> *pk)
{
    std::vector<libff::G1<ppT>> *ret = new std::vector<libff::G1<ppT>>();
    generate_multiples<libff::G1<ppT>>(C, pk->H_query, ret);
    return ret;
}

void CURVE_PREFIX(proving_key_delete)(r1cs_gg_ppzksnark_proving_key<ppT>* pk) {
  delete pk;
}

void CURVE_PREFIX(verification_key_delete)(r1cs_gg_ppzksnark_verification_key<ppT>* vk) {
  delete vk;
}

int CURVE_PREFIX(verification_key_size_in_bits)(
    r1cs_gg_ppzksnark_verification_key<ppT>* vk
) {
  return vk->size_in_bits();
}

std::string* CURVE_PREFIX(verification_key_to_string)(r1cs_gg_ppzksnark_verification_key<ppT>* vk) {
  std::stringstream stream;
  stream << *vk;
  return new std::string(stream.str());
}

r1cs_gg_ppzksnark_verification_key<ppT>* CURVE_PREFIX(verification_key_of_string)(std::string* s) {
  r1cs_gg_ppzksnark_verification_key<ppT>*  vk = new r1cs_gg_ppzksnark_verification_key<ppT>();
  std::stringstream stream(*s);
  stream >> *vk;
  return vk;
}

std::vector<libff::G1<ppT>>*
CURVE_PREFIX(verification_key_query)(r1cs_gg_ppzksnark_verification_key<ppT>* vk) {
  std::vector<libff::G1<ppT>>* res = new std::vector<libff::G1<ppT>>();
  res->emplace_back(vk->ABC_g1.first);
  for (size_t i = 0; i < vk->ABC_g1.rest.values.size(); ++i)
  {
      res->emplace_back(vk->ABC_g1.rest.values[i]);
  }
  return res;
}

libff::G2<ppT>*
CURVE_PREFIX(verification_key_delta)(r1cs_gg_ppzksnark_verification_key<ppT>* vk) {
  return new libff::G2<ppT>(vk->delta_g2);
}

libff::Fqk<ppT>*
CURVE_PREFIX(verification_key_alpha_beta)(r1cs_gg_ppzksnark_verification_key<ppT>* vk) {
  return new libff::Fqk<ppT>(vk->alpha_g1_beta_g2);
}

r1cs_gg_ppzksnark_proving_key<ppT>* CURVE_PREFIX(keypair_pk)(r1cs_gg_ppzksnark_keypair<ppT>* keypair) {
  return new r1cs_gg_ppzksnark_proving_key<ppT>(keypair->pk);
}

r1cs_gg_ppzksnark_verification_key<ppT>* CURVE_PREFIX(keypair_vk)(r1cs_gg_ppzksnark_keypair<ppT>* keypair) {
  return new r1cs_gg_ppzksnark_verification_key<ppT>(keypair->vk);
}

void CURVE_PREFIX(keypair_delete)(r1cs_gg_ppzksnark_keypair<ppT>* keypair) {
  delete keypair;
}

r1cs_gg_ppzksnark_keypair<ppT>* CURVE_PREFIX(keypair_create)(
    r1cs_constraint_system<FieldT>* sys) {
  r1cs_gg_ppzksnark_keypair<ppT> res = r1cs_gg_ppzksnark_generator<ppT>(*sys);
  return new r1cs_gg_ppzksnark_keypair<ppT>(res);
}

std::string* CURVE_PREFIX(proof_to_string)(
    r1cs_gg_ppzksnark_proof<ppT>* p) {
  std::stringstream stream;
  stream << *p;
  return new std::string(stream.str());
}

r1cs_gg_ppzksnark_proof<ppT>* CURVE_PREFIX(proof_of_string)(std::string* s) {
  r1cs_gg_ppzksnark_proof<ppT>*  p = new r1cs_gg_ppzksnark_proof<ppT>();
  std::stringstream stream(*s);
  stream >> *p;
  return p;
}

r1cs_gg_ppzksnark_proof<ppT>* CURVE_PREFIX(proof_create)(
    r1cs_gg_ppzksnark_proving_key<ppT>* key,
    std::vector<FieldT>* primary_input,
    std::vector<FieldT>* auxiliary_input) {
  auto res = r1cs_gg_ppzksnark_prover(*key, *primary_input, *auxiliary_input);
  return new r1cs_gg_ppzksnark_proof<ppT>(res);
}

bool CURVE_PREFIX(proof_verify_components)(
    libff::G1<ppT>* a,
    libff::G2<ppT>* b,
    libff::G1<ppT>* c,
    r1cs_gg_ppzksnark_verification_key<ppT>* key,
    std::vector<FieldT>* primary_input) {

  r1cs_gg_ppzksnark_proof<ppT> p = r1cs_gg_ppzksnark_proof<ppT>();
  p.g_A = *a;
  p.g_B = *b;
  p.g_C = *c;

  return r1cs_gg_ppzksnark_verifier_weak_IC(*key, *primary_input, p);
}

void CURVE_PREFIX(proof_delete)(r1cs_gg_ppzksnark_proof<ppT>* proof) {
  delete proof;
}

bool CURVE_PREFIX(proof_verify)(
    r1cs_gg_ppzksnark_proof<ppT>* proof,
    r1cs_gg_ppzksnark_verification_key<ppT>* key,
    std::vector<FieldT>* primary_input) {
  return r1cs_gg_ppzksnark_verifier_weak_IC(*key, *primary_input, *proof);
}

r1cs_gg_ppzksnark_proof<ppT>* CURVE_PREFIX(proof_dummy)() {
  return new r1cs_gg_ppzksnark_proof<ppT>();
}

libff::G1<ppT>* CURVE_PREFIX(proof_a)(r1cs_gg_ppzksnark_proof<ppT>* proof) {
  return new libff::G1<ppT>(proof->g_A);
}

libff::G2<ppT>* CURVE_PREFIX(proof_b)(r1cs_gg_ppzksnark_proof<ppT>* proof) {
  return new libff::G2<ppT>(proof->g_B);
}

libff::G1<ppT>* CURVE_PREFIX(proof_c)(r1cs_gg_ppzksnark_proof<ppT>* proof) {
  return new libff::G1<ppT>(proof->g_C);
}

// End gg_ppzksnark specific code

// Begin Groth-Maller specific code
r1cs_constraint_system<FieldT>* CURVE_PREFIX(gm_proving_key_r1cs_constraint_system)(
    r1cs_se_ppzksnark_proving_key<ppT>* pk) {
  return &pk->constraint_system;
}

void CURVE_PREFIX(gm_proving_key_set_constraint_system)(
    r1cs_se_ppzksnark_proving_key<ppT>* pk, r1cs_constraint_system<FieldT>* r1cs) {
  pk->constraint_system = *r1cs;
}

std::string* CURVE_PREFIX(gm_proving_key_to_string)(r1cs_se_ppzksnark_proving_key<ppT>* pk) {
  std::stringstream stream;
  stream << *pk;
  return new std::string(stream.str());
}

r1cs_se_ppzksnark_proving_key<ppT>* CURVE_PREFIX(gm_proving_key_of_string)(std::string* s) {
  r1cs_se_ppzksnark_proving_key<ppT>*  pk = new r1cs_se_ppzksnark_proving_key<ppT>();
  std::stringstream stream(*s);
  stream >> *pk;
  return pk;
}

void CURVE_PREFIX(gm_proving_key_delete)(r1cs_se_ppzksnark_proving_key<ppT>* pk) {
  delete pk;
}

void CURVE_PREFIX(gm_verification_key_delete)(r1cs_se_ppzksnark_verification_key<ppT>* vk) {
  delete vk;
}

int CURVE_PREFIX(gm_verification_key_size_in_bits)(
    r1cs_se_ppzksnark_verification_key<ppT>* vk
) {
  return vk->size_in_bits();
}

libff::G2<ppT>*
CURVE_PREFIX(gm_verification_key_h)(
    r1cs_se_ppzksnark_verification_key<ppT>* vk
) {
  return new libff::G2<ppT>(vk->H);
}

libff::G1<ppT>*
CURVE_PREFIX(gm_verification_key_g_alpha)(
    r1cs_se_ppzksnark_verification_key<ppT>* vk
) {
  return new libff::G1<ppT>(vk->G_alpha);
}

libff::G2<ppT>*
CURVE_PREFIX(gm_verification_key_h_beta)(
    r1cs_se_ppzksnark_verification_key<ppT>* vk
) {
  return new libff::G2<ppT>(vk->H_beta);
}

libff::G1<ppT>*
CURVE_PREFIX(gm_verification_key_g_gamma) (
    r1cs_se_ppzksnark_verification_key<ppT>* vk
) {
  return new libff::G1<ppT>(vk->G_gamma);
}

libff::G2<ppT>*
CURVE_PREFIX(gm_verification_key_h_gamma) (
    r1cs_se_ppzksnark_verification_key<ppT>* vk
) {
  return new libff::G2<ppT>(vk->H_gamma);
}

libff::Fqk<ppT>*
CURVE_PREFIX(gm_verification_key_g_alpha_h_beta) (
    r1cs_se_ppzksnark_verification_key<ppT>* vk
) {
  return new libff::Fqk<ppT>(vk->G_alpha_H_beta);
}

std::vector< libff::G1<ppT> >*
CURVE_PREFIX(gm_verification_key_query)(
    r1cs_se_ppzksnark_verification_key<ppT> *vk)
{
    return new std::vector<libff::G1<ppT>>(vk->query);
}

std::string* CURVE_PREFIX(gm_verification_key_to_string)(r1cs_se_ppzksnark_verification_key<ppT>* vk) {
  std::stringstream stream;
  stream << *vk;
  return new std::string(stream.str());
}

r1cs_se_ppzksnark_verification_key<ppT>* CURVE_PREFIX(gm_verification_key_of_string)(std::string* s) {
  r1cs_se_ppzksnark_verification_key<ppT>*  vk = new r1cs_se_ppzksnark_verification_key<ppT>();
  std::stringstream stream(*s);
  stream >> *vk;
  return vk;
}

r1cs_se_ppzksnark_proving_key<ppT>* CURVE_PREFIX(gm_keypair_pk)(r1cs_se_ppzksnark_keypair<ppT>* keypair) {
  return new r1cs_se_ppzksnark_proving_key<ppT>(keypair->pk);
}

r1cs_se_ppzksnark_verification_key<ppT>* CURVE_PREFIX(gm_keypair_vk)(r1cs_se_ppzksnark_keypair<ppT>* keypair) {
  return new r1cs_se_ppzksnark_verification_key<ppT>(keypair->vk);
}

void CURVE_PREFIX(gm_keypair_delete)(r1cs_se_ppzksnark_keypair<ppT>* keypair) {
  delete keypair;
}

r1cs_se_ppzksnark_keypair<ppT>* CURVE_PREFIX(gm_keypair_create)(
    r1cs_constraint_system<FieldT>* sys) {
  r1cs_se_ppzksnark_keypair<ppT> res = r1cs_se_ppzksnark_generator<ppT>(*sys);
  return new r1cs_se_ppzksnark_keypair<ppT>(res);
}

std::string* CURVE_PREFIX(gm_proof_to_string)(
    r1cs_se_ppzksnark_proof<ppT>* p) {
  std::stringstream stream;
  stream << *p;
  return new std::string(stream.str());
}

r1cs_se_ppzksnark_proof<ppT>* CURVE_PREFIX(gm_proof_of_string)(std::string* s) {
  r1cs_se_ppzksnark_proof<ppT>*  p = new r1cs_se_ppzksnark_proof<ppT>();
  std::stringstream stream(*s);
  stream >> *p;
  return p;
}

r1cs_se_ppzksnark_proof<ppT>* CURVE_PREFIX(gm_proof_create)(
    r1cs_se_ppzksnark_proving_key<ppT>* key,
    std::vector<FieldT>* primary_input,
    std::vector<FieldT>* auxiliary_input) {
  auto res = r1cs_se_ppzksnark_prover(*key, *primary_input, *auxiliary_input);
  return new r1cs_se_ppzksnark_proof<ppT>(res);
}

void CURVE_PREFIX(gm_proof_delete)(r1cs_se_ppzksnark_proof<ppT>* proof) {
  delete proof;
}

bool CURVE_PREFIX(gm_proof_verify)(
    r1cs_se_ppzksnark_proof<ppT>* proof,
    r1cs_se_ppzksnark_verification_key<ppT>* key,
    std::vector<FieldT>* primary_input) {
  return r1cs_se_ppzksnark_verifier_weak_IC(*key, *primary_input, *proof);
}

r1cs_se_ppzksnark_proof<ppT>* CURVE_PREFIX(gm_proof_dummy)() {
  return new r1cs_se_ppzksnark_proof<ppT>();
}

libff::G1<ppT>* CURVE_PREFIX(gm_proof_a)(r1cs_se_ppzksnark_proof<ppT>* proof) {
  return new libff::G1<ppT>(proof->A);
}

libff::G2<ppT>* CURVE_PREFIX(gm_proof_b)(r1cs_se_ppzksnark_proof<ppT>* proof) {
  return new libff::G2<ppT>(proof->B);
}

libff::G1<ppT>* CURVE_PREFIX(gm_proof_c)(r1cs_se_ppzksnark_proof<ppT>* proof) {
  return new libff::G1<ppT>(proof->C);
}

// End Groth-Maller specific code

// Begin BG specific code
r1cs_constraint_system<FieldT>* CURVE_PREFIX(bg_proving_key_r1cs_constraint_system)(
    r1cs_bg_ppzksnark_proving_key<ppT>* pk) {
  return &pk->constraint_system;
}

void CURVE_PREFIX(bg_proving_key_set_constraint_system)(
    r1cs_bg_ppzksnark_proving_key<ppT>* pk, r1cs_constraint_system<FieldT>* r1cs) {
  pk->constraint_system = *r1cs;
}

std::string* CURVE_PREFIX(bg_proving_key_to_string)(r1cs_bg_ppzksnark_proving_key<ppT>* pk) {
  std::stringstream stream;
  stream << *pk;
  return new std::string(stream.str());
}

r1cs_bg_ppzksnark_proving_key<ppT>* CURVE_PREFIX(bg_proving_key_of_string)(std::string* s) {
  r1cs_bg_ppzksnark_proving_key<ppT>*  pk = new r1cs_bg_ppzksnark_proving_key<ppT>();
  std::stringstream stream(*s);
  stream >> *pk;
  return pk;
}

void CURVE_PREFIX(bg_proving_key_delete)(r1cs_bg_ppzksnark_proving_key<ppT>* pk) {
  delete pk;
}

void CURVE_PREFIX(bg_verification_key_delete)(r1cs_bg_ppzksnark_verification_key<ppT>* vk) {
  delete vk;
}

int CURVE_PREFIX(bg_verification_key_size_in_bits)(
    r1cs_bg_ppzksnark_verification_key<ppT>* vk
) {
  return vk->size_in_bits();
}

std::string* CURVE_PREFIX(bg_verification_key_to_string)(r1cs_bg_ppzksnark_verification_key<ppT>* vk) {
  std::stringstream stream;
  stream << *vk;
  return new std::string(stream.str());
}

r1cs_bg_ppzksnark_verification_key<ppT>* CURVE_PREFIX(bg_verification_key_of_string)(std::string* s) {
  r1cs_bg_ppzksnark_verification_key<ppT>*  vk = new r1cs_bg_ppzksnark_verification_key<ppT>();
  std::stringstream stream(*s);
  stream >> *vk;
  return vk;
}

std::vector<libff::G1<ppT>>*
CURVE_PREFIX(bg_verification_key_query)(r1cs_bg_ppzksnark_verification_key<ppT>* vk) {
  std::vector<libff::G1<ppT>>* res = new std::vector<libff::G1<ppT>>();
  res->emplace_back(vk->ABC_g1.first);
  for (size_t i = 0; i < vk->ABC_g1.rest.values.size(); ++i)
  {
      res->emplace_back(vk->ABC_g1.rest.values[i]);
  }
  return res;
}

libff::G2<ppT>*
CURVE_PREFIX(bg_verification_key_delta)(r1cs_bg_ppzksnark_verification_key<ppT>* vk) {
  return new libff::G2<ppT>(vk->delta_g2);
}

libff::Fqk<ppT>*
CURVE_PREFIX(bg_verification_key_alpha_beta)(r1cs_bg_ppzksnark_verification_key<ppT>* vk) {
  return new libff::Fqk<ppT>(vk->alpha_g1_beta_g2);
}

r1cs_bg_ppzksnark_proving_key<ppT>* CURVE_PREFIX(bg_keypair_pk)(r1cs_bg_ppzksnark_keypair<ppT>* keypair) {
  return new r1cs_bg_ppzksnark_proving_key<ppT>(keypair->pk);
}

r1cs_bg_ppzksnark_verification_key<ppT>* CURVE_PREFIX(bg_keypair_vk)(r1cs_bg_ppzksnark_keypair<ppT>* keypair) {
  return new r1cs_bg_ppzksnark_verification_key<ppT>(keypair->vk);
}

void CURVE_PREFIX(bg_keypair_delete)(r1cs_bg_ppzksnark_keypair<ppT>* keypair) {
  delete keypair;
}

r1cs_bg_ppzksnark_keypair<ppT>* CURVE_PREFIX(bg_keypair_create)(
    r1cs_constraint_system<FieldT>* sys) {
  r1cs_bg_ppzksnark_keypair<ppT> res = r1cs_bg_ppzksnark_generator<ppT>(*sys);
  return new r1cs_bg_ppzksnark_keypair<ppT>(res);
}

std::string* CURVE_PREFIX(bg_proof_to_string)(
    r1cs_bg_ppzksnark_proof<ppT>* p) {
  std::stringstream stream;
  stream << *p;
  return new std::string(stream.str());
}

r1cs_bg_ppzksnark_proof<ppT>* CURVE_PREFIX(bg_proof_of_string)(std::string* s) {
  r1cs_bg_ppzksnark_proof<ppT>*  p = new r1cs_bg_ppzksnark_proof<ppT>();
  std::stringstream stream(*s);
  stream >> *p;
  return p;
}

r1cs_bg_ppzksnark_proof<ppT>* CURVE_PREFIX(bg_proof_create)(
    r1cs_bg_ppzksnark_proving_key<ppT>* key,
    FieldT* d,
    std::vector<FieldT>* primary_input,
    std::vector<FieldT>* auxiliary_input) {
  auto res = r1cs_bg_ppzksnark_prover(*key, *d, *primary_input, *auxiliary_input);
  return new r1cs_bg_ppzksnark_proof<ppT>(res);
}

bool CURVE_PREFIX(bg_proof_verify_components)(
    libff::G1<ppT>* a,
    libff::G2<ppT>* b,
    libff::G1<ppT>* c,
    libff::G2<ppT>* delta_prime,
    libff::G1<ppT>* z,
    libff::G1<ppT>* y_s,
    r1cs_bg_ppzksnark_verification_key<ppT>* key,
    std::vector<FieldT>* primary_input) {
  r1cs_bg_ppzksnark_proof<ppT> p = r1cs_bg_ppzksnark_proof<ppT>();
  p.g_A = *a;
  p.g_B = *b;
  p.g_C = *c;
  p.delta_prime = *delta_prime;
  p.z = *z;
  p.y_s = *y_s;
  return r1cs_bg_ppzksnark_verifier_strong_IC(*key, *primary_input, p);
}

void CURVE_PREFIX(bg_proof_delete)(r1cs_bg_ppzksnark_proof<ppT>* proof) {
  delete proof;
}

r1cs_bg_ppzksnark_proof<ppT>* CURVE_PREFIX(bg_proof_dummy)() {
  return new r1cs_bg_ppzksnark_proof<ppT>();
}

libff::G1<ppT>* CURVE_PREFIX(bg_proof_a)(r1cs_bg_ppzksnark_proof<ppT>* proof) {
  return new libff::G1<ppT>(proof->g_A);
}

libff::G2<ppT>* CURVE_PREFIX(bg_proof_b)(r1cs_bg_ppzksnark_proof<ppT>* proof) {
  return new libff::G2<ppT>(proof->g_B);
}

libff::G1<ppT>* CURVE_PREFIX(bg_proof_c)(r1cs_bg_ppzksnark_proof<ppT>* proof) {
  return new libff::G1<ppT>(proof->g_C);
}

libff::G2<ppT>* CURVE_PREFIX(bg_proof_delta_prime)(r1cs_bg_ppzksnark_proof<ppT>* proof) {
  return new libff::G2<ppT>(proof->delta_prime);
}

// End BG specific code

}
