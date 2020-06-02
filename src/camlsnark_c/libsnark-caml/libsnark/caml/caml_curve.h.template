// Defines bool
#include <stdbool.h>
// Defines size_t
#include <stddef.h>

void CURVE_PREFIX(init_public_params)();

int CURVE_PREFIX(field_size_in_bits)();

void* CURVE_PREFIX(field_size)();

void* CURVE_PREFIX(var_create)(int i);

void CURVE_PREFIX(var_delete)(void* v);

size_t CURVE_PREFIX(var_index)(void* v);

bool CURVE_PREFIX(field_is_square)(void* x);

void* CURVE_PREFIX(field_sqrt)(void* x);

void* CURVE_PREFIX(field_random)();

void* CURVE_PREFIX(field_of_int)(long n);

void* CURVE_PREFIX(field_inv)(void* x);

void* CURVE_PREFIX(field_square)(void* x);

void* CURVE_PREFIX(field_add)(void* x, void* y);

void* CURVE_PREFIX(field_mul)(void* x, void* y);

void* CURVE_PREFIX(field_sub)(void* x, void* y);

void CURVE_PREFIX(field_mut_add)(void* x, void* y);

void CURVE_PREFIX(field_mut_mul)(void* x, void* y);

void CURVE_PREFIX(field_mut_sub)(void* x, void* y);

void CURVE_PREFIX(field_copy)(void* x, void* y);

void* CURVE_PREFIX(field_rng)(int i);

void CURVE_PREFIX(field_delete)(void* f);

void CURVE_PREFIX(field_print)(void* f);

// bigint r
void* CURVE_PREFIX(bigint_r_of_field)(void* x);

void* CURVE_PREFIX(bigint_r_of_decimal_string)(void* x);

int CURVE_PREFIX(bigint_r_num_limbs)();

char* CURVE_PREFIX(bigint_r_to_data)(void* x);

void* CURVE_PREFIX(bigint_r_of_data)(char* s);

int CURVE_PREFIX(bigint_r_bytes_per_limb)();

void* CURVE_PREFIX(bigint_r_div)(
  void* x,
  void* y);

void* CURVE_PREFIX(bigint_r_to_field)(void* n);

void* CURVE_PREFIX(bigint_r_of_numeral)(const unsigned char* s, int s_length, int base);

int CURVE_PREFIX(bigint_r_compare)(
    void* n1,
    void* n2);

bool CURVE_PREFIX(bigint_r_test_bit)(void* n, int i);

void CURVE_PREFIX(bigint_r_delete)(void* n);

void CURVE_PREFIX(bigint_r_print)(void* n);

void* CURVE_PREFIX(bigint_r_find_wnaf)(
    size_t window_size, void* scalar);

// bigint q
bool CURVE_PREFIX(bigint_q_test_bit)(void* n, int i);

void CURVE_PREFIX(bigint_q_delete)(void* n);

void CURVE_PREFIX(bigint_q_print)(void* n);

void* CURVE_PREFIX(bigint_q_find_wnaf)(
    size_t window_size, void* scalar);

bool CURVE_PREFIX(field_equal)(void* x1, void* x2);

// begin linear_combination_vector
void* CURVE_PREFIX(linear_combination_vector_create)();

void CURVE_PREFIX(linear_combination_vector_delete)(void* v);

int CURVE_PREFIX(linear_combination_vector_length)(void* v);

void CURVE_PREFIX(linear_combination_vector_emplace_back)(void* v, void* x);

void* CURVE_PREFIX(linear_combination_vector_get)(void* v, int i);
// end linear_combination_vector

void* CURVE_PREFIX(linear_combination_create)();

void CURVE_PREFIX(linear_combination_add_term)(void* lc, void* coeff, void* v);

void* CURVE_PREFIX(linear_combination_of_var)(void* v);

void* CURVE_PREFIX(linear_combination_of_int)(int n);

void* CURVE_PREFIX(linear_combination_of_field)(void* x);

void CURVE_PREFIX(linear_combination_delete)(void* lc);

void CURVE_PREFIX(linear_combination_print)(void* lc);

void* CURVE_PREFIX(linear_combination_terms)(void* lc);

void* CURVE_PREFIX(linear_combination_var_add)(void* v, void* other);

void* CURVE_PREFIX(linear_combination_var_sub)(void* v, void* other);

void* CURVE_PREFIX(linear_combination_of_terms)(void* v);

void* CURVE_PREFIX(linear_combination_term_create)(void* x, void* v);

void CURVE_PREFIX(linear_combination_term_delete)(void* lt);

void* CURVE_PREFIX(linear_combination_term_coeff)(void* lt);

int CURVE_PREFIX(linear_combination_term_index)(void* lt);

void* CURVE_PREFIX(linear_combination_term_vector_create)();

void CURVE_PREFIX(linear_combination_term_vector_delete)(void* v);

int CURVE_PREFIX(linear_combination_term_vector_length)(void* v);

// Not too sure what's going on here memory-wise...
void CURVE_PREFIX(linear_combination_term_vector_emplace_back)(void* v, void* x);

void* CURVE_PREFIX(linear_combination_term_vector_get)(void* v, int i);

void* CURVE_PREFIX(r1cs_constraint_create)(
    void* a,
    void* b,
    void* c);

void CURVE_PREFIX(r1cs_constraint_delete)(void* c);

void CURVE_PREFIX(r1cs_constraint_set_is_square)(void* c, bool is_square);

void* CURVE_PREFIX(r1cs_constraint_a)(void* c);

void* CURVE_PREFIX(r1cs_constraint_b)(void* c);

void* CURVE_PREFIX(r1cs_constraint_c)(void* c);

void* CURVE_PREFIX(r1cs_constraint_system_create)();

void CURVE_PREFIX(r1cs_constraint_system_clear)(void* sys);

void CURVE_PREFIX(linear_combination_update_digest)(
    void* lc,
    void* ctx);

void* CURVE_PREFIX(r1cs_constraint_system_digest)(
    void* sys);

bool CURVE_PREFIX(r1cs_constraint_system_is_satisfied)(
    void* sys,
    const void* primary_input,
    const void* auxiliary_input
    );

bool CURVE_PREFIX(linear_combination_check)(
    size_t total_input_size,
    void* lc);

bool CURVE_PREFIX(r1cs_constraint_system_check)(void* sys);

void CURVE_PREFIX(r1cs_constraint_system_delete)(void* sys);

void CURVE_PREFIX(r1cs_constraint_system_report_statistics)(void* sys);

void CURVE_PREFIX(r1cs_constraint_system_swap_AB_if_beneficial)(
    void* sys);

void CURVE_PREFIX(r1cs_constraint_system_add_constraint)(
    void* sys, 
    void* c);

void CURVE_PREFIX(r1cs_constraint_system_add_constraint_with_annotation)(
    void* sys, 
    void* c,
    void* s);

void CURVE_PREFIX(r1cs_constraint_system_set_primary_input_size)(
    void* sys, 
    int n);

void CURVE_PREFIX(r1cs_constraint_system_set_auxiliary_input_size)(
    void* sys, 
    int n);

int CURVE_PREFIX(r1cs_constraint_system_get_primary_input_size)(
    void* sys);

int CURVE_PREFIX(r1cs_constraint_system_get_auxiliary_input_size)(
    void* sys);

void CURVE_PREFIX(r1cs_constraint_system_iter)(
    void* sys,
    void (*f)(const void*));

void* CURVE_PREFIX(field_vector_create)();

int CURVE_PREFIX(field_vector_length)(void* v);

// Not too sure what's going on here memory-wise...
void CURVE_PREFIX(field_vector_emplace_back)(void* v, void* x);

void* CURVE_PREFIX(field_vector_get)(void* v, int i);

void CURVE_PREFIX(field_vector_delete)(void* v);

// Begin ppzksnark specific code
void* CURVE_PREFIX(proving_key_r1cs_constraint_system)(
    void* pk);

void* CURVE_PREFIX(proving_key_to_string)(void* pk);

void* CURVE_PREFIX(proving_key_of_string)(void* s);

void CURVE_PREFIX(proving_key_delete)(void* pk);

void CURVE_PREFIX(verification_key_delete)(void* vk);

int CURVE_PREFIX(verification_key_size_in_bits)(
    void* vk
);

void* CURVE_PREFIX(verification_key_to_string)(void* vk);

void* CURVE_PREFIX(verification_key_of_string)(void* s);

void*
CURVE_PREFIX(verification_key_query)(void* vk);

void*
CURVE_PREFIX(verification_key_delta)(void* vk);

void*
CURVE_PREFIX(verification_key_alpha_beta)(void* vk);

void* CURVE_PREFIX(keypair_pk)(void* keypair);

void* CURVE_PREFIX(keypair_vk)(void* keypair);

void CURVE_PREFIX(keypair_delete)(void* keypair);

void* CURVE_PREFIX(keypair_create)(
    void* sys);

void* CURVE_PREFIX(proof_to_string)(
    void* p);

void* CURVE_PREFIX(proof_of_string)(void* s);

void* CURVE_PREFIX(proof_create)(
    void* key,
    void* primary_input,
    void* auxiliary_input);

void CURVE_PREFIX(proof_delete)(void* proof);

bool CURVE_PREFIX(proof_verify)(
    void* proof,
    void* key,
    void* primary_input);

void* CURVE_PREFIX(proof_dummy)();

void* CURVE_PREFIX(proof_a)(void* proof);

void* CURVE_PREFIX(proof_b)(void* proof);

void* CURVE_PREFIX(proof_c)(void* proof);

// End gg_ppzksnark specific code

// Begin Groth-Maller specific code
void* CURVE_PREFIX(gm_proving_key_r1cs_constraint_system)(
    void* pk);

void* CURVE_PREFIX(gm_proving_key_to_string)(void* pk);

void* CURVE_PREFIX(gm_proving_key_of_string)(void* s);

void CURVE_PREFIX(gm_proving_key_delete)(void* pk);

void CURVE_PREFIX(gm_verification_key_delete)(void* vk);

int CURVE_PREFIX(gm_verification_key_size_in_bits)(
    void* vk
);

void* 
CURVE_PREFIX(gm_verification_key_h)(
    void* vk
);

void* 
CURVE_PREFIX(gm_verification_key_g_alpha)(
    void* vk
);

void* 
CURVE_PREFIX(gm_verification_key_h_beta)(
    void* vk
);

void* 
CURVE_PREFIX(gm_verification_key_g_gamma) (
    void* vk
);

void* 
CURVE_PREFIX(gm_verification_key_h_gamma) (
    void* vk
);

void* 
CURVE_PREFIX(gm_verification_key_g_alpha_h_beta) (
    void* vk
);

void*
CURVE_PREFIX(gm_verification_key_query)(
    void* vk);

void* CURVE_PREFIX(gm_verification_key_to_string)(void* vk);

void* CURVE_PREFIX(gm_verification_key_of_string)(void* s);

void* CURVE_PREFIX(gm_keypair_pk)(void* keypair);

void* CURVE_PREFIX(gm_keypair_vk)(void* keypair);

void CURVE_PREFIX(gm_keypair_delete)(void* keypair);

void* CURVE_PREFIX(gm_keypair_create)(
    void* sys);

void* CURVE_PREFIX(gm_proof_to_string)(
    void* p);

void* CURVE_PREFIX(gm_proof_of_string)(void* s);

void* CURVE_PREFIX(gm_proof_create)(
    void* key,
    void* primary_input,
    void* auxiliary_input);

void CURVE_PREFIX(gm_proof_delete)(void* proof);

bool CURVE_PREFIX(gm_proof_verify)(
    void* proof,
    void* key,
    void* primary_input);

void* CURVE_PREFIX(gm_proof_dummy)();

void* CURVE_PREFIX(gm_proof_a)(void* proof);

void* CURVE_PREFIX(gm_proof_b)(void* proof);

void* CURVE_PREFIX(gm_proof_c)(void* proof);

// End Groth-Maller specific code

// Begin BG specific code
void* CURVE_PREFIX(bg_proving_key_r1cs_constraint_system)(
    void* pk);

void* CURVE_PREFIX(bg_proving_key_to_string)(void* pk);

void* CURVE_PREFIX(bg_proving_key_of_string)(void* s);

void CURVE_PREFIX(bg_proving_key_delete)(void* pk);

void CURVE_PREFIX(bg_verification_key_delete)(void* vk);

int CURVE_PREFIX(bg_verification_key_size_in_bits)(
    void* vk
);

void* CURVE_PREFIX(bg_verification_key_to_string)(void* vk);

void* CURVE_PREFIX(bg_verification_key_of_string)(void* s);

void*
CURVE_PREFIX(bg_verification_key_query)(void* vk);

void*
CURVE_PREFIX(bg_verification_key_delta)(void* vk);

void*
CURVE_PREFIX(bg_verification_key_alpha_beta)(void* vk);

void* CURVE_PREFIX(bg_keypair_pk)(void* keypair);

void* CURVE_PREFIX(bg_keypair_vk)(void* keypair);

void CURVE_PREFIX(bg_keypair_delete)(void* keypair);

void* CURVE_PREFIX(bg_keypair_create)(
    void* sys);

void* CURVE_PREFIX(bg_proof_to_string)(
    void* p);

void* CURVE_PREFIX(bg_proof_of_string)(void* s);

void* CURVE_PREFIX(bg_proof_create)(
    void* key,
    void* d,
    void* primary_input,
    void* auxiliary_input);

bool CURVE_PREFIX(bg_proof_verify_components)(
    void* a,
    void* b,
    void* c,
    void* delta_prime,
    void* z,
    void* y_s,
    void* key,
    void* primary_input);

void CURVE_PREFIX(bg_proof_delete)(void* proof);

void* CURVE_PREFIX(bg_proof_dummy)();

void* CURVE_PREFIX(bg_proof_a)(void* proof);

void* CURVE_PREFIX(bg_proof_b)(void* proof);

void* CURVE_PREFIX(bg_proof_c)(void* proof);

void* CURVE_PREFIX(bg_proof_delta_prime)(void* proof);

// End BG specific code
