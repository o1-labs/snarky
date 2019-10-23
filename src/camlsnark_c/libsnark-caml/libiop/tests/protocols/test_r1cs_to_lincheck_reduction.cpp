#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/common/common.hpp"
#include "libiop/protocols/encoded/r1cs_rs_iop/r1cs_rs_iop.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"
#include "libiop/relations/r1cs.hpp"
#include "libiop/relations/variable.hpp"
#include "libiop/tests/protocols/utilities.cpp"

namespace libiop {

template<typename FieldT>
std::tuple<iop_protocol<FieldT>, encoded_aurora_protocol<FieldT>> run_protocol(const r1cs_example<FieldT> r1cs_system,
    const bool make_zk, const std::size_t query_bound,
    const field_subset<FieldT> constraint_domain, const field_subset<FieldT> variable_domain,
    const field_subset<FieldT> codeword_domain, const field_subset_type domain_type)
{
    iop_protocol<FieldT> IOP;

    const domain_handle constraint_domain_handle = IOP.register_domain(constraint_domain);
    const domain_handle variable_domain_handle = IOP.register_domain(variable_domain);
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);

    const size_t summation_domain_dim = std::max<size_t>(constraint_domain.dimension(), variable_domain.dimension());
    const size_t dummy_security_parameter = 64;
    const bool holographic = false;
    const encoded_aurora_parameters<FieldT> params(dummy_security_parameter,
                                                   codeword_domain.dimension(),
                                                   constraint_domain.dimension(),
                                                   summation_domain_dim,
                                                   query_bound,
                                                   make_zk,
                                                   holographic,
                                                   domain_type);

    std::shared_ptr<r1cs_constraint_system<FieldT>> cs =
        std::make_shared<r1cs_constraint_system<FieldT>>(r1cs_system.constraint_system_);

    encoded_aurora_protocol<FieldT> proto(IOP,
                                          constraint_domain_handle,
                                          variable_domain_handle,
                                          codeword_domain_handle,
                                          cs,
                                          params);

    proto.register_challenge();
    proto.register_proof();
    IOP.seal_interaction_registrations();

    IOP.seal_query_registrations();

    /* Proving */
    proto.submit_witness_oracles(r1cs_system.primary_input_, r1cs_system.auxiliary_input_);
    IOP.signal_prover_round_done();

    proto.calculate_and_submit_proof();
    IOP.signal_prover_round_done();
    return std::make_tuple(IOP, proto);
}

// Packages protected parameters to avoid long function parameters / repeated FFT's
template<typename FieldT>
struct r1cs_testing_struct {
    r1cs_example<FieldT> r1cs_params;

    field_subset<FieldT> constraint_domain_;
    field_subset<FieldT> input_variable_domain_;
    field_subset<FieldT> variable_domain_;
    field_subset<FieldT> summation_domain_;
    field_subset<FieldT> codeword_domain_;

    std::vector<FieldT> Az_vec;
    std::vector<FieldT> Bz_vec;
    std::vector<FieldT> Cz_vec;
    std::vector<FieldT> fz_vec;

    std::vector<FieldT> fz_over_summation_dom;

    bool make_zk_;
    field_subset_type field_subset_type_;
    std::size_t query_bound_;

    r1cs_testing_struct() {}
};

template<typename FieldT>
void test_fw(const std::vector<FieldT> &fw_over_codeword_domain,
             std::vector<FieldT> f_1v_coeff,
             const r1cs_testing_struct<FieldT> *params){
    const vanishing_polynomial<FieldT> input_vp(params->input_variable_domain_);
    std::vector<FieldT> input_vp_over_var_domain =
        input_vp.evaluations_over_field_subset(params->variable_domain_);
    r1cs_auxiliary_input<FieldT> auxiliary_input = params->r1cs_params.auxiliary_input_;

    std::vector<FieldT> fw_prime_coeff = IFFT_over_field_subset<FieldT>(fw_over_codeword_domain, params->codeword_domain_);

    polynomial<FieldT> f_1v_poly(std::move(f_1v_coeff));
    polynomial<FieldT> fw_prime_poly(std::move(fw_prime_coeff));

    if (params->make_zk_) {
        // fw_prime is supposed to be of degree n - k + b = |w| + b
        ASSERT_EQ(fw_prime_poly.minimal_num_terms(), auxiliary_input.size() + params->query_bound_) <<
            "fw_prime had different degree than expected in the zero knowledge case.";
    } else {
        // fw_prime is supposed to be degree n - k = |w|
        ASSERT_EQ(fw_prime_poly.minimal_num_terms(), auxiliary_input.size()) <<
            "fw_prime had degree not equal to the witness size";
    }
    std::size_t primary_input_size = params->r1cs_params.primary_input_.size();
    for (std::size_t i = 0; i < auxiliary_input.size(); ++i)
    {
        const std::size_t index = params->variable_domain_.reindex_by_subset(
            params->input_variable_domain_.dimension(), i + (primary_input_size + 1));
        const FieldT b = params->variable_domain_.element_by_index(index);
        FieldT binv = input_vp_over_var_domain[index].inverse();
        FieldT lhs = fw_prime_poly.evaluation_at_point(b);
        FieldT rhs = (auxiliary_input[i] - f_1v_poly.evaluation_at_point(b)) * binv;
        ASSERT_TRUE(lhs == rhs) << "fw was not consistent on the " << i << "th element of the witness";
    }
}

template<typename FieldT>
void test_f1v(std::vector<FieldT> f_1v_coeff,
            const r1cs_testing_struct<FieldT> *params)
{
    polynomial<FieldT> f_1v(std::move(f_1v_coeff));
    r1cs_primary_input<FieldT> primary_input = params->r1cs_params.primary_input_;
    FieldT zeroElem = params->input_variable_domain_.element_by_index(0);
    ASSERT_TRUE(f_1v.evaluation_at_point(zeroElem) == FieldT(1)) << "f_1v(0) != 1";
    for (std::size_t i = 0; i < primary_input.size(); ++i)
    {
        const FieldT b = params->input_variable_domain_.element_by_index(i + 1);
        FieldT lhs = f_1v.evaluation_at_point(b);
        FieldT rhs = primary_input[i];
        ASSERT_TRUE(lhs == rhs) << "f_1v was not consistent on the " << i << "th element of the primary input";
    }
}

// This runs into an issue where montgomery representation of lhs and rhs is not equal,
// but the values are. Thus it had to be casted to bigint, which required this different method.
template<typename FieldT>
void test_f1v_multiplicative(std::vector<FieldT> f_1v_coeff,
            const r1cs_testing_struct<FieldT> *params)
{
    polynomial<FieldT> f_1v(std::move(f_1v_coeff));
    r1cs_primary_input<FieldT> primary_input = params->r1cs_params.primary_input_;
    FieldT zeroElem = params->input_variable_domain_.element_by_index(0);
    ASSERT_TRUE(f_1v.evaluation_at_point(zeroElem) == FieldT(1)) << "f_1v(0) != 1";
    for (std::size_t i = 0; i < primary_input.size(); ++i)
    {
        const FieldT b = params->input_variable_domain_.element_by_index(i + 1);
        FieldT lhs = f_1v.evaluation_at_point(b);
        FieldT rhs = primary_input[i];
        ASSERT_TRUE(lhs.as_bigint() == rhs.as_bigint()) <<
            "f_1v was not consistent on the " << i << "th element of the primary input";
    }
}

template<typename FieldT>
void test_fz(const r1cs_testing_struct<FieldT> *params) {
    r1cs_primary_input<FieldT> primary_input = params->r1cs_params.primary_input_;
    r1cs_auxiliary_input<FieldT> auxiliary_input = params->r1cs_params.auxiliary_input_;

    ASSERT_TRUE(params->fz_over_summation_dom[0] == FieldT(1)) << "f_z(0) != 1";
    for (std::size_t i = 0; i < primary_input.size(); ++i)
    {
        FieldT lhs = params->fz_over_summation_dom[i + 1];
        FieldT rhs = primary_input[i];
        ASSERT_TRUE(lhs == rhs) << "f_z was not consistent on the " << i << "th element of the primary input";
    }
    for (std::size_t i = 0; i < auxiliary_input.size(); ++i)
    {
        FieldT lhs = params->fz_over_summation_dom[primary_input.size() + i + 1];
        FieldT rhs = auxiliary_input[i];
        ASSERT_TRUE(lhs == rhs) << "f_z was not consistent on the " << i << "th element of the witness";
    }
}

template<typename FieldT>
void test_fz_multiplicative(const r1cs_testing_struct<FieldT> *params) {
    r1cs_primary_input<FieldT> primary_input = params->r1cs_params.primary_input_;
    r1cs_auxiliary_input<FieldT> auxiliary_input = params->r1cs_params.auxiliary_input_;

    ASSERT_TRUE(params->fz_over_summation_dom[0] == FieldT(1)) << "f_z(0) != 1";
    for (std::size_t i = 0; i < primary_input.size(); ++i)
    {
        const std::size_t variable_index = params->variable_domain_.reindex_by_subset(
            params->input_variable_domain_.dimension(), i + 1);
        const std::size_t index = params->summation_domain_.reindex_by_subset(
            params->variable_domain_.dimension(), variable_index);
        FieldT lhs = params->fz_over_summation_dom[index];
        FieldT rhs = primary_input[i];
        ASSERT_TRUE(lhs.as_bigint() == rhs.as_bigint()) << "f_z was not consistent on the " << i << "th element of the primary input";
    }
    for (std::size_t i = 0; i < auxiliary_input.size(); ++i)
    {
        const std::size_t variable_index = params->variable_domain_.reindex_by_subset(
            params->input_variable_domain_.dimension(), i + primary_input.size() + 1);
        const std::size_t index = params->summation_domain_.reindex_by_subset(
            params->variable_domain_.dimension(), variable_index);
        FieldT lhs = params->fz_over_summation_dom[index];
        FieldT rhs = auxiliary_input[i];
        ASSERT_TRUE(lhs.as_bigint() == rhs.as_bigint()) << "f_z was not consistent on the " << i << "th element of the witness";
    }
}

template<typename FieldT>
void test_ABCz_linearity(const r1cs_testing_struct<FieldT> *params) {

    std::vector<FieldT> Az_vec(params->Az_vec);
    std::vector<FieldT> Bz_vec(params->Bz_vec);
    std::vector<FieldT> Cz_vec(params->Cz_vec);
    std::vector<FieldT> fz_vec(params->fz_vec);
    polynomial<FieldT> Az(std::move(Az_vec));
    polynomial<FieldT> Bz(std::move(Bz_vec));
    polynomial<FieldT> Cz(std::move(Cz_vec));
    polynomial<FieldT> fz(std::move(fz_vec));

    r1cs_constraint_system<FieldT> cs = params->r1cs_params.constraint_system_;
    field_subset<FieldT> cons_domain = params->constraint_domain_;
    field_subset<FieldT> var_domain = params->variable_domain_;

    if (params->make_zk_) {
        ASSERT_EQ(Az.minimal_num_terms(), cons_domain.num_elements() + params->query_bound_) <<
            "Az was of incorrect degree in the zero knowledge case, was it masked correctly?";
        ASSERT_EQ(Bz.minimal_num_terms(), cons_domain.num_elements() + params->query_bound_) <<
            "Bz was of incorrect degree in the zero knowledge case, was it masked correctly?";
        ASSERT_EQ(Cz.minimal_num_terms(), cons_domain.num_elements() + params->query_bound_) <<
            "Cz was of incorrect degree in the zero knowledge case, was it masked correctly?";
    } else {
        ASSERT_EQ(Az.minimal_num_terms(), cons_domain.num_elements()) <<
            "Az was of incorrect degree in the non-zk case";
        ASSERT_EQ(Bz.minimal_num_terms(), cons_domain.num_elements()) <<
            "Bz was of incorrect degree in the non-zk case";
        ASSERT_EQ(Cz.minimal_num_terms(), cons_domain.num_elements()) <<
            "Cz was of incorrect degree in the non-zk case";
    }

    for (std::size_t a_ind = 0; a_ind < cons_domain.num_elements(); a_ind++) {
        FieldT alpha = cons_domain.element_by_index(a_ind);
        FieldT lhs_A = Az.evaluation_at_point(alpha);
        FieldT lhs_B = Bz.evaluation_at_point(alpha);
        FieldT lhs_C = Cz.evaluation_at_point(alpha);
        FieldT rhs_A = FieldT(0);
        FieldT rhs_B = FieldT(0);
        FieldT rhs_C = FieldT(0);

        // its a sparse matrix, so must be iterated like this
        for (auto &lt : cs.constraints_[a_ind].a_.terms)
        {
            const std::size_t variable_index = var_domain.reindex_by_subset(
                params->input_variable_domain_.dimension(), lt.index_);
            FieldT b = var_domain.element_by_index(variable_index);
            FieldT A_ab = lt.coeff_;
            rhs_A += A_ab * fz.evaluation_at_point(b);
        }
        for (auto &lt : cs.constraints_[a_ind].b_.terms)
        {
            const std::size_t variable_index = var_domain.reindex_by_subset(
                params->input_variable_domain_.dimension(), lt.index_);
            FieldT b = var_domain.element_by_index(variable_index);
            FieldT B_ab = lt.coeff_;
            rhs_B += B_ab * fz.evaluation_at_point(b);
        }
        for (auto &lt : cs.constraints_[a_ind].c_.terms)
        {
            const std::size_t variable_index = var_domain.reindex_by_subset(
                params->input_variable_domain_.dimension(), lt.index_);
            FieldT b = var_domain.element_by_index(variable_index);
            FieldT C_ab = lt.coeff_;
            rhs_C += C_ab * fz.evaluation_at_point(b);
        }
        ASSERT_TRUE(lhs_A == rhs_A) << "Az was not consistent on the " << a_ind << "th constraint";
        ASSERT_TRUE(lhs_B == rhs_B) << "Bz was not consistent on the " << a_ind << "th constraint";
        ASSERT_TRUE(lhs_C == rhs_C) << "Cz was not consistent on the " << a_ind << "th constraint";
    }
}

template<typename FieldT>
void test_ABCz_row(const r1cs_testing_struct<FieldT> *params) {
    std::vector<FieldT> Az_vec(params->Az_vec);  std::vector<FieldT> Bz_vec(params->Bz_vec);
    std::vector<FieldT> Cz_vec(params->Cz_vec);
    polynomial<FieldT> Az(std::move(Az_vec));    polynomial<FieldT> Bz(std::move(Bz_vec));
    polynomial<FieldT> Cz(std::move(Cz_vec));

    for (std::size_t i = 0; i < params->constraint_domain_.num_elements(); i++) {
        FieldT b = params->constraint_domain_.element_by_index(i);
        FieldT lhs = Az.evaluation_at_point(b) * Bz.evaluation_at_point(b);
        FieldT rhs = Cz.evaluation_at_point(b);
        ASSERT_TRUE(lhs == rhs) << "rowcheck property failed on the " << i << "th entry";
    }
}

/* Friend test, so it can access members of r1cs IOP and ensure their validity individually */
TEST(TestAdditiveR1CSComponents, R1CSTest) {
    typedef gf64 FieldT;

/** Test configurations where constraint dim is greater than, less than, and equal to variable dim,
 *  both zk and non-zk */
for (std::size_t constraint_domain_dim = 7; constraint_domain_dim < 9; constraint_domain_dim++) {
for (std::size_t variable_domain_dim = 7; variable_domain_dim < 9; variable_domain_dim++) {
    std::size_t input_variable_domain_dim = variable_domain_dim - 2;
    for (std::size_t make_zk_param = 0; make_zk_param < 2; make_zk_param++) {
        r1cs_testing_struct<FieldT> params;

        params.make_zk_ = make_zk_param ? true : false;
        params.field_subset_type_ = affine_subspace_type;
        printf("running components test with constraint dim %lu, variable dim %lu, make_zk %s, is_multiplicative %s\n",
            constraint_domain_dim, variable_domain_dim,
            params.make_zk_ ? "true" : "false",
            (params.field_subset_type_ == multiplicative_coset_type) ? "true" : "false");

        const std::size_t num_constraints = 1 << constraint_domain_dim;
        const std::size_t num_inputs = (1 << input_variable_domain_dim) - 1;
        const std::size_t num_variables = (1 << variable_domain_dim) - 1;

        params.r1cs_params = generate_r1cs_example<FieldT>(num_constraints, num_inputs, num_variables);

        const std::size_t RS_extra_dimensions = 2; /* \rho = 1/4 */
        const std::size_t codeword_domain_dim = RS_extra_dimensions +
            + (params.make_zk_ ? 2 : 1)
            + std::max(constraint_domain_dim, variable_domain_dim);

        const field_subset<FieldT> unshifted_codeword_domain(1ull << codeword_domain_dim);
        const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();

        params.constraint_domain_ = field_subset<FieldT>(num_constraints);
        params.variable_domain_ = field_subset<FieldT>(num_variables + 1);
        params.codeword_domain_ = field_subset<FieldT>(1ull << codeword_domain_dim, codeword_domain_shift);

        if (constraint_domain_dim > variable_domain_dim) {
            params.summation_domain_ = params.constraint_domain_;
        } else {
            params.summation_domain_ = params.variable_domain_;
        }
        params.query_bound_ = num_inputs + 2;

        auto iop_and_protocol = run_protocol<FieldT>(params.r1cs_params, params.make_zk_,
            params.query_bound_, params.constraint_domain_, params.variable_domain_, params.codeword_domain_,
            params.field_subset_type_);

        iop_protocol<FieldT> IOP = std::get<0>(iop_and_protocol);
        encoded_aurora_protocol<FieldT> proto = std::get<1>(iop_and_protocol);

        /* Test various stages of the protocol */
        params.input_variable_domain_ = proto.input_variable_domain_;

        params.Az_vec = IFFT_over_field_subset(
            *IOP.get_oracle_evaluations(std::make_shared<oracle_handle>(proto.fAz_handle_)).get(),
            params.codeword_domain_);
        params.Bz_vec = IFFT_over_field_subset(
            *IOP.get_oracle_evaluations(std::make_shared<oracle_handle>(proto.fBz_handle_)).get(),
            params.codeword_domain_);
        params.Cz_vec = IFFT_over_field_subset(
            *IOP.get_oracle_evaluations(std::make_shared<oracle_handle>(proto.fCz_handle_)).get(),
            params.codeword_domain_);
        std::shared_ptr<std::vector<FieldT>> fw_over_codeword_domain = IOP.get_oracle_evaluations(
            std::make_shared<oracle_handle>(proto.fw_handle_));
        std::vector<FieldT> fz_over_codeword_domain = *proto.fz_oracle_->evaluated_contents(
            {fw_over_codeword_domain}).get();
        params.fz_vec = IFFT_over_field_subset(fz_over_codeword_domain, params.codeword_domain_);

        params.fz_over_summation_dom = naive_FFT(
            params.fz_vec, field_subset<FieldT>(params.summation_domain_));

        test_fw(
            *IOP.get_oracle_evaluations(std::make_shared<oracle_handle>(proto.fw_handle_)).get(),
            proto.f_1v_coefficients_,
            &params);
        test_f1v(proto.f_1v_coefficients_, &params);
        test_fz(&params);

        // tests lincheck property holds
        test_ABCz_linearity(&params);
        // tests rowcheck property holds
        test_ABCz_row(&params);

        /* Verification */
        std::vector<oracle_handle_ptr> handles = proto.get_all_oracle_handles();
        test_oracles_degree_and_consistency(IOP, handles, params.codeword_domain_, true);
        }
    }
}}


/* Friend test, so it can access members of r1cs IOP and ensure their validity individually */
TEST(TestMultiplicativeR1CSComponents, R1CSTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

/** Test configurations where constraint dim is greater than, less than, and equal to variable dim,
 *  both zk and non-zk */
for (std::size_t constraint_domain_dim = 6; constraint_domain_dim < 8; constraint_domain_dim++) {
for (std::size_t variable_domain_dim = 6; variable_domain_dim < 8; variable_domain_dim++) {
    std::size_t input_variable_domain_dim = variable_domain_dim - 2;
    for (std::size_t make_zk_param = 0; make_zk_param < 2; make_zk_param++) {
        r1cs_testing_struct<FieldT> params;

        params.make_zk_ = make_zk_param ? true : false;
        params.field_subset_type_ = multiplicative_coset_type;
        printf("running components test with constraint dim %lu, variable dim %lu, make_zk %s, is_multiplicative %s\n",
            constraint_domain_dim, variable_domain_dim,
            params.make_zk_ ? "true" : "false",
            (params.field_subset_type_ == multiplicative_coset_type) ? "true" : "false");

        const std::size_t num_constraints = 1 << constraint_domain_dim;
        const std::size_t num_inputs = (1 << input_variable_domain_dim) - 1;
        const std::size_t num_variables = (1 << variable_domain_dim) - 1;

        const std::size_t summation_domain_dim = std::max(constraint_domain_dim, variable_domain_dim);

        params.r1cs_params = generate_r1cs_example<FieldT>(num_constraints, num_inputs, num_variables);

        const std::size_t RS_extra_dimensions = 2; /* \rho = 1/4 */
        const std::size_t codeword_domain_dim = RS_extra_dimensions +
            + (params.make_zk_ ? 2 : 1)
            + summation_domain_dim;

        const field_subset<FieldT> unshifted_codeword_domain(1ull << codeword_domain_dim);
        const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();

        params.constraint_domain_ = field_subset<FieldT>(num_constraints);
        params.variable_domain_ = field_subset<FieldT>(num_variables + 1);
        params.codeword_domain_ = field_subset<FieldT>(1ull << codeword_domain_dim, codeword_domain_shift);

        if (constraint_domain_dim > variable_domain_dim) {
            params.summation_domain_ = params.constraint_domain_;
        } else {
            params.summation_domain_ = params.variable_domain_;
        }
        params.query_bound_ = num_inputs + 2;

        auto iop_and_protocol = run_protocol<FieldT>(params.r1cs_params, params.make_zk_,
            params.query_bound_, params.constraint_domain_, params.variable_domain_, params.codeword_domain_,
            params.field_subset_type_);

        iop_protocol<FieldT> IOP = std::get<0>(iop_and_protocol);
        encoded_aurora_protocol<FieldT> proto = std::get<1>(iop_and_protocol);

        /* Test various stages of the protocol */
        params.input_variable_domain_ = proto.input_variable_domain_;

        params.Az_vec = IFFT_over_field_subset(
            *IOP.get_oracle_evaluations(std::make_shared<oracle_handle>(proto.fAz_handle_)).get(),
            params.codeword_domain_);
        params.Bz_vec = IFFT_over_field_subset(
            *IOP.get_oracle_evaluations(std::make_shared<oracle_handle>(proto.fBz_handle_)).get(),
            params.codeword_domain_);
        params.Cz_vec = IFFT_over_field_subset(
            *IOP.get_oracle_evaluations(std::make_shared<oracle_handle>(proto.fCz_handle_)).get(),
            params.codeword_domain_);
        std::shared_ptr<std::vector<FieldT>> fw_over_codeword_domain = IOP.get_oracle_evaluations(
            std::make_shared<oracle_handle>(proto.fw_handle_));
        std::vector<FieldT> fz_over_codeword_domain = *proto.fz_oracle_->evaluated_contents(
            {fw_over_codeword_domain}).get();
        params.fz_vec = IFFT_over_field_subset(fz_over_codeword_domain, params.codeword_domain_);

        params.fz_over_summation_dom = naive_FFT(
            params.fz_vec, field_subset<FieldT>(params.summation_domain_));

        test_fw(
            *IOP.get_oracle_evaluations(std::make_shared<oracle_handle>(proto.fw_handle_)).get(),
            proto.f_1v_coefficients_,
            &params);
        test_f1v_multiplicative(proto.f_1v_coefficients_, &params);
        test_fz_multiplicative(&params);

        // tests lincheck property holds
        test_ABCz_linearity(&params);
        // tests rowcheck property holds
        test_ABCz_row(&params);

        /* Verification */
        std::vector<oracle_handle_ptr> handles = proto.get_all_oracle_handles();
        test_oracles_degree_and_consistency(IOP, handles, params.codeword_domain_, true);
        }
    }
}}
}