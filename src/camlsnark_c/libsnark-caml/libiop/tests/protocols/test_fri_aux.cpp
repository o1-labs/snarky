#include <cstdint>
#include <gtest/gtest.h>
#include <vector>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/protocols/ldt/fri/fri_aux.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/utils.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>


namespace libiop {

template<typename FieldT>
void run_lagrange_test(const field_subset<FieldT> &domain) {
    const std::size_t dim = domain.dimension();

    /** TODO: Make a much more rigorous testing method.
      *       This current method is essentially saying choose a polynomial of degree |coset|,
      *       so it can be interpolated from any of its cosets.
      *       run evaluate_next_f_i_over_entire_domain, and check that all points evaluated to P(x_i)
      * */
    const size_t poly_deg = 1ull<<2;
    const size_t num_cosets = domain.num_elements() / poly_deg;
    const polynomial<FieldT> poly = polynomial<FieldT>::random_polynomial(poly_deg);

    const std::vector<FieldT> poly_evals = FFT_over_field_subset(poly.coefficients(), domain);

    const FieldT point = FieldT::random_element();
    const FieldT evaluation = poly.evaluation_at_point(point);

    const std::shared_ptr<std::vector<FieldT>> shared_poly_evals = std::make_shared<std::vector<FieldT>>(poly_evals);
    const std::vector<FieldT> interpolations = *evaluate_next_f_i_over_entire_domain(
        shared_poly_evals, domain, poly_deg, point).get();
    ASSERT_EQ(interpolations.size(), num_cosets);

    const field_subset<FieldT> localizer_domain = domain.get_subset_of_order(poly_deg);
    const vanishing_polynomial<FieldT> vp(localizer_domain);
    ASSERT_TRUE(interpolations[0] == evaluation);
    ASSERT_TRUE(interpolations[1] == evaluation);
    ASSERT_TRUE(interpolations[2] == evaluation);

    const size_t coset_index = std::rand() % num_cosets;
    std::vector<FieldT> coset_evals;
    if (domain.type() == affine_subspace_type) {
        for (size_t i = 0; i < poly_deg; i++) {
            coset_evals.emplace_back(poly_evals[coset_index*poly_deg + i]);
        }
        const field_subset<FieldT> unshifted_coset(affine_subspace<FieldT>(
            localizer_domain.basis(), FieldT::zero()));
        const localizer_polynomial<FieldT> unshifted_vp(unshifted_coset);

        FieldT eval = additive_evaluate_next_f_i_at_coset(
            coset_evals,
            unshifted_coset,
            domain.element_by_index(coset_index*poly_deg),
            unshifted_vp,
            point);
        ASSERT_TRUE(eval == evaluation);
    } else {
        for (size_t i = 0; i < poly_deg; i++) {
            coset_evals.emplace_back(poly_evals[coset_index + i*num_cosets]);
        }
        const field_subset<FieldT> unshifted_coset(localizer_domain.num_elements());
        const localizer_polynomial<FieldT> unshifted_vp(unshifted_coset);

        FieldT eval = multiplicative_evaluate_next_f_i_at_coset(
            coset_evals,
            unshifted_coset.generator(),
            domain.element_by_index(coset_index),
            point);
        ASSERT_TRUE(eval == evaluation);
    }
}

TEST(Test, LagrangeTest) {
    const std::size_t dim = 15;
    const field_subset<gf64> additive_domain(
        affine_subspace<gf64>::random_affine_subspace(dim));
    run_lagrange_test<gf64>(additive_domain);
    libff::edwards_pp::init_public_params();
    const field_subset<libff::edwards_Fr> multiplicative_domain_with_offset(
        1ull << dim, libff::edwards_Fr::multiplicative_generator);
    run_lagrange_test<libff::edwards_Fr>(multiplicative_domain_with_offset);
}

template<typename FieldT>
void run_calculate_next_coset_query_positions_test(
    const field_subset<FieldT> codeword_domain,
    const size_t initial_query_pos,
    const size_t prev_localization_parameter,
    const size_t cur_localization_parameter,
    const std::vector<size_t> expected_query_positions)
{
    /* offset doesn't matter, since we only look at query position in this test. */
    const field_subset<FieldT> localized_domain =
        codeword_domain.get_subset_of_order(1ull << (codeword_domain.dimension() - prev_localization_parameter));

     /* Initialize IOP */
    iop_protocol<FieldT> IOP;
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    IOP.register_oracle(codeword_domain_handle, 0, false);
    IOP.seal_interaction_registrations();

    query_position_handle base_handle =
        IOP.register_deterministic_query_position(
            { },
            [initial_query_pos]
            (const std::vector<std::size_t> &seed_positions)
            -> std::size_t {
                return initial_query_pos;
            });
    std::vector<query_position_handle> query_positions =
        calculate_next_coset_query_positions(IOP, base_handle, codeword_domain, localized_domain,
        prev_localization_parameter, cur_localization_parameter);

    ASSERT_EQ(expected_query_positions.size(), query_positions.size());
    for (size_t i = 0; i < expected_query_positions.size(); i++)
    {
        size_t actual_query_pos = IOP.obtain_query_position(query_positions[i]);
        ASSERT_EQ(actual_query_pos, expected_query_positions[i]);
    }
}

TEST(QueryPositionTest, AdditiveTest) {
    typedef gf64 FieldT;
    size_t codeword_domain_dim = 10;
    FieldT codeword_domain_offset = FieldT::zero();
    size_t prev_localization_param = 2;
    size_t cur_localization_param = 3;
    field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, codeword_domain_offset);
    size_t initial_query_pos = 0;
    std::vector<size_t> expected_query_pos({0,1,2,3,4,5,6,7});
    run_calculate_next_coset_query_positions_test<FieldT>(
        codeword_domain, initial_query_pos, prev_localization_param, cur_localization_param, expected_query_pos);

    /* 2nd element, in the 5th coset */
    initial_query_pos = (1ull << prev_localization_param) * 5 + 1;
    expected_query_pos = std::vector<size_t> ({0,1,2,3,4,5,6,7});
    run_calculate_next_coset_query_positions_test<FieldT>(
        codeword_domain, initial_query_pos, prev_localization_param, cur_localization_param, expected_query_pos);

    /* 3rd element, in the 9th coset */
    initial_query_pos = (1ull << prev_localization_param) * 9 + 2;
    expected_query_pos = std::vector<size_t> ({8,9,10,11,12,13,14,15});
    run_calculate_next_coset_query_positions_test<FieldT>(
        codeword_domain, initial_query_pos, prev_localization_param, cur_localization_param, expected_query_pos);
}

TEST(QueryPositionTest, MultiplicativeTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;

    size_t codeword_domain_dim = 10;
    FieldT codeword_domain_shift = FieldT::one();
    size_t prev_localization_param = 2;
    size_t cur_localization_param = 3;
    field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, codeword_domain_shift);
    size_t initial_query_pos = 0;
    size_t offset = 1ull << (codeword_domain_dim - prev_localization_param - cur_localization_param);
    std::vector<size_t> expected_query_pos({0,offset,2*offset,3*offset,4*offset,5*offset,6*offset,7*offset});
    run_calculate_next_coset_query_positions_test<FieldT>(
        codeword_domain, initial_query_pos, prev_localization_param, cur_localization_param, expected_query_pos);

    /* 2nd element, in the 5th coset */
    initial_query_pos = (1ull << (codeword_domain_dim - 1)) + 5;
    expected_query_pos = std::vector<size_t> (
        {5, 5 + offset,5 + 2*offset, 5 + 3*offset, 5 + 4*offset, 5 + 5*offset, 5 + 6*offset, 5 + 7*offset});

    run_calculate_next_coset_query_positions_test<FieldT>(
        codeword_domain, initial_query_pos, prev_localization_param, cur_localization_param, expected_query_pos);
}

}
