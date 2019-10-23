#include <cstdint>
#include <gtest/gtest.h>
#include <vector>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/exponentiation.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>

namespace libiop {

template<typename FieldT>
FieldT power_naive(const FieldT &base, const std::size_t exponent)
{
    FieldT result = FieldT::one();

    for (std::size_t i = 1; i <= exponent; ++i)
    {
        result *= base;
    }

    return result;
}

template<typename FieldT>
std::vector<FieldT> domain_element_powers_naive(const field_subset<FieldT> &S,
                                                const std::size_t exponent)
{
    std::vector<FieldT> result;

    for (auto &el : S.all_elements())
    {
        result.emplace_back(libiop::power(el, exponent));
    }

    return result;
}


TEST(ExponentiationTest, SimpleTest) {
    typedef gf64 FieldT;

    const std::size_t max_power = 1000;
    FieldT X = FieldT::random_element();

    FieldT X_i = FieldT::one();
    for (std::size_t i = 0 ; i < max_power; ++i)
    {
        const FieldT X_i_naive = power_naive<FieldT>(X, i);
        const FieldT X_i_square_and_multiply = power<FieldT>(X, i);

        EXPECT_EQ(X_i, X_i_naive);
        EXPECT_EQ(X_i, X_i_square_and_multiply);

        X_i *= X;
    }
}

TEST(ExponentiationTest, SubspaceElementPowersTest) {
    typedef gf64 FieldT;

    const std::size_t dimension = 10;
    const std::size_t max_power = 1000;

    const affine_subspace<FieldT> S = affine_subspace<FieldT>::random_affine_subspace(dimension);

    for (std::size_t i = 0 ; i < max_power; ++i)
    {
        const std::vector<FieldT> S_to_i_naive =
            domain_element_powers_naive(field_subset<FieldT>(S), i);

        const std::vector<FieldT> S_to_i_linearized_poly =
            subspace_element_powers(S, i);

        EXPECT_EQ(S_to_i_naive, S_to_i_linearized_poly);
    }
}

TEST(ExponentiationTest, CosetElementPowersTest) {    
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    const std::size_t dimension = 10;
    const std::size_t max_power = 1000;

    FieldT shift = FieldT::multiplicative_generator;
    const multiplicative_coset<FieldT> S = multiplicative_coset<FieldT>(dimension, shift);

    for (std::size_t i = 0 ; i < max_power; ++i)
    {
        const std::vector<FieldT> S_to_i_naive =
            domain_element_powers_naive(field_subset<FieldT>(S), i);

        const std::vector<FieldT> S_to_i =
            coset_element_powers(S, i);

        EXPECT_TRUE(S_to_i_naive == S_to_i);
    }
}

}
