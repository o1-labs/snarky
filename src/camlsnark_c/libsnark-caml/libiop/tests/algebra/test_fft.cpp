#include <cstdint>
#include <gtest/gtest.h>
#include <vector>

#include <libff/algebra/curves/edwards/edwards_pp.hpp>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/algebra/utils.hpp"

namespace libiop {

template<typename FieldT>
std::vector<FieldT> elementwise_random_vector(const std::size_t count)
{
    std::vector<FieldT> result(count);

    for (size_t i = 0; i < count; ++i)
    {
        result[i] = FieldT::random_element();
    }

    return result;
}

TEST(AdditiveTest, SimpleTest) {
    typedef gf64 FieldT;

    for (size_t m = 1; m <= 11; ++m)
    {
        std::vector<FieldT> poly_coeffs = random_vector<FieldT>(1ull<<m);
        field_subset<FieldT> domain = field_subset<FieldT>(
            affine_subspace<FieldT>::random_affine_subspace(m));

        /* Additive equals naive */
        const std::vector<FieldT> naive_result =
            naive_FFT<FieldT>(poly_coeffs, domain);
        const std::vector<FieldT> additive_result =
            additive_FFT<FieldT>(poly_coeffs, domain.subspace());

        EXPECT_EQ(naive_result, additive_result);

        /* Inverse interpolates naive */
        const std::vector<FieldT> new_naive_result =
            naive_FFT<FieldT>(poly_coeffs, domain);
        const std::vector<FieldT> interpolation =
            additive_IFFT<FieldT>(new_naive_result, domain.subspace());

        EXPECT_EQ(interpolation, poly_coeffs);
    }
}

TEST(MultiplicativeSubgroupTest, SimpleTest) {
    edwards_pp::init_public_params();
    typedef edwards_Fr FieldT;

    for (size_t domain_dim = 1; domain_dim < 10; domain_dim++)
    {
        for (size_t poly_dim = 1; poly_dim <= domain_dim; poly_dim++)
        {
            std::vector<FieldT> poly_coeffs = elementwise_random_vector<FieldT>(1ull<<domain_dim);
            field_subset<FieldT> domain(1ull << domain_dim);

            /* Multiplicative equals naive */
            const std::vector<FieldT> naive_result =
                naive_FFT<FieldT>(poly_coeffs, domain);
            const std::vector<FieldT> multiplicative_result =
                multiplicative_FFT<FieldT>(poly_coeffs, domain.coset());

            EXPECT_EQ(naive_result, multiplicative_result);

            /* Inverse interpolates naive */
            const std::vector<FieldT> new_naive_result =
                naive_FFT<FieldT>(poly_coeffs, domain);
            const std::vector<FieldT> interpolation =
                multiplicative_IFFT<FieldT>(new_naive_result, domain.coset());

            EXPECT_EQ(interpolation, poly_coeffs);
        }
    }
}

TEST(MultiplicativeCosetTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    for (size_t m = 1; m <= 11; ++m)
    {
        std::vector<FieldT> poly_coeffs = elementwise_random_vector<FieldT>(1ull<<m);
        FieldT shift = FieldT::random_element();

        field_subset<FieldT> domain = field_subset<FieldT>(
            multiplicative_coset<FieldT>(1ull<<m, shift));

        /* Multiplicative equals naive */
        const std::vector<FieldT> naive_result =
            naive_FFT<FieldT>(poly_coeffs, domain);
        const std::vector<FieldT> multiplicative_result =
            multiplicative_FFT<FieldT>(poly_coeffs, domain.coset());
        EXPECT_EQ(naive_result, multiplicative_result);

        /* Inverse interpolates naive */
        const std::vector<FieldT> new_naive_result =
            naive_FFT<FieldT>(poly_coeffs, domain);
        const std::vector<FieldT> interpolation =
            multiplicative_IFFT<FieldT>(new_naive_result, domain.coset());
        EXPECT_EQ(interpolation, poly_coeffs);
    }
}

TEST(ExtendedRangeTest, SimpleTest) {
    typedef gf64 FieldT;

    for (size_t m = 1; m <= 21; ++m)
    {
        std::vector<FieldT> poly_coeffs = random_vector<FieldT>(1ull<<m);
        field_subset<FieldT> domain = field_subset<FieldT>(
            affine_subspace<FieldT>::random_affine_subspace(m));

        const std::vector<FieldT> additive_result =
            additive_FFT<FieldT>(poly_coeffs, domain.subspace());
        const std::vector<FieldT> interpolation =
            additive_IFFT<FieldT>(additive_result, domain.subspace());

        EXPECT_EQ(interpolation, poly_coeffs);
    }
}

}