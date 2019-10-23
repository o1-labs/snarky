#include <cstdint>
#include <gtest/gtest.h>
#include <vector>

#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/common/common.hpp"

namespace libiop {

TEST(BatchInverseTest, SimpleTest) {
    typedef gf64 FieldT;

    const std::size_t sz = 100;
    const std::vector<FieldT> vec = random_vector<FieldT>(sz);
    const std::vector<FieldT> vec_inv = batch_inverse<FieldT>(vec);

    for (std::size_t i = 0; i < sz; ++i)
    {
        EXPECT_EQ(vec[i] * vec_inv[i], FieldT(1));
    }

    std::vector<FieldT> vec_inv2 = vec;
    mut_batch_inverse(vec_inv2);

    for (std::size_t i = 0; i < sz; ++i)
    {
        EXPECT_EQ(vec[i] * vec_inv2[i], FieldT(1));
    }
}

TEST(MultiplicativeBatchInverseTest, SimpleTest) {
    libff::alt_bn128_pp::init_public_params();
    typedef libff::alt_bn128_Fr FieldT;

    const std::size_t sz = 100;
    std::vector<FieldT> vec = random_vector<FieldT>(sz);
    std::vector<FieldT> vec_inv = batch_inverse<FieldT>(vec);

    for (std::size_t i = 0; i < sz; ++i)
    {
        EXPECT_TRUE(vec[i] * vec_inv[i] == FieldT(1));
    }

    std::vector<FieldT> vec_inv2 = vec;
    mut_batch_inverse(vec_inv2);

    for (std::size_t i = 0; i < sz; ++i)
    {
        EXPECT_TRUE(vec[i] * vec_inv2[i] == FieldT(1));
    }

    vec[0] = FieldT::zero();
    const bool input_can_contain_zeroes = true;
    vec_inv = batch_inverse<FieldT>(vec, input_can_contain_zeroes);
    ASSERT_TRUE(vec_inv[0] == FieldT::zero());
    for (std::size_t i = 1; i < sz; ++i)
    {
        EXPECT_TRUE(vec[i] * vec_inv[i] == FieldT(1));
    }
}

}
