#include <cstdint>
#include <gtest/gtest.h>

#include "libiop/common/common.hpp"

namespace libiop {

TEST(Log2Test, SimpleTest) {
    EXPECT_EQ(log2(0), 0ull);
    EXPECT_EQ(log2(1), 0ull);
    EXPECT_EQ(log2(2), 1ull);
    EXPECT_EQ(log2(3), 2ull);
    EXPECT_EQ(log2(4), 2ull);
    EXPECT_EQ(log2(5), 3ull);
    EXPECT_EQ(log2(6), 3ull);
    EXPECT_EQ(log2(7), 3ull);
    EXPECT_EQ(log2(8), 3ull);
    EXPECT_EQ(log2(9), 4ull);
}

TEST(Log2Test, PowersOfTwo) {
    for (std::size_t i = 10; i < 20; ++i)
    {
        const std::size_t k = (1ull<<i);
        EXPECT_EQ(log2(k-1), i);
        EXPECT_EQ(log2(k), i);
        EXPECT_EQ(log2(k+1), i+1);
    }
}

}
