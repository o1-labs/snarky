/** @file
 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef CURVE_UTILS_TCC_
#define CURVE_UTILS_TCC_

namespace libff {

template<typename GroupT, mp_size_t m>
GroupT scalar_mul(const GroupT &base, const bigint<m> &scalar)
{
    GroupT result = GroupT::zero();

    bool found_one = false;
    for (long i = scalar.max_bits() - 1; i >= 0; --i)
    {
        if (found_one)
        {
            result = result.dbl();
        }

        if (scalar.test_bit(i))
        {
            found_one = true;
            result = result + base;
        }
    }

    return result;
}

/* 

  let w = scalar_mul_window_size
  let w = 2^w
  let n = the size of the scalar field in bits

  Return a vector v of length ceil(n / w) * W such that for I < W we have
  v[j*W + I] = (I * 2^{j*w}) g

  so that

    (\sum_{i=0}^{n-1} 2^i b_i) g
  = (\sum_{j=0}^{ceil(n / w) - 1} (\sum_{i=0}^{w - 1} 2^{j*w + i} b_{j*w + i})) g
  = \sum_{j=0}^{ceil(n / w) - 1}
      (\sum_{i=0}^{w - 1} 2^{j*w + i} b_{j*w + i}) g
  = \sum_{j=0}^{ceil(n / w) - 1}
      (\sum_{i=0}^{w - 1} 2^{j*w} 2^i b_{j*w + i}) g
  = \sum_{j=0}^{ceil(n / w) - 1}
      (\sum_{i=0}^{w - 1} 2^i b_{j*w + i}) 2^{j*w} g
  = \sum_{j=0}^{ceil(n / w) - 1}
      v[ j * W + \sum_{i=0}^{w - 1} 2^i b_{j*w + i} ]

*/

template<typename GroupT>
std::vector<GroupT> create_window_table(const GroupT &g) { 
    const size_t n = GroupT::size_in_bits();
    const size_t num_windows = (n + scalar_mul_window_size - 1) / scalar_mul_window_size;
    const size_t W = 1 << scalar_mul_window_size;
    std::vector<GroupT> res;

    GroupT JW_g = GroupT(g);
    // Invariant: JW_g = 2^{j*w} g
    for (size_t j = 0; j < num_windows; ++j) {

        GroupT iJW_g = GroupT::zero();
        // Invariant: iJW_g = i * JW_g;
        for (size_t i = 0; i < W; ++i) {
            res.emplace_back(GroupT(iJW_g));
            iJW_g = iJW_g + JW_g;
        }
        // At this point, iJW_g = W * JW_g = W * 2^{j*w} g = 2^{(j+1) w} g
        JW_g = iJW_g;
    }

    return res;
}

template<typename GroupT, mp_size_t m>
GroupT window_scalar_mul(const std::vector<GroupT> &table, const bigint<m> &scalar) {
    GroupT result = GroupT::zero();

    const size_t num_windows = (scalar.max_bits() + scalar_mul_window_size - 1) / scalar_mul_window_size;
    const size_t W = 1 << scalar_mul_window_size;

    for (size_t j = 0; j < num_windows; ++j) {
        size_t I = 0;

        for (size_t i = 0; i < scalar_mul_window_size; ++i) {
            I = I | (scalar.test_bit(j * scalar_mul_window_size + i) ? (1 << i) : 0);
        }

        result = result + table[j * W + I];
    }

    return result;
}

}

// libff
#endif // CURVE_UTILS_TCC_
