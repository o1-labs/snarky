#include <libiop/common/common.hpp>

namespace libiop {

std::size_t log2(std::size_t n)
{
    std::size_t r = ((n & (n-1)) == 0 ? 0 : 1); // add 1 if n is not power of 2

    while (n > 1)
    {
        n >>= 1;
        r++;
    }

    return r;
}

 /* If n is a power of 2, returns n */
std::size_t round_to_next_power_of_2(const std::size_t n)
{
    return (1ull << log2(n));
}

bool is_power_of_2(const std::size_t n)
{
    return ((n != 0) && ((n & (n-1)) == 0));
}

std::size_t bitreverse(std::size_t n, const std::size_t l)
{
    std::size_t r = 0;
    for (std::size_t k = 0; k < l; ++k)
    {
        r = (r << 1) | (n & 1);
        n >>= 1;
    }
    return r;
}

long double add_soundness_error_bits(const size_t bits1, const size_t bits2)
{
    return add_soundness_error_bits((long double)bits1, (long double)bits2);
}

long double add_soundness_error_bits(const long double bits1, const long double bits2)
{
    long double result = exp2l(-1.0 * bits1) + exp2l(-1.0 * bits2);
    return -1 * log2l(result);
}

template<typename T>
void print_vector(std::vector<T> &vec)
{
    printf("{ ");
    for (auto const& elem : vec)
    {
        std::cout << elem << " ";
    }
    printf("}\n");
}

template<typename T>
void print_vector(std::vector<T> vec)
{
    printf("{ ");
    for (auto const& elem : vec)
    {
        std::cout << elem << " ";
    }
    printf("}\n");
}

} // namespace libiop
