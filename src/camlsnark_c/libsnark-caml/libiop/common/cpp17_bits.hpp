/**@file
*****************************************************************************
Bits of C++17 standard library temporarily included for C++11 compatibility.
*****************************************************************************
* @copyright  ???
*****************************************************************************/
#ifndef LIBIOP_COMMON_CPP17_BITS_HPP_
#define LIBIOP_COMMON_CPP17_BITS_HPP_

namespace libiop {

// http://en.cppreference.com/w/cpp/algorithm/unique

template<class ForwardIt>
ForwardIt std__unique(ForwardIt first, ForwardIt last)
{
    if (first == last)
        return last;

    ForwardIt result = first;
    while (++first != last) {
        if (!(*result == *first) && ++result != first) {
            *result = std::move(*first);
        }
    }
    return ++result;
}

// http://en.cppreference.com/w/cpp/algorithm/adjacent_find
template<class ForwardIt, class BinaryPredicate>
ForwardIt std__adjacent_find(ForwardIt first, ForwardIt last,
                             BinaryPredicate p)
{
    if (first == last) {
        return last;
    }
    ForwardIt next = first;
    ++next;
    for (; next != last; ++next, ++first) {
        if (p(*first, *next)) {
            return first;
        }
    }
    return last;
}

} // libiop

#endif // LIBIOP_COMMON_CPP17_BITS_HPP_
