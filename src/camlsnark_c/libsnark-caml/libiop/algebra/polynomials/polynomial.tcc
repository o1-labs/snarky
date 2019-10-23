#include <algorithm>

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/fft.hpp"

namespace libiop {

/* polynomial_base<FieldT> */
template<typename FieldT>
polynomial_base<FieldT>::polynomial_base()
{
}

/* polynomial<FieldT> */
template<typename FieldT>
polynomial<FieldT>::polynomial() :
    polynomial_base<FieldT>()
{
}

template<typename FieldT>
polynomial<FieldT>::polynomial(std::vector<FieldT> &&coefficients) :
    polynomial_base<FieldT>(),
    coefficients_(std::move(coefficients))
{
}

template<typename FieldT>
bool polynomial<FieldT>::coefficients_equivalent_with(const polynomial<FieldT> &other) const
{
    /** Coefficients may begin with many zeros */
    const std::size_t min_size = std::min<std::size_t>(this->coefficients_.size(),
                                                       other.coefficients_.size());

    return (std::equal(this->coefficients_.begin(),
                       this->coefficients_.begin() + min_size,
                       other.coefficients_.begin()) &&
            std::all_of(this->coefficients_.begin() + min_size,
                        this->coefficients_.end(),
                        [](const FieldT &el) { return el.is_zero(); }) &&
            std::all_of(other.coefficients_.begin() + min_size,
                        other.coefficients_.end(),
                        [](const FieldT &el) { return el.is_zero(); }));
}

template<typename FieldT>
void polynomial<FieldT>::multiply_coefficients_by(const FieldT &other)
{
    std::for_each(this->coefficients_.begin(), this->coefficients_.end(),
                  [other](FieldT &el) { el *= other; });
}

template<typename FieldT>
void polynomial<FieldT>::add_coefficients_of(const polynomial<FieldT> &other)
{
    this->coefficients_.reserve(other.coefficients_.size()); /* this is no-op if *this has enough terms already */
    for (size_t i = 0; i < other.coefficients_.size(); ++i)
    {
        if (i < this->coefficients_.size())
        {
            this->coefficients_[i] += other.coefficients_[i];
        }
        else
        {
            this->coefficients_.emplace_back(other.coefficients_[i]);
        }
    }
}

template<typename FieldT>
void polynomial<FieldT>::set_degree(const std::size_t degree_bound, const bool truncate)
{
    if (this->coefficients_.size() == degree_bound+1)
    {
        /* already of the desired degree */
        return;
    }
    if (this->coefficients_.size() < degree_bound+1 || truncate)
    {
        this->coefficients_.resize(degree_bound+1);
        return;
    }

    /* we need to shrink, but are not allowed to truncate, so let's
     * check */
    if (this->num_terms_at_most(degree_bound+1))
    {
        this->coefficients_.resize(degree_bound+1);
        return;
    }
    else
    {
        assert(0); // TODO: add exceptions
    }
}

template<typename FieldT>
FieldT polynomial<FieldT>::evaluation_at_point(const FieldT &evalpoint) const
{
    FieldT result = FieldT(0);

    for (auto it = this->coefficients_.rbegin(); it != this->coefficients_.rend(); ++it)
    {
        result *= evalpoint;
        result += (*it);
    }

    return result;
}

template<typename FieldT>
std::vector<FieldT> polynomial<FieldT>::evaluations_over_field_subset(const field_subset<FieldT> &S) const
{
    return FFT_over_field_subset<FieldT>(this->coefficients(), S);
}

template<typename FieldT>
void polynomial<FieldT>::reserve(const std::size_t degree_bound)
{
    this->coefficients_.reserve(degree_bound);
}

template<typename FieldT>
std::size_t polynomial<FieldT>::capacity() const
{
    return this->coefficients_.capacity();
}

template<typename FieldT>
void polynomial<FieldT>::add_term(FieldT &&coeff)
{
    this->coefficients_.emplace_back(coeff);
}

template<typename FieldT>
void polynomial<FieldT>::remove_term(const std::size_t index)
{
    this->coefficients_.erase(this->coefficients_.begin() + index);
}

template<typename FieldT>
std::size_t polynomial<FieldT>::num_terms() const
{
    return this->coefficients_.size();
}

template<typename FieldT>
std::size_t polynomial<FieldT>::minimal_num_terms() const
/** minimal_num_terms returns the degree of the polynomial,
 * excluding higher order terms having 0 coefficients.
 */
{
    auto it = std::find_if(this->coefficients_.rbegin(),
                           this->coefficients_.rend(),
                           [](const FieldT &el) { return !el.is_zero(); });

    if (it == this->coefficients_.rend())
    {
        return 0;
    }
    else
    {
        return ((this->coefficients_.size()) -
                std::distance(this->coefficients_.rbegin(), it));
    }
}

template<typename FieldT>
bool polynomial<FieldT>::num_terms_at_most(const std::size_t bound) const
{
    return (this->coefficients_.size() < bound ||
            std::all_of(this->coefficients_.begin() + bound,
                        this->coefficients_.end(),
                        [](const FieldT &el) { return el.is_zero(); }));
}

template<typename FieldT>
std::size_t polynomial<FieldT>::degree() const
{
    return (this->coefficients_.size() == 0 ? 0 : this->coefficients_.size()-1);
}

template<typename FieldT>
const FieldT& polynomial<FieldT>::operator[](const std::size_t index) const
{
    return this->coefficients_[index];
}

template<typename FieldT>
FieldT& polynomial<FieldT>::operator[](const std::size_t index)
{
    return this->coefficients_[index];
}

template<typename FieldT>
const std::vector<FieldT>& polynomial<FieldT>::coefficients() const
{
    return this->coefficients_;
}

template<typename FieldT>
bool polynomial<FieldT>::operator==(const polynomial<FieldT> &other) const
{
    return (this->coefficients_equivalent_with(other));
}

template<typename FieldT>
bool polynomial<FieldT>::operator!=(const polynomial<FieldT> &other) const
{
    return !(this->operator==(other));
}

template<typename FieldT>
polynomial<FieldT>& polynomial<FieldT>::operator+=(const polynomial<FieldT> &other)
{
    this->add_coefficients_of(other);
    return (*this);
}

template<typename FieldT>
polynomial<FieldT> polynomial<FieldT>::operator+(const polynomial<FieldT> &other) const
{
    polynomial<FieldT> result(*this);
    result += other;
    return result;
}

// returns polynomial specified by degree_bound coefficients.
// the returned polynomial is then of degree d - 1 polynomial.
template<typename FieldT>
polynomial<FieldT> polynomial<FieldT>::random_polynomial(const size_t degree_bound)
{
    /* Can't use bytewise random_vector<FieldT> because that will give invalid elements
        for libff prime fields. */
    std::vector<FieldT> random_coefficients;
    random_coefficients.reserve(degree_bound);
    for (size_t i = 0; i < degree_bound; ++i)
    {
        random_coefficients.emplace_back(FieldT::random_element());
    }

    return polynomial<FieldT>(std::move(random_coefficients));
}

} // namespace libiop
