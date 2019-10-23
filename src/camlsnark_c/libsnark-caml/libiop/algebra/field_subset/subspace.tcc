#include <stdexcept>

#include "libiop/algebra/utils.hpp"

namespace libiop {

/* linear_subspace<FieldT> */

template<typename FieldT>
linear_subspace<FieldT>::linear_subspace(const std::vector<FieldT> &basis) :
    basis_(basis)
{
    this->is_standard_basis_ = this->is_standard_basis();
}

template<typename FieldT>
linear_subspace<FieldT>::linear_subspace(std::vector<FieldT> &&basis) :
    basis_(std::move(basis))
{
    this->is_standard_basis_ = this->is_standard_basis();
}

template<typename FieldT>
std::size_t linear_subspace<FieldT>::dimension() const
{
    return this->basis_.size();
}

template<typename FieldT>
std::size_t linear_subspace<FieldT>::num_elements() const
{
    return (1ull << this->basis_.size());
}

template<typename FieldT>
const std::vector<FieldT>& linear_subspace<FieldT>::basis() const
{
    return this->basis_;
}

template<typename FieldT>
const FieldT& linear_subspace<FieldT>::basis_element(const std::size_t i) const
{
    return this->basis_[i];
}

template<typename FieldT>
std::vector<FieldT> linear_subspace<FieldT>::all_elements() const
{
    return all_subset_sums<FieldT>(this->basis_);
}

template<typename FieldT>
FieldT linear_subspace<FieldT>::element_by_index(const std::size_t index) const
{
    if (index >= this->num_elements())
    {
        throw std::invalid_argument("element index out of bounds");
    }

    FieldT result = FieldT(0);
    for (std::size_t i = 0; i < this->basis_.size(); ++i)
    {
        if (index & (1ull<<i))
        {
            result += this->basis_[i];
        }
    }

    return result;
}

template<typename FieldT>
std::size_t linear_subspace<FieldT>::coset_index(const std::size_t position, const std::size_t coset_size) const
{
    return position / coset_size;
}

/** Assumes that the coset uses the first few basis vectors as this subspace. */
template<typename FieldT>
std::size_t linear_subspace<FieldT>::intra_coset_index(const std::size_t position, const std::size_t coset_size) const
{
    return position % coset_size;
}

template<typename FieldT>
std::size_t linear_subspace<FieldT>::position_by_coset_indices(
        const size_t coset_index, const size_t intra_coset_index, const size_t coset_size) const
{
    return coset_index * coset_size + intra_coset_index;
}

template<typename FieldT>
linear_subspace<FieldT> linear_subspace<FieldT>::standard_basis(const std::size_t dimension)
{
    /** For binary fields, log_of_field_size = extension degree */
    assert(dimension <= log_of_field_size_helper<FieldT>(FieldT::zero()));

    std::vector<FieldT> basis_elements;
    basis_elements.reserve(dimension);

    for (std::size_t i = 0; i < dimension; ++i)
    {
        basis_elements.emplace_back(FieldT(1ull<<i));
    }

    return linear_subspace<FieldT>(std::move(basis_elements));
}

template<typename FieldT>
linear_subspace<FieldT> linear_subspace<FieldT>::random_linear_subspace(const std::size_t dimension)
{
    const std::vector<FieldT> basis_elements = random_FieldT_vector<FieldT>(dimension);
    return linear_subspace<FieldT>(std::move(basis_elements));
}

template<typename FieldT>
bool linear_subspace<FieldT>::operator==(const linear_subspace<FieldT> &other) const
{
    return this->basis_ == other->basis_;
}

template<typename FieldT>
bool linear_subspace<FieldT>::operator!=(const linear_subspace<FieldT> &other) const
{
    return !(this->operator==(other));
}

template<typename FieldT>
affine_subspace<FieldT>::affine_subspace(const std::vector<FieldT> &basis,
                                         const FieldT &offset) :
    linear_subspace<FieldT>(basis), offset_(offset)
{
}

template<typename FieldT>
affine_subspace<FieldT>::affine_subspace(
    const linear_subspace<FieldT> &base_space,
    const FieldT &offset) :
    linear_subspace<FieldT>(base_space),
    offset_(offset)
{
}

template<typename FieldT>
affine_subspace<FieldT>::affine_subspace(
    linear_subspace<FieldT> &&base_space,
    const FieldT &offset) :
    linear_subspace<FieldT>(std::move(base_space)),
    offset_(offset)
{
}

template<typename FieldT>
const FieldT& affine_subspace<FieldT>::offset() const
{
    return this->offset_;
}

template<typename FieldT>
std::vector<FieldT> affine_subspace<FieldT>::all_elements() const
{
    return all_subset_sums<FieldT>(this->basis_, this->offset_);
}

template<typename FieldT>
FieldT affine_subspace<FieldT>::element_by_index(const std::size_t index) const
{
    return (this->offset_ + (linear_subspace<FieldT>::element_by_index(index)));
}


template<typename FieldT>
bool internal_element_in_subset(const typename libiop::enable_if<is_multiplicative<FieldT>::value, FieldT>::type x,
    FieldT offset, size_t dimension)
{
    throw std::invalid_argument("subspace.element_in_subset() is only supported for binary fields");
}

template<typename FieldT>
bool internal_element_in_subset(const typename libiop::enable_if<is_additive<FieldT>::value, FieldT>::type x,
    FieldT offset, size_t dimension)
{
    /** TODO: Implement this case */
    if (dimension > 64)
    {
        throw std::invalid_argument(
            "subspace.element_in_subset() is currently unimplimented for basis of dimension greater than 64");
    }
    /** We first remove the offset, and then test if an element is in the standard basis.
     *  It is in the standard basis if for all i >= basis.dimension(), the coefficient of x^i is 0.
     *  Due to the representation of field elements,
     *  this corresponds to all but the first basis.dimension() bits being 0.
     *  (using little endian ordering)
    */
    const std::vector<uint64_t> words = (x + offset).as_words();
    /* Check that all but the least significant 64 bits are 0 */
    for (size_t i = 1; i < words.size(); i++)
    {
        if (words[i] != 0)
        {
            return false;
        }
    }
    /* Check that in the least significant 64 bits, only the first basis.dimension() are non-zero */
    return words[0] < (1ull << dimension);
}

template<typename FieldT>
bool affine_subspace<FieldT>::element_in_subset(const FieldT x) const
{
    if (this->is_standard_basis_)
    {
        return internal_element_in_subset(x, this->offset_, this->dimension());
    }
    throw std::invalid_argument("subspace.element_in_subset() is only supported for standard basis");
}

template<typename FieldT>
FieldT affine_subspace<FieldT>::element_outside_of_subset() const
{
    if (this->is_standard_basis_)
    {
        return this->offset() + FieldT(1ull << this->dimension());
    }
    throw std::invalid_argument("subspace.element_outside_of_subset() is only supported for standard basis");
}

template<typename FieldT>
bool linear_subspace<FieldT>::is_standard_basis() const
{
    for (size_t i = 0; i < this->basis_.size(); ++i)
    {
        if (this->basis_[i] != FieldT(1ull<<i))
        {
            return false;
        }
    }
    return true;
}

template<typename FieldT>
linear_subspace<FieldT> standard_basis(const std::size_t dimension)
{
    std::vector<FieldT> basis;
    basis.reserve(dimension);
    for (size_t i = 0; i < dimension; ++i)
    {
        basis.emplace_back(FieldT(1ull<<i));
    }

    return linear_subspace<FieldT>(std::move(basis));
}

template<typename FieldT>
affine_subspace<FieldT> affine_subspace<FieldT>::shifted_standard_basis(
    const std::size_t dimension,
    const FieldT& offset)
{
    const linear_subspace<FieldT> basis =
        linear_subspace<FieldT>::standard_basis(dimension);
    return affine_subspace<FieldT>(std::move(basis), offset);
}

template<typename FieldT>
affine_subspace<FieldT> affine_subspace<FieldT>::random_affine_subspace(const std::size_t dimension)
{
    const linear_subspace<FieldT> basis =
        linear_subspace<FieldT>::standard_basis(dimension);
    const FieldT offset = FieldT::random_element();
    return affine_subspace<FieldT>(std::move(basis), offset);
}

template<typename FieldT>
bool affine_subspace<FieldT>::operator==(const affine_subspace<FieldT> &other) const
{
    return linear_subspace<FieldT>::operator==(other) && this->offset_ == other->offset_;
}

} // namespace libiop
