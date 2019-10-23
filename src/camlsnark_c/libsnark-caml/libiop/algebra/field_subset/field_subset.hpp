/**@file
 *****************************************************************************
 Any field subsets which can be used as evaluation domains.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_FIELD_SUBSET_HPP_
#define LIBIOP_ALGEBRA_FIELD_SUBSET_HPP_

#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"

namespace libiop {

/** While solely affine subspace and multiplicative cosets for now,
 *  there could be more field subset types of interest. */
enum field_subset_type {
    affine_subspace_type = 1,
    multiplicative_coset_type = 2
};

static const char* field_subset_type_names[] = {"", "affine subspace", "multiplicative coset"};

template<typename FieldT>
class field_subset {
protected:
    std::shared_ptr<affine_subspace<FieldT>> subspace_;
    std::shared_ptr<multiplicative_coset<FieldT>> coset_;
    field_subset_type type_;
public:
    field_subset() = default;

    field_subset(const std::size_t num_elements);
    field_subset(const std::size_t num_elements, const FieldT coset_shift);

    field_subset(const affine_subspace<FieldT> subspace);
    field_subset(const multiplicative_coset<FieldT> coset);

    field_subset_type type() const;

    std::size_t dimension() const;
    std::size_t num_elements() const;

    std::vector<FieldT> all_elements() const;
    FieldT element_by_index(const std::size_t index) const;
    std::size_t reindex_by_subset(const std::size_t reindex_subset_dim, const std::size_t index) const;
    std::size_t coset_index(const std::size_t position, const std::size_t coset_size) const;
    std::size_t intra_coset_index(const std::size_t position, const std::size_t coset_size) const;
    size_t position_by_coset_indices(
        const size_t coset_index, const size_t intra_coset_index, const size_t coset_size) const;
    std::vector<size_t> all_positions_in_coset_i(
        const size_t coset_index, const size_t coset_size) const;
    std::vector<size_t> all_positions_with_intra_coset_index_i(
        const size_t intra_coset_index, const size_t coset_size) const;
    /* In the additive case, returns the subset which uses the first log_2(order) basis vectors */
    field_subset<FieldT> get_subset_of_order(const std::size_t order) const;

    bool element_in_subset(const FieldT x) const;
    FieldT element_outside_of_subset() const;

    affine_subspace<FieldT> subspace() const;
    multiplicative_coset<FieldT> coset() const;

    FieldT generator() const;

    const FieldT& offset() const;
    const FieldT shift() const;
    const std::vector<FieldT>& basis() const;

    bool operator==(const field_subset<FieldT> &other) const;
    bool operator!=(const field_subset<FieldT> &other) const;
protected:
    void construct_internal(const std::size_t num_elements,
                            const typename libiop::enable_if<is_multiplicative<FieldT>::value, FieldT>::type coset_shift);
    void construct_internal(const std::size_t num_elements,
                            const typename libiop::enable_if<is_additive<FieldT>::value, FieldT>::type coset_shift);
};

} // namespace libiop

#include "libiop/algebra/field_subset/field_subset.tcc"

#endif // LIBIOP_ALGEBRA_FIELD_SUBSET_HPP_
