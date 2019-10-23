/**@file
 *****************************************************************************
 Classes for F_2-linear subspaces.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_ALGEBRA_SUBSPACES_HPP_
#define LIBIOP_ALGEBRA_SUBSPACES_HPP_

#include <cstddef>
#include <vector>
#include "libiop/algebra/fields/utils.hpp"

namespace libiop {

template<typename FieldT>
class linear_subspace {
protected:
    std::vector<FieldT> basis_;
    bool is_standard_basis_;
public:
    linear_subspace() = default;
    linear_subspace(const std::vector<FieldT> &basis);
    linear_subspace(std::vector<FieldT> &&basis);

    std::size_t dimension() const;
    std::size_t num_elements() const;

    const std::vector<FieldT>& basis() const;
    const FieldT& basis_element(const std::size_t i) const;

    std::vector<FieldT> all_elements() const;
    FieldT element_by_index(const std::size_t index) const;
    std::size_t coset_index(const std::size_t position, const std::size_t coset_size) const;
    std::size_t intra_coset_index(const std::size_t position, const std::size_t coset_size) const;
    size_t position_by_coset_indices(
        const size_t coset_index, const size_t intra_coset_index, const size_t coset_size) const;
    // TODO: add a check for linear independence

    bool is_standard_basis() const;
    static linear_subspace<FieldT> standard_basis(const std::size_t dimension);
    // TODO: check if the elements are actually linearly independent
    static linear_subspace<FieldT> random_linear_subspace(const std::size_t dimension);

    bool operator==(const linear_subspace<FieldT> &other) const;
    bool operator!=(const linear_subspace<FieldT> &other) const;
};

template<typename FieldT>
class affine_subspace : public linear_subspace<FieldT> {
protected:
    FieldT offset_;

public:
    affine_subspace() = default;
    affine_subspace(const std::vector<FieldT> &basis, const FieldT &offset = FieldT(0));
    affine_subspace(const linear_subspace<FieldT> &base_space, const FieldT &offset = FieldT(0));
    affine_subspace(linear_subspace<FieldT> &&base_space, const FieldT &offset = FieldT(0));

    const FieldT& offset() const;

    std::vector<FieldT> all_elements() const;
    FieldT element_by_index(const std::size_t index) const;

    bool element_in_subset(const FieldT x) const;
    FieldT element_outside_of_subset() const;

    static affine_subspace<FieldT> shifted_standard_basis(
        const std::size_t dimension,
        const FieldT& offset);
    static affine_subspace<FieldT> random_affine_subspace(const std::size_t dimension);

    bool operator==(const affine_subspace<FieldT> &other) const;
};

} // namespace libiop

#include "libiop/algebra/field_subset/subspace.tcc"

#endif // LIBIOP_ALGEBRA_SUBSPACES_HPP_
