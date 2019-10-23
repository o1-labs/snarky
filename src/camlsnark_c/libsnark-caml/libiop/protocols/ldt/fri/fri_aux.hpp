/**@file
 *****************************************************************************
 Implementations of various methods used within FRI
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_LDT_FRI_FRI_AUX_HPP_
#define LIBIOP_PROTOCOLS_LDT_FRI_FRI_AUX_HPP_

#include <vector>

#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/protocols/ldt/fri/localizer_polynomial.hpp"
#include "libiop/iop/iop.hpp"

namespace libiop {

template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> evaluate_next_f_i_over_entire_domain(
    const std::shared_ptr<std::vector<FieldT>> &f_i_evals,
    const field_subset<FieldT> &f_i_domain,
    const size_t coset_size,
    const FieldT x_i);

/** TODO: We should make a "lagrange cache" per reduction */
template<typename FieldT>
FieldT evaluate_next_f_i_at_coset(
    const std::vector<FieldT> &f_i_evals_over_coset,
    const field_subset<FieldT> &unshifted_coset,
    const FieldT offset,
    const localizer_polynomial<FieldT> &unshifted_vp,
    const FieldT x_i);

template<typename FieldT>
FieldT additive_evaluate_next_f_i_at_coset(
    const std::vector<FieldT> &f_i_evals_over_coset,
    const field_subset<FieldT> &unshifted_coset,
    const FieldT offset,
    const localizer_polynomial<FieldT> &unshifted_vp,
    const FieldT x_i);

template<typename FieldT>
FieldT multiplicative_evaluate_next_f_i_at_coset(
    const std::vector<FieldT> &f_i_evals_over_coset,
    const FieldT coset_g,
    const FieldT coset_h,
    const FieldT x_i);

template<typename FieldT>
std::vector<query_position_handle> calculate_next_coset_query_positions(
    iop_protocol<FieldT> &IOP,
    const query_position_handle &non_localized_query_handle,
    const field_subset<FieldT> &non_localized_domain,
    const field_subset<FieldT> &localized_domain,
    const size_t prev_localization_parameter,
    const size_t cur_localization_parameter);

/* return all partitions of this number (that is, all possible FRI localization parameter vectors
   for log2(tested degree)) */
std::vector<std::vector<size_t>> all_localization_vectors(size_t dimension_to_reduce);

} // namespace libiop

#include "libiop/protocols/ldt/fri/fri_aux.tcc"

#endif // LIBIOP_PROTOCOLS_LDT_FRI_FRI_AUX_HPP_
