/**@file
*****************************************************************************
Virtual oracle for linear combinations of rational oracles
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_COMMON_RATIONAL_LINEAR_COMBINATION_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_COMMON_RATIONAL_LINEAR_COMBINATION_HPP_

#include <cstring>
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

#include "libiop/algebra/lagrange.hpp"
#include "libiop/iop/iop.hpp"

namespace libiop {

template<typename FieldT>
class combined_denominator : public virtual_oracle<FieldT> {
protected:
    std::size_t num_rationals_;
public:
    combined_denominator(const std::size_t num_rationals);
    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const;
    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const;
};

template<typename FieldT>
class combined_numerator : public virtual_oracle<FieldT> {
protected:
    std::vector<FieldT> coefficients_;
    std::size_t num_rationals_;
public:
    combined_numerator(const std::size_t num_rationals);
    void set_coefficients(const std::vector<FieldT>& coefficients);
    /** Expects oracles to come in the form N_1, N_2, ..., D_1, D_2, ...
     *  Suppose there are three rationals. This would return
     *      (r_1 * N_1 * D_2 * D_3) + (r_2 * N_2 * D_1 * D_3) + (r_3 * N_3 * D_1 * D_2)
     */
    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const;
    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const;
};

/** Takes a linear combination of rational oracles.
 *  A rational oracle is represented as a pair of oracles N, D
 *  It expects the oracles in the form (N_1, N_2, ..., D_1, D_2, ....)
 */
template<typename FieldT>
class rational_linear_combination {
protected:
    iop_protocol<FieldT> &IOP_;
    std::vector<FieldT> coefficients_;
    std::size_t num_rationals_;
    std::shared_ptr<combined_numerator<FieldT>> numerator_;
    std::shared_ptr<combined_denominator<FieldT>> denominator_;

    virtual_oracle_handle combined_numerator_handle_;
    virtual_oracle_handle combined_denominator_handle_;
public:
    rational_linear_combination(iop_protocol<FieldT> &IOP,
                                const size_t num_rationals_,
                                const std::vector<oracle_handle_ptr> numerator_handles,
                                const std::vector<oracle_handle_ptr> denominator_handles);
    void set_coefficients(const std::vector<FieldT>& coefficients);

    std::vector<FieldT> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &numerator_evals,
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &denominator_evals) const;

    oracle_handle_ptr get_numerator_handle() const;
    oracle_handle_ptr get_denominator_handle() const;
};

} // libiop

#include "libiop/protocols/encoded/common/rational_linear_combination.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_COMMON_RATIONAL_LINEAR_COMBINATION_HPP_
