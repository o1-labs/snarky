/**@file
*****************************************************************************
virtual oracles for holographic lincheck
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_LINCHECK_HOLOGRAPHIC_LINCHECK_AUX_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_LINCHECK_HOLOGRAPHIC_LINCHECK_AUX_HPP_

#include <cstring>
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

#include "libiop/algebra/polynomials/lagrange_polynomial.hpp"
#include "libiop/algebra/polynomials/bivariate_lagrange_polynomial.hpp"
#include "libiop/algebra/lagrange.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/lincheck/common.hpp"

namespace libiop {

template<typename FieldT>
class holographic_multi_lincheck_virtual_oracle : public virtual_oracle<FieldT> {
protected:
    const field_subset<FieldT> codeword_domain_;
    const field_subset<FieldT> summation_domain_;
    const std::size_t input_variable_dim_;
    const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> matrices_;

    std::vector<FieldT> r_Mz_;
    lagrange_polynomial<FieldT> p_alpha_prime_;

public:
    holographic_multi_lincheck_virtual_oracle(
        const field_subset<FieldT> &codeword_domain,
        const field_subset<FieldT> &summation_domain,
        const std::size_t input_variable_dim,
        const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> &matrices);

    // TODO: Make this take in p_alpha
    void set_challenge(const FieldT &alpha, const std::vector<FieldT> r_Mz);

    FieldT eval_at_out_of_domain_point(const std::vector<FieldT> &constituent_oracle_evaluations) const;

    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const;
    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const;
};

template<typename FieldT>
class single_matrix_denominator : public virtual_oracle<FieldT> {
protected:
    const field_subset<FieldT> codeword_domain_;
    const field_subset<FieldT> summation_domain_;
    const std::size_t input_variable_dim_;

    FieldT row_query_point_;
    FieldT column_query_point_;
public:
    single_matrix_denominator(
        const field_subset<FieldT> &codeword_domain,
        const field_subset<FieldT> &summation_domain,
        const std::size_t input_variable_dim);

    void set_challenge(const FieldT &row_query_point,
                       const FieldT &column_query_point);

    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const;
    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const;
};

} // libiop

#include "libiop/protocols/encoded/lincheck/holographic_lincheck_aux.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_LINCHECK_HOLOGRAPHIC_LINCHECK_AUX_HPP_
