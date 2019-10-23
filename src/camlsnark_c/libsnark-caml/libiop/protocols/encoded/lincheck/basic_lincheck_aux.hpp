/**@file
*****************************************************************************
Basic R1CS multi lincheck virtual oracle
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_LINCHECK_BASIC_LINCHECK_AUX_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_LINCHECK_BASIC_LINCHECK_AUX_HPP_

#include <cstring>
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

#include "libiop/relations/sparse_matrix.hpp"
#include "libiop/algebra/lagrange.hpp"
#include "libiop/iop/iop.hpp"


namespace libiop {

template<typename FieldT>
class multi_lincheck_virtual_oracle : public virtual_oracle<FieldT> {
protected:
    const field_subset<FieldT> codeword_domain_;
    const field_subset<FieldT> constraint_domain_;
    const field_subset<FieldT> variable_domain_;
    const field_subset<FieldT> summation_domain_;
    const std::size_t input_variable_dim_;
    const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> matrices_;

    std::vector<FieldT> r_Mz_;
    polynomial<FieldT> p_alpha_ABC_;
    polynomial<FieldT> p_alpha_prime_;

    /** The implementation uses a quasi-linear solution which works much faster than the linear solution
     *  for the multiplicative case, and works ~10-20% faster for the additive case
     *  with sufficiently large fields at the currently benchmarked sizes.
     *  The multiplicative speedup should hold for ~all reasonably sized summation domains.
     *  The additive speedup may not hold for sufficiently large summation domains.
     *
     *  The linear solution mentioned is using lagrange interpolation.
     *  Lagrange interpolation makess sense for very low security parameters / very small numbers of queries.
     *  It will improve speed for small fields with many lincheck repetitiions (such as gf64),
     *  and it may make sense for large additive fields with large summation domains. (Benchmark this for your setting)
     *
     *  As we improve the additive FFT, the speedup of setting this flag to false should further increase.
     *
     *  The flag cannot currently be set to true with everything working as-is.
     *  In the code, there are code blocks with if(use_lagrange) that describe what should be done
     *  in the lagrange case, however some of them require minor refactors to the interface.
    */
    const bool use_lagrange_ = false;
    std::vector<FieldT> alpha_powers_;
    std::vector<FieldT> p_alpha_ABC_evals_;
    std::shared_ptr<lagrange_cache<FieldT> > lagrange_coefficients_cache_;
public:
    multi_lincheck_virtual_oracle(
        const field_subset<FieldT> &codeword_domain,
        const field_subset<FieldT> &constraint_domain,
        const field_subset<FieldT> &variable_domain,
        const field_subset<FieldT> &summation_domain,
        const std::size_t input_variable_dim,
        const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> &matrices);

    void set_challenge(const FieldT &alpha, const std::vector<FieldT> r_Mz);

    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const;
    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const;
};

} // libiop

#include "libiop/protocols/encoded/lincheck/basic_lincheck_aux.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_LINCHECK_BASIC_LINCHECK_AUX_HPP_
