/**@file
*****************************************************************************
R1CS virtual rowcheck oracle
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_ENCODED_COMMON_ROWCHECK_HPP_
#define LIBIOP_PROTOCOLS_ENCODED_COMMON_ROWCHECK_HPP_

#include <cstring>
#include <cstddef>
#include <memory>
#include <vector>

#include "libiop/iop/iop.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"

namespace libiop {

template<typename FieldT>
class rowcheck_ABC_virtual_oracle : public virtual_oracle<FieldT> {
protected:
    field_subset<FieldT> codeword_domain_;
    field_subset<FieldT> constraint_domain_;
    vanishing_polynomial<FieldT> Z_;
public:
    rowcheck_ABC_virtual_oracle(const field_subset<FieldT> &codeword_domain,
                                const field_subset<FieldT> &constraint_domain);

    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const;
    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const;
};

} // libiop

#include "libiop/protocols/encoded/common/rowcheck.tcc"

#endif // LIBIOP_PROTOCOLS_ENCODED_COMMON_ROWCHECK_HPP_
