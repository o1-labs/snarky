/**@file
*****************************************************************************
Dummy encoded protocol + FRI LDT (for use in FRI SNARK only).
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_FRI_IOP_HPP_
#define LIBIOP_PROTOCOLS_FRI_IOP_HPP_

#include <cstddef>
#include <functional>
#include <memory>
#include <vector>

#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/dummy_protocol.hpp"
#include "libiop/protocols/ldt/fri/fri_ldt.hpp"
#include "libiop/protocols/ldt/ldt_reducer.hpp"

namespace libiop {

/** This is used solely for profiling FRI. */
struct FRI_iop_protocol_parameters {
    std::size_t codeword_domain_dim_;
    std::size_t RS_extra_dimensions_;
    std::size_t localization_parameter_;
    std::vector<std::size_t> localization_parameter_array_;
    std::size_t num_query_repetitions_;
    std::size_t num_interactive_repetitions_;

    std::size_t num_oracles_;

    field_type field_type_;
};

template<typename FieldT>
class FRI_iop_protocol {
protected:
    iop_protocol<FieldT> &IOP_;

    polynomial<FieldT> poly_;

    FRI_iop_protocol_parameters parameters_;

    std::shared_ptr<dummy_protocol<FieldT> > protocol_;
    std::shared_ptr<LDT_instance_reducer<FieldT, FRI_protocol<FieldT>> > LDT_;
public:
    /* Initialization and registration */
    FRI_iop_protocol(iop_protocol<FieldT> &IOP,
                     const std::vector<FieldT> evaluations,
                     const FRI_iop_protocol_parameters &parameters);

    void register_interactions();
    void register_queries();

    /* Proving */
    void produce_proof();

    /* Verification */
    bool verifier_predicate();
};


} // namespace libiop

#include "libiop/protocols/fri_iop.tcc"

#endif // LIBIOP_PROTOCOLS_FRI_IOP_HPP_
