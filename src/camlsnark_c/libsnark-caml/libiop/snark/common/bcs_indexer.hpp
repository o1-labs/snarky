/**@file
 *****************************************************************************
 BCS16 transformation indexer, for generating a proving index and verification index
 from a holographic IOP indexer.
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_SNARK_COMMON_BCS16_INDEXER_HPP_
#define LIBIOP_SNARK_COMMON_BCS16_INDEXER_HPP_

#include <set>

#include "libiop/common/profiling.hpp"
#include "libiop/snark/common/bcs_common.hpp"

namespace libiop {

template<typename FieldT>
class bcs_indexer : public bcs_protocol<FieldT> {
protected:
    std::size_t MTs_processed_ = 0;
    size_t prover_messages_indexed = 0;
    std::vector<std::vector<FieldT>> indexed_oracles_;

    bool get_prover_index_has_been_called_ = false;
public:
    bcs_indexer(const bcs_transformation_parameters<FieldT> &parameters);

    /* Produces the merkle tree for the index oracles */
    virtual void signal_index_submissions_done();

    /** Throws error - The indexer should not be calling these */
    virtual void signal_prover_round_done();
    virtual std::vector<FieldT> obtain_verifier_random_message(const verifier_random_message_handle &random_message);
    virtual FieldT obtain_query_response(const query_handle &query);

    bcs_prover_index<FieldT> get_bcs_prover_index();
    bcs_verifier_index<FieldT> get_verifier_index();
};

} // namespace libiop

#include "libiop/snark/common/bcs_indexer.tcc"

#endif // LIBIOP_SNARK_COMMON_BCS16_INDEXER_HPP_
