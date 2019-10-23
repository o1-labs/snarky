/**@file
 *****************************************************************************
 BCS16 prover, for converting an IOP prover into a SNARK prover
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_SNARK_COMMON_BCS16_PROVER_HPP_
#define LIBIOP_SNARK_COMMON_BCS16_PROVER_HPP_

#include <set>

#include "libiop/common/profiling.hpp"
#include "libiop/snark/common/bcs_common.hpp"

namespace libiop {

template<typename FieldT>
class bcs_prover : public bcs_protocol<FieldT> {
protected:
    std::size_t processed_MTs_ = 0;
    bool is_preprocessing_ = false;
    size_t num_indexed_MTs_ = 0;
    std::vector<std::vector<FieldT>> indexed_prover_messages_;
    void remove_index_info_from_transcript(bcs_transformation_transcript<FieldT> &transcript);
public:
    bcs_prover(const bcs_transformation_parameters<FieldT> &parameters);
    /* Mutates index */
    bcs_prover(const bcs_transformation_parameters<FieldT> &parameters,
               bcs_prover_index<FieldT> &index);

    /** The overloaded method for signal_prover_round_done performs
     *  hashing of all oracles and prover messages submitted in the
     *  current round.     */
    virtual void signal_prover_round_done();
    /** If its a preprocessing SNARK, preprocessed oracles will be submitted after
     *  queries are registered. */
    virtual void seal_query_registrations();
    virtual void signal_index_submissions_done();

    /** We also overload verifier's randomness extraction functions
     * below to use the extracted values.    */
    virtual std::vector<FieldT> obtain_verifier_random_message(const verifier_random_message_handle &random_message);

    virtual FieldT obtain_query_response(const query_handle &query);
    bcs_transformation_transcript<FieldT> get_transcript();

    std::size_t MT_size() const;
    std::size_t state_size() const;

    void describe_sizes() const;
};

} // namespace libiop

#include "libiop/snark/common/bcs_prover.tcc"

#endif // LIBIOP_SNARK_COMMON_BCS16_PROVER_HPP_
