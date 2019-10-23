/**@file
 *****************************************************************************
  FRI localization parameter array optimizer for minimal circuit size
 *****************************************************************************
 * @author     This file is part of libiop (see AUTHORS)
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_LDT_FRI_OPTIMIZER_HPP_
#define LIBIOP_PROTOCOLS_LDT_FRI_OPTIMIZER_HPP_

#include <vector>
#include <algorithm>

#include "libiop/algebra/fields/utils.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/protocols/ldt/fri/fri_aux.hpp"
#include "libiop/snark/common/bcs_common.hpp"

namespace libiop {

/** Returns the vector of FRI localization parameters that are predicted to produce the
 *  smallest circuit size for these parameters.
 *  This is calculated by brute forcing all options.
 *  The first localization parameter is fixed as 1, to mitigate verifier time.
 *
 *  TODO: Make this take in a make zk vector
 *  TODO: Don't fix the first parameter
 *  TODO: Make this optimize for multiple FRI interactive repetitions
 */
template<typename FieldT>
std::vector<size_t> compute_circuit_size_optimal_localization_parameters(
    std::vector<size_t> oracle_locality_vector,
    size_t codeword_dim,
    size_t query_repetitions,
    size_t max_tested_degree,
    size_t encoded_circuit_cost_per_query,
    hash_circuit_description<FieldT> hash_info);

/** Returns the expected circuit size from the optimizer.
 *
 *  The locality vector is the vector of the number of oracles, by round,
 *  that are being low degree tested with FRI.
*/
template<typename FieldT>
size_t circuit_size_predictor(
    std::vector<size_t> oracle_locality_vector,
    std::vector<size_t> fri_localization_vector,
    size_t codeword_dim,
    size_t num_queries,
    size_t max_tested_degree,
    size_t encoded_circuit_size_per_query,
    hash_circuit_description<FieldT> hash_info);

} // namespace libiop

#include "libiop/protocols/ldt/fri/circuit_size_optimizer.tcc"

#endif // LIBIOP_PROTOCOLS_LDT_FRI_OPTIMIZER_HPP_
