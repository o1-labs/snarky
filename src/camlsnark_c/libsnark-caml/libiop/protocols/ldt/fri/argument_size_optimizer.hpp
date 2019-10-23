/**@file
 *****************************************************************************
  FRI localization parameter array optimizer for minimal argument size
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

/** Returns the vector of FRI localization parameters that is predicted to produce the smallest
 *  argument size for these parameters. This is calculated by brute forcing all options.
 *  The first localization parameter is fixed as 1, to mitigate verifier time.
 *
 *  TODO: Make this take in a make zk vector
 *  TODO: Add flag to not fix the first parameter
 *  TODO: Make this optimize for multiple FRI interactive repetitions
 */
template<typename FieldT>
std::vector<size_t> compute_argument_size_optimal_localization_parameters(
    std::vector<size_t> oracle_locality_vector,
    size_t codeword_dim,
    size_t query_repetitions,
    size_t max_tested_degree,
    size_t hash_size_in_bytes);

/** Returns the expected argument size from the optimizer.
 *  The argument size that is returned accounts for both IOP size and BCS size.
 *
 *  The locality vector is the vector of the number of oracles, by round,
 *  that are being low degree tested into FRI.
*/
template<typename FieldT>
size_t argument_size_predictor(
    std::vector<size_t> oracle_locality_vector,
    std::vector<size_t> fri_localization_vector,
    size_t codeword_dim,
    size_t query_repetitions,
    size_t max_tested_degree,
    size_t hash_size_in_bytes);

} // namespace libiop

#include "libiop/protocols/ldt/fri/argument_size_optimizer.tcc"

#endif // LIBIOP_PROTOCOLS_LDT_FRI_OPTIMIZER_HPP_
