namespace libiop {

/* TODO: Final interpolation costs */
const size_t FRI_final_interpolation_cost_per_elem = 1;
const size_t FRI_final_interpolation_cost_additive_constant = 1;
/** The below constants are for a single query repetition of a FRI round
 *  parameterized by the localization parameter n. (coset_size = 2^n)
 *  FRI_round_cost_additive_constant is only paid for once across all query repetitions
 *
 *  The cost of a single FRI round in the multiplicative setting is as follows:
 *  Once for all cosets:
 *  Raise x^|coset| - takes n constraints
 *  Per query repetition:
 *  * Lagrange interpolate f_i over the coset - (2*2^n + n + 4 constraints)
 *  * Check equality with the next codeword's evaluation - (1 constraint)
 *  The strategy for lagrange interpolating f_i over the coset is:
 *  The ith lagrange coefficient is
 *      l_i := (Z_coset(x) * |coset|^-1 * g^i * h^{-coset + 1}) / (x - hg^i)
 *  and the interpolation is
 *      \sum_{j in coset} l_j * f_i(j)
 *  To save multiplications, we multiply Z_coset(x) only at the end, so let
 *      l'_j = l_i / (Z_coset(x) = |coset|^-1 * g^i * h^{-coset + 1}) / (x - hg^i),
 *  and we compute the interpolation as
 *      Z_coset(x) \sum_{j in coset} l'_j * f_i(j)
 *  We then convert l'_j to the equivalent expression:
 *      WIP, see excel spreadsheet for now
*/
const size_t FRI_round_cost_per_query_coset_size_multiplier = 2;
const size_t FRI_round_cost_per_query_localization_param_multiplier = 2;
const size_t FRI_round_cost_per_query_additive_constant = 4;
const size_t FRI_round_cost_additive_constant = 4;

size_t FRI_final_interpolation_degree(
    const size_t max_tested_degree,
    const std::vector<size_t> fri_localization_vector)
{
    /* end_degree (will be) the degree of the polynomial we interpolate at the end */
    size_t end_degree = max_tested_degree;
    for (size_t i = 0; i < fri_localization_vector.size(); ++i)
    {
        end_degree = end_degree / (1ull << fri_localization_vector[i]);
    }
    return end_degree;
}

size_t FRI_final_interpolation_circuit_cost(
    std::vector<size_t> fri_localization_vector,
    size_t max_tested_degree, size_t num_queries)
{
    const size_t final_interpolation_size =
        FRI_final_interpolation_degree(max_tested_degree, fri_localization_vector);
    const size_t IFFT_cost = (size_t) -1;
    const size_t lagrange_interpolation_cost =
        (FRI_final_interpolation_cost_per_elem * final_interpolation_size
        + FRI_final_interpolation_cost_additive_constant) * num_queries;
    return std::min({IFFT_cost, lagrange_interpolation_cost});
}

size_t FRI_round_by_round_costs(
    std::vector<size_t> fri_localization_vector, size_t num_queries)
{
    size_t total_cost_per_query = 0;
    for (size_t i = 0; i < fri_localization_vector.size(); i++)
    {
        const size_t coset_size = 1ull << fri_localization_vector[i];
        total_cost_per_query += coset_size * FRI_round_cost_per_query_coset_size_multiplier
            + fri_localization_vector[i] * FRI_round_cost_per_query_localization_param_multiplier
            + FRI_round_cost_per_query_additive_constant;
    }
    return (total_cost_per_query * num_queries) +
        FRI_round_cost_additive_constant * fri_localization_vector.size();
}

template<typename FieldT>
size_t leaf_hash_circuit_size(
    std::vector<size_t> oracle_locality_vector,
    std::vector<size_t> fri_localization_vector,
    size_t num_queries,
    hash_circuit_description<FieldT> hash_info)
{
    size_t total_per_query = 0;
    /* Handle the input oracles */
    for (size_t i = 0; i < oracle_locality_vector.size(); ++i)
    {
        size_t leaf_size = oracle_locality_vector[i] * (1ull << fri_localization_vector[0]);
        total_per_query += hash_info.arity_m_hash_complexity(leaf_size);
    }

    for (size_t i = 1; i < fri_localization_vector.size(); ++i)
    {
        size_t leaf_size = 1ull << fri_localization_vector[i];
        total_per_query += hash_info.arity_m_hash_complexity(leaf_size);
    }
    /* Assumes no query point collisions */
    return total_per_query * num_queries;
}

template<typename FieldT>
size_t internal_hash_circuit_size(
    std::vector<size_t> oracle_locality_vector,
    std::vector<size_t> fri_localization_vector,
    size_t num_queries,
    size_t codeword_dim,
    hash_circuit_description<FieldT> hash_info)
{
    /** In our circuit, we handle pruning by hashing the top c layers only once,
     *  and hashing all lower layers in every query regardless of collisions.
     *  The top c layers is called the "upper cap"
     */
    size_t total_two_to_one_hashes = 0;
    size_t upper_cap_total_circuit_cost = 0;
    /* TODO: Be more accurate about upper cap size */
    const size_t upper_cap_depth = libiop::log2(num_queries) - 1;
    const size_t upper_cap_logic_cost = 0;
    const size_t logic_cost_per_two_to_one_hash = 2;

    /* Handle input oracles */
    const size_t input_oracle_MT_depth = codeword_dim - fri_localization_vector[0];
    assert(upper_cap_depth <= input_oracle_MT_depth);
    const size_t input_oracle_hashes_per_tree = (input_oracle_MT_depth - upper_cap_depth)
        * num_queries;
    const size_t input_oracle_hashes = oracle_locality_vector.size() *
        input_oracle_hashes_per_tree;
    total_two_to_one_hashes += input_oracle_hashes;
    upper_cap_total_circuit_cost += oracle_locality_vector.size() *
        (hash_info.arity_m_hash_complexity(1ull << upper_cap_depth) + upper_cap_logic_cost);

    size_t current_codeword_dim = input_oracle_MT_depth;
    for (size_t i = 1; i < fri_localization_vector.size(); ++i)
    {
        const size_t MT_depth = current_codeword_dim - fri_localization_vector[i];
        const size_t cur_cap_depth = std::min(upper_cap_depth, MT_depth);
        const size_t hashes_per_query = (MT_depth - cur_cap_depth);
        total_two_to_one_hashes += hashes_per_query * num_queries;

        upper_cap_total_circuit_cost +=
            hash_info.arity_m_hash_complexity(1ull << cur_cap_depth) + upper_cap_logic_cost;

        current_codeword_dim = MT_depth;
    }

    const size_t two_to_one_hash_costs = total_two_to_one_hashes *
        (hash_info.arity_m_hash_complexity(2) + logic_cost_per_two_to_one_hash);

    return two_to_one_hash_costs + upper_cap_total_circuit_cost;
}

template<typename FieldT>
size_t circuit_size_predictor(
    std::vector<size_t> oracle_locality_vector,
    std::vector<size_t> fri_localization_vector,
    size_t codeword_dim,
    size_t num_queries,
    size_t max_tested_degree,
    size_t encoded_circuit_size_per_query,
    hash_circuit_description<FieldT> hash_info)
{
    const size_t encoded_circuit_cost = encoded_circuit_size_per_query *
        (1ull << fri_localization_vector[0]) * num_queries;
    const size_t FRI_final_interpolation_cost =
        FRI_final_interpolation_circuit_cost(fri_localization_vector,
            max_tested_degree, num_queries);

    /* TODO: Better name, basically all the IOP cost except final interpolation */
    size_t FRI_cost_across_rounds =
        FRI_round_by_round_costs(fri_localization_vector, num_queries);
    const size_t IOP_cost = encoded_circuit_cost +
        FRI_final_interpolation_cost + FRI_cost_across_rounds;

    /* TODO: Abstract arity */
    const size_t total_membership_proof_hash_cost = internal_hash_circuit_size<FieldT>(
        oracle_locality_vector, fri_localization_vector,
        num_queries, codeword_dim, hash_info);
    const size_t total_leaf_hash_cost = leaf_hash_circuit_size<FieldT>(
        oracle_locality_vector, fri_localization_vector, num_queries, hash_info);
    /* We don't consider hash chain cost of the input oracles, or getting queries */
    /* Solve for sponge size and state size in other script */
    const size_t FRI_hash_chain_cost = hash_info.hash_chain_complexity(2, 1)
        * fri_localization_vector.size();
    const size_t BCS_cost = total_membership_proof_hash_cost
        + total_leaf_hash_cost + FRI_hash_chain_cost;

    // printf("predicted IOP size %lu\n", IOP_size);
    // printf("predicted BCS size %lu\n", BCS_size);
    return IOP_cost + BCS_cost;
}

template<typename FieldT>
std::vector<size_t> compute_circuit_size_optimal_localization_parameters(
    std::vector<size_t> oracle_locality_vector,
    size_t codeword_dim,
    size_t num_queries,
    size_t max_tested_degree,
    size_t encoded_circuit_cost_per_query,
    hash_circuit_description<FieldT> hash_info)
{
    /* Set this to improve efficiency of brute force */
    const size_t minimum_final_constant_dim = 2;
    /* TODO: Consider if the -1 is necessary, I think it is for max_tested_degree */
    size_t num_dimensions_to_reduce = libiop::log2(max_tested_degree) - 1 - minimum_final_constant_dim;
    std::vector<std::vector<size_t>> options =
        all_localization_vectors(num_dimensions_to_reduce);

    size_t min = (size_t)-1;
    std::vector<size_t> best = {};
    for (size_t i = 0; i < options.size(); ++i)
    {
        size_t current = circuit_size_predictor<FieldT>(
                            oracle_locality_vector, options[i],
                            codeword_dim, num_queries, max_tested_degree,
                            encoded_circuit_cost_per_query, hash_info);
        if (current < min)
        {
            min = current;
            best = options[i];
        }
    }

    return best;
}

}