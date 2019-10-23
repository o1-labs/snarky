namespace libiop {

/* helper functions for estimating argument size */

/* return the expected number of hashes needed in the membership proof for a tree */
size_t num_hashes_in_a_membership_proof(size_t num_queries, size_t depth)
{
    /** We wish to know the expected number of hashes the prover must provide to the verifier for q randomly chosen leafs. (q's can collide)
     *  We consider this layer by layer. */
    float sum = 0.0;
    for (size_t d = 1; d <= depth; ++d)
    {
        /** Probability that the first element of this layer
         *  must be given is the probability that at least one
         *  query goes to its neighbor, and no queries go to it.
         *  With q queries, this is bounded by:
         *  over-estimate:  q * ((width - 1) / width)^{q - 1} * (1 / width)
         *  under-estimate: q * ((width - 2) / width)^{q - 1} * (1 / width)
         *
         *  The over-estimate comes from considering 1 query to the neighbor,
         *  and q-1 queries anywhere but the first element.
         *  The under-estimate comes from 1 query to the neighbour,
         *  and q-1 queries anywhere but the first two positions.
         *
         *  By linearity of expectation value,
         *  the number of elements given in this row is bounded by:
         *  over-estimate:  q * ((width - 1) / width)^{q - 1}
         *  under-estimate: q * ((width - 2) / width)^{q - 1}
         *  Due to lack of an explicit formula, we take the over-estimate of the cost.
         *  This over-estimate is always less than the cost without pruning.
         *  Empirically, this over-estimate seems to be within 5% of the actual numbers
         */
        float width = 1ull << d;
        float estimated_num_hashes = num_queries * pow((width - 1) / width, num_queries - 1);
        sum += estimated_num_hashes;
    }

    return ((size_t) std::round(sum));
}

size_t num_hashes_in_all_membership_proofs(
    std::vector<size_t> oracle_locality_vector,
    std::vector<size_t> fri_localization_vector,
    size_t num_queries,
    size_t codeword_dim)
{
    size_t total_hashes = 0;
    const size_t input_oracle_MT_depth = codeword_dim - fri_localization_vector[0];
    const size_t input_oracle_hashes = oracle_locality_vector.size() *
        (num_hashes_in_a_membership_proof(num_queries, input_oracle_MT_depth));
    total_hashes += input_oracle_hashes;
    size_t current_codeword_dim = input_oracle_MT_depth;
    // printf("predicted BCS MT_depth %lu\n", input_oracle_MT_depth);
    // printf("predicted BCS num_hashes %lu\n", total_hashes);

    for (size_t i = 1; i < fri_localization_vector.size(); ++i)
    {
        const size_t MT_depth = current_codeword_dim - fri_localization_vector[i];
        const size_t num_hashes_in_proof =
            num_hashes_in_a_membership_proof(num_queries, MT_depth);
        // printf("predicted BCS MT_depth %lu\n", MT_depth);
        // printf("predicted BCS num_hashes %lu\n", num_hashes_in_proof);
        total_hashes += num_hashes_in_proof;
        current_codeword_dim = MT_depth;
    }

    return total_hashes;
}

size_t num_elements_in_query_answers(
    std::vector<size_t> oracle_locality_vector,
    std::vector<size_t> fri_localization_vector,
    size_t num_queries,
    size_t codeword_dim)
{
    size_t total_per_query = 0;
    /* Handle the input oracles */
    for (size_t i = 0; i < oracle_locality_vector.size(); ++i)
    {
        total_per_query += oracle_locality_vector[i] * (1ull << fri_localization_vector[0]);
    }

    for (size_t i = 1; i < fri_localization_vector.size(); ++i)
    {
        total_per_query += (1ull << fri_localization_vector[i]);
    }
    /* Assumes no query point collisions */
    const size_t total = total_per_query * num_queries;

    return total;
}

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

template<typename FieldT>
size_t argument_size_predictor(
    std::vector<size_t> oracle_locality_vector,
    std::vector<size_t> fri_localization_vector,
    size_t codeword_dim,
    size_t num_queries,
    size_t max_tested_degree,
    size_t hash_size_in_bytes)
{
    const size_t field_size_in_bits = log_of_field_size_helper<FieldT>(FieldT::zero());
    const size_t field_size_in_bytes = (field_size_in_bits + 7) / 8;

    size_t num_prover_messages =
        FRI_final_interpolation_degree(max_tested_degree, fri_localization_vector);
    size_t num_query_answers =
        num_elements_in_query_answers(oracle_locality_vector, fri_localization_vector,
            num_queries, codeword_dim);
    const size_t IOP_size = field_size_in_bytes * (num_prover_messages + num_query_answers);

    const size_t total_hashes = num_hashes_in_all_membership_proofs(
        oracle_locality_vector, fri_localization_vector, num_queries, codeword_dim);
    const size_t num_MT_roots =
        fri_localization_vector.size() + oracle_locality_vector.size() - 1;
    const size_t BCS_size = hash_size_in_bytes * (num_MT_roots + total_hashes);

    // printf("predicted IOP size %lu\n", IOP_size);
    // printf("predicted BCS size %lu\n", BCS_size);
    return IOP_size + BCS_size;
}

/* return the vector of FRI localization parameters that is predicted to produce the smallest
   argument size for these parameters */
template<typename FieldT>
std::vector<size_t> compute_argument_size_optimal_localization_parameters(
    std::vector<size_t> oracle_locality_vector,
    size_t codeword_dim,
    size_t num_queries,
    size_t max_tested_degree,
    size_t hash_size_in_bytes)
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
        size_t current = argument_size_predictor<FieldT>(
                            oracle_locality_vector, options[i],
                            codeword_dim, num_queries, max_tested_degree,
                            hash_size_in_bytes);
        if (current < min)
        {
            min = current;
            best = options[i];
        }
    }

    return best;
}

}