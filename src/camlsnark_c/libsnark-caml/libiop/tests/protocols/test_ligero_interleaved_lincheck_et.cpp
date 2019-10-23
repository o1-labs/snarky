#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include <libff/algebra/curves/edwards/edwards_pp.hpp>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/common/common.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/ligero/interleaved_lincheck_et.hpp"
#include "libiop/relations/r1cs.hpp"

namespace libiop {

template<typename FieldT>
std::vector<oracle<FieldT>> generate_row_oracles(std::vector<FieldT> vector,
                                                 std::size_t num_oracles,
                                                 field_subset<FieldT> systematic_domain,
                                                 field_subset<FieldT> codeword_domain)
{
    std::size_t systematic_domain_size = systematic_domain.num_elements();
    
    std::vector<oracle<FieldT>> row_oracles;
    for (size_t i = 0; i < num_oracles; ++i)
    {
        std::size_t start = i * systematic_domain_size;
        std::size_t end = (i + 1) * systematic_domain_size;
    
        typename std::vector<FieldT>::const_iterator first = vector.begin() + start;
        typename std::vector<FieldT>::const_iterator last = vector.begin() + end;

        std::vector<FieldT> row_vector(first, last);
        std::vector<FieldT> row_coefficients =
            IFFT_over_field_subset<FieldT>(row_vector, systematic_domain);
        oracle<FieldT> row_oracle(FFT_over_field_subset<FieldT>(row_coefficients, codeword_domain));
        row_oracles.push_back(std::move(row_oracle));
    }
    
    return row_oracles;
}

template<typename FieldT>
std::vector<FieldT> matrix_multiply(naive_sparse_matrix<FieldT> matrix, std::vector<FieldT> vector)
{
    std::vector<FieldT> result;
    for (size_t i = 0; i < vector.size(); ++i)
    {
        FieldT mult_val(0);
        std::map<std::size_t, FieldT> this_row = matrix[i];
        for (typename std::map<std::size_t, FieldT>::iterator iter = this_row.begin(); iter != this_row.end(); ++iter)
        {
            std::size_t index = iter->first;
            FieldT value = iter->second;
            mult_val += value * vector[index];
        }
        result.emplace_back(mult_val);
    }
    return result;
}

template<typename FieldT>
naive_sparse_matrix<FieldT> random_matrix(std::size_t size)
{
    naive_sparse_matrix<FieldT> random_matrix;
    for (size_t i = 0; i < size; ++i)
    {
        std::map<std::size_t, FieldT> this_row;
        for (size_t j = 0; j < size / 2; ++j)
        {
            std::size_t index = rand() % size;
            this_row.insert(std::pair<std::size_t, FieldT>(index, FieldT::random_element()));
        }
        
        random_matrix.emplace_back(this_row);
    }
    return random_matrix;
}

template<typename FieldT>
bool run_test(naive_sparse_matrix<FieldT> &constraint_matrix,
              std::vector<FieldT> input_vector,
              std::vector<FieldT> target_vector,
              std::size_t codeword_domain_size,
              bool make_zk,
              field_subset_type domain_type)
{
    const std::size_t codeword_domain_dim = log2(codeword_domain_size);
    const std::size_t systematic_domain_dim = codeword_domain_dim - 2;

    const std::size_t systematic_domain_size = 1ull << systematic_domain_dim;
    const std::size_t extended_systematic_domain_size = 1ull << (systematic_domain_dim + 1);
    
    const FieldT shift(3);

    field_subset<FieldT> codeword_domain(codeword_domain_size);
    field_subset<FieldT> systematic_domain(systematic_domain_size, shift);
    field_subset<FieldT> extended_systematic_domain(extended_systematic_domain_size, shift);
    
    /* Set up the blueprint for the protocol */
    iop_protocol<FieldT> IOP;
    
    const domain_handle codeword_domain_handle = IOP.register_domain(codeword_domain);
    const domain_handle systematic_domain_handle = IOP.register_domain(systematic_domain);
    const domain_handle extended_systematic_domain_handle = IOP.register_domain(extended_systematic_domain);
    
    const std::size_t num_oracles = 4;
    const std::size_t num_queries = 2;
    const std::size_t num_interactions = 6;
    
    std::vector<oracle_handle_ptr> input_vector_handles;
    for (size_t i = 0; i < num_oracles; ++i)
    {
        input_vector_handles.emplace_back(std::make_shared<oracle_handle>(
            IOP.register_oracle(codeword_domain_handle, systematic_domain.num_elements()+1, make_zk)));
    }
    
    std::vector<oracle_handle_ptr> blinding_vector_handles;
    if (make_zk)
    {
        for (size_t i = 0; i < num_interactions; ++i)
        {
            blinding_vector_handles.emplace_back(std::make_shared<oracle_handle>(
                IOP.register_oracle(codeword_domain_handle, codeword_domain_dim, make_zk)));
        }
    }
    
    interleaved_lincheck_et_protocol<FieldT> linconstraints(IOP,
                                                            codeword_domain_handle,
                                                            systematic_domain_handle,
                                                            extended_systematic_domain_handle,
                                                            num_oracles,
                                                            num_queries,
                                                            num_interactions,
                                                            make_zk,
                                                            domain_type,
                                                            constraint_matrix,
                                                            target_vector);
    linconstraints.attach_input_vector_row_oracles(input_vector_handles);
    if (make_zk)
    {
        linconstraints.attach_blinding_vector_row_oracles(blinding_vector_handles);
    }
    linconstraints.register_linear_combinations();
    linconstraints.register_responses();
    IOP.seal_interaction_registrations();
    
    linconstraints.register_queries();
    IOP.seal_query_registrations();
    
    /* Proving */
    std::vector<oracle<FieldT>> input_row_oracles = generate_row_oracles<FieldT>(input_vector,
                                                                                 num_oracles,
                                                                                 systematic_domain,
                                                                                 codeword_domain);
    for (size_t i = 0; i < num_oracles; ++i)
    {
        IOP.submit_oracle(input_vector_handles[i], std::move(input_row_oracles[i]));
    }
    
    if (make_zk)
    {
        for (size_t h = 0; h < num_interactions; ++h)
        {
            /* Choose random elements to sum to 0 */
            std::vector<FieldT> elems;
            elems.resize(extended_systematic_domain_size);
            FieldT sum(0);
            for (size_t i = 0; i < systematic_domain_size - 1; ++i)
            {
                /* In the multiplicative case, the systematic domain is a SUBGROUP of the extended
                   systematic domain of half the size, so it's EVERY OTHER element; in the additive
                   case, it's a SUBSPACE, so it's the FIRST HALF of the elements. */
                const std::size_t idx = extended_systematic_domain.reindex_by_subset(
                    systematic_domain_dim, i);
                elems[idx] = FieldT::random_element();
                sum += elems[idx];
            }
            const std::size_t sum_idx = extended_systematic_domain.reindex_by_subset(
                    systematic_domain_dim, systematic_domain_size - 1);
            elems[sum_idx] = - sum;

            for (size_t i = 0; i < systematic_domain_size - 1; ++i)
            {
                const std::size_t idx = extended_systematic_domain.reindex_by_subset(
                    systematic_domain_dim, i + systematic_domain_size);
                elems[idx] = FieldT(0);
            }
            
            std::vector<FieldT> coeffs = IFFT_over_field_subset<FieldT>(elems, extended_systematic_domain);
            std::vector<FieldT> vector = FFT_over_field_subset<FieldT>(coeffs, codeword_domain);
            oracle<FieldT> vector_oracle(vector);

            IOP.submit_oracle(blinding_vector_handles[h], std::move(vector_oracle));
        }
    }
    
    IOP.signal_prover_round_done();
    linconstraints.calculate_and_submit_responses();
    IOP.signal_prover_round_done();

    return linconstraints.verifier_predicate();
}
    
TEST(InterleavedLincheckETTrueTest, SimpleTest) {
    typedef gf64 FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> input_vector = random_vector<FieldT>(vector_size);
        naive_sparse_matrix<FieldT> constraint_matrix = random_matrix<FieldT>(vector_size);
        std::vector<FieldT> target_vector = matrix_multiply(constraint_matrix, input_vector);

        EXPECT_TRUE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, true, affine_subspace_type));
        EXPECT_TRUE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, false, affine_subspace_type));
    }
}

TEST(InterleavedLincheckETTrueMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> input_vector = random_vector<FieldT>(vector_size);
        naive_sparse_matrix<FieldT> constraint_matrix = random_matrix<FieldT>(vector_size);
        std::vector<FieldT> target_vector = matrix_multiply(constraint_matrix, input_vector);

        EXPECT_TRUE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, true, multiplicative_coset_type));
        EXPECT_TRUE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, false, multiplicative_coset_type));
    }
}

TEST(InterleavedLincheckETBadMatrixTest, SimpleTest) {
    typedef gf64 FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> input_vector = random_vector<FieldT>(vector_size);
        naive_sparse_matrix<FieldT> constraint_matrix = random_matrix<FieldT>(vector_size);
        std::vector<FieldT> target_vector = matrix_multiply(constraint_matrix, input_vector);

        std::size_t bad_index = rand() % vector_size;
        constraint_matrix[bad_index].clear();
        constraint_matrix[bad_index].insert(std::pair<std::size_t, FieldT>(bad_index, FieldT::random_element()));

        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, true, affine_subspace_type));
        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, false, affine_subspace_type));
    }
}

TEST(InterleavedLincheckETBadMatrixMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> input_vector = random_vector<FieldT>(vector_size);
        naive_sparse_matrix<FieldT> constraint_matrix = random_matrix<FieldT>(vector_size);
        std::vector<FieldT> target_vector = matrix_multiply(constraint_matrix, input_vector);

        std::size_t bad_index = rand() % vector_size;
        constraint_matrix[bad_index].clear();
        constraint_matrix[bad_index].insert(std::pair<std::size_t, FieldT>(bad_index, FieldT::random_element()));

        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, true, multiplicative_coset_type));
        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, false, multiplicative_coset_type));
    }
}

TEST(InterleavedLincheckETBadInputVectorTest, SimpleTest) {
    typedef gf64 FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> input_vector = random_vector<FieldT>(vector_size);
        naive_sparse_matrix<FieldT> constraint_matrix = random_matrix<FieldT>(vector_size);
        std::vector<FieldT> target_vector = matrix_multiply(constraint_matrix, input_vector);

        const std::size_t bad_index = rand() % vector_size;
        input_vector[bad_index] = FieldT::random_element();

        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, true, affine_subspace_type));
        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, false, affine_subspace_type));
    }
}

TEST(InterleavedLincheckETBadInputVectorMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> input_vector = random_vector<FieldT>(vector_size);
        naive_sparse_matrix<FieldT> constraint_matrix = random_matrix<FieldT>(vector_size);
        std::vector<FieldT> target_vector = matrix_multiply(constraint_matrix, input_vector);

        const std::size_t bad_index = rand() % vector_size;
        input_vector[bad_index] = FieldT::random_element();

        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, true, multiplicative_coset_type));
        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, false, multiplicative_coset_type));
    }
}

TEST(InterleavedLincheckETBadTargetVectorTest, SimpleTest) {
    typedef gf64 FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> input_vector = random_vector<FieldT>(vector_size);
        naive_sparse_matrix<FieldT> constraint_matrix = random_matrix<FieldT>(vector_size);
        std::vector<FieldT> target_vector = matrix_multiply(constraint_matrix, input_vector);

        const std::size_t bad_index = rand() % vector_size;
        target_vector[bad_index] = FieldT::random_element();

        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, true, affine_subspace_type));
        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, false, affine_subspace_type));
    }
}

TEST(InterleavedLincheckETBadTargetVectorMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> input_vector = random_vector<FieldT>(vector_size);
        naive_sparse_matrix<FieldT> constraint_matrix = random_matrix<FieldT>(vector_size);
        std::vector<FieldT> target_vector = matrix_multiply(constraint_matrix, input_vector);

        const std::size_t bad_index = rand() % vector_size;
        target_vector[bad_index] = FieldT::random_element();

        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, true, multiplicative_coset_type));
        EXPECT_FALSE(run_test<FieldT>(constraint_matrix, input_vector, target_vector, vector_size, false, multiplicative_coset_type));
    }
}

}
