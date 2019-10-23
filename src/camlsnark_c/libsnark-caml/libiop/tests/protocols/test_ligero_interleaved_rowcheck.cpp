#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include <libff/algebra/curves/edwards/edwards_pp.hpp>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/common/common.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/encoded/ligero/interleaved_rowcheck.hpp"

namespace libiop {

template<typename FieldT>
std::vector<oracle<FieldT>> generate_row_oracles(std::vector<FieldT> values,
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
    
        typename std::vector<FieldT>::const_iterator first = values.begin() + start;
        typename std::vector<FieldT>::const_iterator last = values.begin() + end;

        std::vector<FieldT> row_vector(first, last);
        std::vector<FieldT> row_coefficients =
            IFFT_over_field_subset<FieldT>(row_vector, systematic_domain);
        oracle<FieldT> row_oracle(FFT_over_field_subset<FieldT>(row_coefficients, codeword_domain));
        row_oracles.push_back(std::move(row_oracle));
    }
    
    return row_oracles;
}

template<typename FieldT>
bool run_test(std::vector<FieldT> x_vector,
              std::vector<FieldT> y_vector,
              std::vector<FieldT> z_vector,
              std::size_t codeword_domain_size,
              bool make_zk,
              field_subset_type domain_type)
{
    const std::size_t codeword_domain_dim = log2(codeword_domain_size);
    const std::size_t systematic_domain_dim = codeword_domain_dim - 2;
    
    const FieldT shift(3);
    const std::size_t systematic_domain_size = 1ull << systematic_domain_dim;
    const std::size_t extended_systematic_domain_size = 1ull << (systematic_domain_dim + 1);

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
    
    const std::size_t encoding_independence = 3;
    
    std::vector<oracle_handle_ptr> x_vector_handles;
    for (size_t i = 0; i < num_oracles; ++i)
    {
        x_vector_handles.emplace_back(std::make_shared<oracle_handle>(
            IOP.register_oracle(codeword_domain_handle, systematic_domain.num_elements()+1, make_zk)));
    }
    
    std::vector<oracle_handle_ptr> y_vector_handles;
    for (size_t i = 0; i < num_oracles; ++i)
    {
        y_vector_handles.emplace_back(std::make_shared<oracle_handle>(
            IOP.register_oracle(codeword_domain_handle, systematic_domain.num_elements()+1, make_zk)));
    }
    
    std::vector<oracle_handle_ptr> z_vector_handles;
    for (size_t i = 0; i < num_oracles; ++i)
    {
        z_vector_handles.emplace_back(std::make_shared<oracle_handle>(
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
    
    interleaved_rowcheck_protocol<FieldT> rowconstraints(IOP,
                                                         codeword_domain_handle,
                                                         systematic_domain_handle,
                                                         extended_systematic_domain_handle,
                                                         num_oracles,
                                                         num_queries,
                                                         num_interactions,
                                                         make_zk,
                                                         domain_type);
    rowconstraints.attach_vector_row_oracles(x_vector_handles,
                                             y_vector_handles,
                                             z_vector_handles);
    if (make_zk)
    {
        rowconstraints.attach_blinding_vector_row_oracles(blinding_vector_handles);
    }
    rowconstraints.register_linear_combinations();
    rowconstraints.register_responses();
    IOP.seal_interaction_registrations();
    
    rowconstraints.register_queries();
    IOP.seal_query_registrations();
    
    /* Proving */
    std::vector<oracle<FieldT>> x_row_oracles = generate_row_oracles<FieldT>(x_vector,
                                                                             num_oracles,
                                                                             systematic_domain,
                                                                             codeword_domain);
    std::vector<oracle<FieldT>> y_row_oracles = generate_row_oracles<FieldT>(y_vector,
                                                                             num_oracles,
                                                                             systematic_domain,
                                                                             codeword_domain);
    std::vector<oracle<FieldT>> z_row_oracles = generate_row_oracles<FieldT>(z_vector,
                                                                             num_oracles,
                                                                             systematic_domain,
                                                                             codeword_domain);
    for (size_t i = 0; i < num_oracles; ++i)
    {
        IOP.submit_oracle(x_vector_handles[i], std::move(x_row_oracles[i]));
        IOP.submit_oracle(y_vector_handles[i], std::move(y_row_oracles[i]));
        IOP.submit_oracle(z_vector_handles[i], std::move(z_row_oracles[i]));
    }
    
    if (make_zk)
    {
        for (size_t h = 0; h < num_interactions; ++h)
        {
            /* Encode 0 vector. */
            std::vector<FieldT> elems(extended_systematic_domain_size, FieldT(0));
            for (size_t i = systematic_domain_size; i < systematic_domain_size + encoding_independence; ++i)
            {
                const std::size_t index = extended_systematic_domain.reindex_by_subset(
                    systematic_domain_dim, i);
                elems[index] = FieldT::random_element();
            }
            
            std::vector<FieldT> coeffs = IFFT_over_field_subset<FieldT>(elems, extended_systematic_domain);
            std::vector<FieldT> vector = FFT_over_field_subset<FieldT>(coeffs, codeword_domain);
            oracle<FieldT> vector_oracle(vector);
            
            IOP.submit_oracle(blinding_vector_handles[h], std::move(vector_oracle));
        }
    }
    
    IOP.signal_prover_round_done();
    rowconstraints.calculate_and_submit_responses();
    IOP.signal_prover_round_done();
    
    /* Verification */
    return rowconstraints.verifier_predicate();
}

TEST(InterleavedRowcheckTrueTest, SimpleTest) {
    typedef gf64 FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> x_vector;
        std::vector<FieldT> y_vector;
        std::vector<FieldT> z_vector;
        for (size_t i = 0; i < vector_size; ++i)
        {
            x_vector.emplace_back(FieldT::random_element());
            y_vector.emplace_back(FieldT::random_element());
            z_vector.emplace_back(x_vector[i] * y_vector[i]);
        }
        
        EXPECT_TRUE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, true, affine_subspace_type));
        EXPECT_TRUE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, false, affine_subspace_type));
    }
}

TEST(InterleavedRowcheckTrueMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> x_vector;
        std::vector<FieldT> y_vector;
        std::vector<FieldT> z_vector;
        for (size_t i = 0; i < vector_size; ++i)
        {
            x_vector.emplace_back(FieldT::random_element());
            y_vector.emplace_back(FieldT::random_element());
            z_vector.emplace_back(x_vector[i] * y_vector[i]);
        }
    
        EXPECT_TRUE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, true, multiplicative_coset_type));
        EXPECT_TRUE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, false, multiplicative_coset_type));
    }
}

TEST(InterleavedRowcheckBadXTest, SimpleTest) {
    typedef gf64 FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> x_vector;
        std::vector<FieldT> y_vector;
        std::vector<FieldT> z_vector;
        for (size_t i = 0; i < vector_size; ++i)
        {
            x_vector.emplace_back(FieldT::random_element());
            y_vector.emplace_back(FieldT::random_element());
            z_vector.emplace_back(x_vector[i] * y_vector[i]);
        }
        const std::size_t index = rand() % vector_size;
        x_vector[index] = FieldT::random_element();
    
        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, true, affine_subspace_type));
        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, false, affine_subspace_type));
    }
}

TEST(InterleavedRowcheckBadXMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> x_vector;
        std::vector<FieldT> y_vector;
        std::vector<FieldT> z_vector;
        for (size_t i = 0; i < vector_size; ++i)
        {
            x_vector.emplace_back(FieldT::random_element());
            y_vector.emplace_back(FieldT::random_element());
            z_vector.emplace_back(x_vector[i] * y_vector[i]);
        }
        const std::size_t index = rand() % vector_size;
        x_vector[index] = FieldT::random_element();
        
        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, true, multiplicative_coset_type));
        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, false, multiplicative_coset_type));
    }
}

TEST(InterleavedRowcheckBadYTest, SimpleTest) {
    typedef gf64 FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> x_vector;
        std::vector<FieldT> y_vector;
        std::vector<FieldT> z_vector;
        for (size_t i = 0; i < vector_size; ++i)
        {
            x_vector.emplace_back(FieldT::random_element());
            y_vector.emplace_back(FieldT::random_element());
            z_vector.emplace_back(x_vector[i] * y_vector[i]);
        }
        const std::size_t index = rand() % vector_size;
        y_vector[index] = FieldT::random_element();
    
        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, true, affine_subspace_type));
        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, false, affine_subspace_type));
    }
}

TEST(InterleavedRowcheckBadYMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> x_vector;
        std::vector<FieldT> y_vector;
        std::vector<FieldT> z_vector;
        for (size_t i = 0; i < vector_size; ++i)
        {
            x_vector.emplace_back(FieldT::random_element());
            y_vector.emplace_back(FieldT::random_element());
            z_vector.emplace_back(x_vector[i] * y_vector[i]);
        }
        const std::size_t index = rand() % vector_size;
        y_vector[index] = FieldT::random_element();

        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, true, multiplicative_coset_type));
        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, false, multiplicative_coset_type));
    }
}

TEST(InterleavedRowcheckBadZTest, SimpleTest) {
    typedef gf64 FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> x_vector;
        std::vector<FieldT> y_vector;
        std::vector<FieldT> z_vector;
        for (size_t i = 0; i < vector_size; ++i)
        {
            x_vector.emplace_back(FieldT::random_element());
            y_vector.emplace_back(FieldT::random_element());
            z_vector.emplace_back(x_vector[i] * y_vector[i]);
        }
        const std::size_t index = rand() % vector_size;
        z_vector[index] = FieldT::random_element();

        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, true, affine_subspace_type));
        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, false, affine_subspace_type));
    }
}

TEST(InterleavedRowcheckBadZMultiplicativeTest, SimpleTest) {
    edwards_pp::init_public_params();

    typedef edwards_Fr FieldT;

    for (size_t vector_size = 16; vector_size <= 128; vector_size *= 2)
    {
        std::vector<FieldT> x_vector;
        std::vector<FieldT> y_vector;
        std::vector<FieldT> z_vector;
        for (size_t i = 0; i < vector_size; ++i)
        {
            x_vector.emplace_back(FieldT::random_element());
            y_vector.emplace_back(FieldT::random_element());
            z_vector.emplace_back(x_vector[i] * y_vector[i]);
        }
        const std::size_t index = rand() % vector_size;
        z_vector[index] = FieldT::random_element();

        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, true, multiplicative_coset_type));
        EXPECT_FALSE(run_test<FieldT>(x_vector, y_vector, z_vector, vector_size, false, multiplicative_coset_type));
    }
}

}

