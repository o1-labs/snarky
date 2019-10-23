#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"
#include "libiop/protocols/encoded/common/rowcheck.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"
#include "libiop/relations/r1cs.hpp"
#include "libiop/relations/variable.hpp"
#include "libiop/tests/protocols/utilities.cpp"

namespace libiop {

template<typename FieldT>
void run_test(const field_subset<FieldT> constraint_domain,
              const field_subset<FieldT> codeword_domain,
              const std::vector<FieldT> Az_over_codeword_domain,
              const std::vector<FieldT> Bz_over_codeword_domain,
              const std::vector<FieldT> Cz_over_codeword_domain,
              bool make_zk,
              std::size_t query_bound,
              bool exp_pass)
{
    rowcheck_ABC_virtual_oracle<FieldT> rowcheck_oracle(
        codeword_domain,
        constraint_domain);

    // calculate rowcheck output
    const std::vector<std::shared_ptr<std::vector<FieldT>>> codewords(
        {std::make_shared<std::vector<FieldT>>(Az_over_codeword_domain),
         std::make_shared<std::vector<FieldT>>(Bz_over_codeword_domain),
         std::make_shared<std::vector<FieldT>>(Cz_over_codeword_domain)});
    const std::shared_ptr<std::vector<FieldT>> rowcheck_evals = rowcheck_oracle.evaluated_contents(codewords);
    const polynomial<FieldT> rowcheck_poly(
        IFFT_over_field_subset<FieldT>(*rowcheck_evals.get(), codeword_domain));
    /** degree is deg((f_Az * f_Bz - f_Cz) / Z_{constraint}) =
     * (|constraint_domain| + query_bound) * 2 - constraint_domain =
     * |constraint_domain| + query_bound * 2 */
    size_t rowcheck_degree = constraint_domain.num_elements() + (make_zk ? query_bound * 2 : 0) - 1;
    if (exp_pass) {
        EXPECT_EQ(rowcheck_degree, rowcheck_poly.minimal_num_terms()) << "rowcheck polynomial is of incorrect degree";
    } else {
        EXPECT_NE(rowcheck_degree, rowcheck_poly.minimal_num_terms()) << "rowcheck polynomial is of correct degree";
    }
    // Ensure that evaluation at point matches oracle evaluations for a few points
    std::srand(0);
    for (std::size_t i = 0; i < 10; i++) {
        std::size_t evaluation_index = std::rand() % codeword_domain.num_elements();
        FieldT evaluation_elem = codeword_domain.element_by_index(evaluation_index);
        const std::vector<FieldT> constituent_evaluations(
            {Az_over_codeword_domain[evaluation_index],
            Bz_over_codeword_domain[evaluation_index],
            Cz_over_codeword_domain[evaluation_index]});
        FieldT eval = rowcheck_oracle.evaluation_at_point(
            evaluation_index,
            evaluation_elem,
            constituent_evaluations);
        EXPECT_TRUE(eval == rowcheck_evals->operator[](evaluation_index)) << "rowcheck evaluation at point was inconsistent";
    }
}

template<typename FieldT>
std::vector<std::vector<FieldT>> calculate_ABCz(r1cs_constraint_system<FieldT> cs,
    std::vector<FieldT> z) {
    std::vector<FieldT> Az, Bz, Cz;
    for (size_t i = 0; i < cs.num_constraints(); ++i)
    {
        FieldT Az_i = FieldT::zero();
        for (auto &lt : cs.constraints_[i].a_.terms)
        {
            Az_i += z[lt.index_] * lt.coeff_;
        }
        Az.emplace_back(Az_i);

        FieldT Bz_i = FieldT::zero();
        for (auto &lt : cs.constraints_[i].b_.terms)
        {
            Bz_i += z[lt.index_] * lt.coeff_;
        }
        Bz.emplace_back(Bz_i);

        FieldT Cz_i = FieldT::zero();
        for (auto &lt : cs.constraints_[i].c_.terms)
        {
            Cz_i += z[lt.index_] * lt.coeff_;
        }
        Cz.emplace_back(Cz_i);
    }
    std::vector<std::vector<FieldT>> ABCz({ Az, Bz, Cz });
    return ABCz;
}

template<typename FieldT>
std::vector<std::vector<FieldT>> convert_ABCz_to_codeword_domain(
    std::vector<std::vector<FieldT>> ABCz,
    bool make_zk,
    std::size_t query_bound,
    field_subset<FieldT> constraint_domain,
    field_subset<FieldT> codeword_domain) {
    std::vector<std::vector<FieldT>> ABCz_over_codeword_domain;
    for (std::size_t i = 0; i < 3; i++) {
        std::vector<FieldT> f_Mz = IFFT_over_field_subset(ABCz[i], constraint_domain);
        std::vector<FieldT> Mz_over_codeword_domain = FFT_over_field_subset(f_Mz, codeword_domain);
        if (make_zk) {
            Mz_over_codeword_domain = make_codeword_zk(
                Mz_over_codeword_domain, query_bound, constraint_domain, codeword_domain);
        }
        ABCz_over_codeword_domain.emplace_back(Mz_over_codeword_domain);
    }
    return ABCz_over_codeword_domain;
}

template<typename FieldT>
void run_random_rowcheck_instance(std::size_t constraint_domain_dim,
                                  std::size_t variable_domain_dim,
                                  std::size_t input_variable_domain_dim,
                                  bool make_zk) {
    const std::size_t num_constraints = 1 << constraint_domain_dim;
    const std::size_t num_inputs = (1 << input_variable_domain_dim) - 1;
    const std::size_t num_variables = (1 << variable_domain_dim) - 1;
    r1cs_example<FieldT> r1cs_params = generate_r1cs_example<FieldT>(
        num_constraints, num_inputs, num_variables);

    const std::size_t codeword_domain_dim = 4 +
        std::max(constraint_domain_dim, variable_domain_dim);

    const field_subset<FieldT> unshifted_codeword_domain(1ull << codeword_domain_dim);
    const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();

    const field_subset<FieldT> constraint_domain(1ull << constraint_domain_dim);
    const field_subset<FieldT> variable_domain(1ull << variable_domain_dim);
    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, codeword_domain_shift);
    std::vector<FieldT> variable_assignment({ FieldT::one() });
    variable_assignment.insert(variable_assignment.end(),
        r1cs_params.primary_input_.begin(), r1cs_params.primary_input_.end());
    variable_assignment.insert(variable_assignment.end(),
        r1cs_params.auxiliary_input_.begin(), r1cs_params.auxiliary_input_.end());
    EXPECT_TRUE(r1cs_params.constraint_system_.is_satisfied(r1cs_params.primary_input_, r1cs_params.auxiliary_input_));

    const std::size_t query_bound = num_inputs + 2;
    const std::vector<std::vector<FieldT>> ABCz = calculate_ABCz(
        r1cs_params.constraint_system_,
        variable_assignment);
    const std::vector<std::vector<FieldT>> ABCz_over_codeword_domain =
        convert_ABCz_to_codeword_domain(
            ABCz, make_zk, query_bound,
            constraint_domain,
            codeword_domain);
    run_test(constraint_domain, codeword_domain,
        ABCz_over_codeword_domain[0], ABCz_over_codeword_domain[1], ABCz_over_codeword_domain[2],
        make_zk, query_bound, true);
}

TEST(AdditiveSucceedingTests, RowcheckTest) {
    typedef gf64 FieldT;
    for (std::size_t constraint_domain_dim = 6; constraint_domain_dim < 8; constraint_domain_dim++) {
        for (std::size_t variable_domain_dim = 6; variable_domain_dim < 8; variable_domain_dim++) {
            std::size_t input_variable_domain_dim = variable_domain_dim - 2;
            for (std::size_t make_zk_param = 0; make_zk_param < 2; make_zk_param++) {
                run_random_rowcheck_instance<FieldT>(
                    constraint_domain_dim,
                    variable_domain_dim,
                    input_variable_domain_dim,
                    (make_zk_param == 1));
            }
        }
    }
}

TEST(MultiplicativeSucceedingTests, RowcheckTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    for (std::size_t constraint_domain_dim = 6; constraint_domain_dim < 8; constraint_domain_dim++) {
        for (std::size_t variable_domain_dim = 6; variable_domain_dim < 8; variable_domain_dim++) {
            std::size_t input_variable_domain_dim = variable_domain_dim - 2;
            for (std::size_t make_zk_param = 0; make_zk_param < 2; make_zk_param++) {
                run_random_rowcheck_instance<FieldT>(
                    constraint_domain_dim,
                    variable_domain_dim,
                    input_variable_domain_dim,
                    (make_zk_param == 1));
            }
        }
    }
}

// This tests the following failing scenarios:
// one auxiliary element is wrong
// TODO: Add more failing scenarios
template<typename FieldT>
void run_failing_rowcheck_instances(std::size_t constraint_domain_dim,
                                  std::size_t variable_domain_dim,
                                  std::size_t input_variable_domain_dim) {
    const std::size_t num_constraints = 1 << constraint_domain_dim;
    const std::size_t num_inputs = (1 << input_variable_domain_dim) - 1;
    const std::size_t num_variables = (1 << variable_domain_dim) - 1;
    r1cs_example<FieldT> r1cs_params = generate_r1cs_example<FieldT>(
        num_constraints, num_inputs, num_variables);

    const std::size_t codeword_domain_dim = 4 +
        std::max(constraint_domain_dim, variable_domain_dim);
    const field_subset<FieldT> unshifted_codeword_domain(1ull << codeword_domain_dim);
    const FieldT codeword_domain_shift = unshifted_codeword_domain.element_outside_of_subset();

    const field_subset<FieldT> constraint_domain(1ull << constraint_domain_dim);
    const field_subset<FieldT> variable_domain(1ull << variable_domain_dim);
    const field_subset<FieldT> codeword_domain(1ull << codeword_domain_dim, codeword_domain_shift);

    std::vector<FieldT> variable_assignment({ FieldT::one() });
    variable_assignment.insert(variable_assignment.end(),
        r1cs_params.primary_input_.begin(), r1cs_params.primary_input_.end());
    variable_assignment.insert(variable_assignment.end(),
        r1cs_params.auxiliary_input_.begin(), r1cs_params.auxiliary_input_.end());
    const std::size_t query_bound = num_inputs + 2;

    // Alter one element of auxiliary input
    std::size_t index = 1 + num_inputs + (std::rand() % (num_variables - num_inputs));
    variable_assignment[index] = FieldT::random_element();
    // run failing tests
    for (std::size_t make_zk_param = 0; make_zk_param < 2; make_zk_param++) {
        bool make_zk = make_zk_param == 1;
        const std::vector<std::vector<FieldT>> ABCz = calculate_ABCz(
            r1cs_params.constraint_system_,
            variable_assignment);
        const std::vector<std::vector<FieldT>> ABCz_over_codeword_domain =
            convert_ABCz_to_codeword_domain(
                ABCz, make_zk, query_bound,
                constraint_domain,
                codeword_domain);
        run_test(constraint_domain, codeword_domain,
            ABCz_over_codeword_domain[0], ABCz_over_codeword_domain[1], ABCz_over_codeword_domain[2],
            make_zk, query_bound, false);
    }
}

TEST(AdditiveFailingTests, RowcheckTest) {
    typedef gf64 FieldT;
    run_failing_rowcheck_instances<FieldT>(6, 7, 5);
}

TEST(MultiplicativeFailingTests, RowcheckTest) {
    alt_bn128_pp::init_public_params();
    typedef alt_bn128_Fr FieldT;
    run_failing_rowcheck_instances<FieldT>(6, 7, 5);
}

}