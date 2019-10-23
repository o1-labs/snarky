#include <cstdint>
#include <stdexcept>

#include <gtest/gtest.h>

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"
#include "libiop/iop/iop.hpp"

namespace libiop {

TEST(IOPTest, OracleRegistration) {
    typedef gf64 FieldT;

    const std::size_t L_dim = 10;
    for (size_t i = 0; i < 2; i++)
    {
        const bool make_zk = (i == 1);
        
        iop_protocol<FieldT> IOP;
        const affine_subspace<FieldT> L = linear_subspace<FieldT>::standard_basis(L_dim);
        const domain_handle L_handle = IOP.register_subspace(L);

        const oracle_handle R_handle = IOP.register_oracle(L_handle, 20, make_zk); /* R \in RS[L,21] */
        UNUSED(R_handle);

        /* registering an overflowing oracle (deg >= elements in subspace)
        should raise an error */
        EXPECT_THROW(IOP.register_oracle(L_handle, 1024, make_zk), std::invalid_argument);
    }
}

/* TODO: add more tests for the basic IOP scaffolding */

TEST(IOPTest, SumcheckTest) {
    typedef gf64 FieldT;

    const std::size_t H_dim = 10;
    const std::size_t L_dim = 12;
    const bool make_zk = false;

    const std::size_t H_size = 1ull<<H_dim;
    const std::size_t L_size = 1ull<<L_dim;

    const std::size_t f_degree = 1ull<<11; /* somewhere in the middle between the sizes of H and L */
    const std::size_t g_degree = H_size - 1;
    const std::size_t h_degree = f_degree - H_size;

    const affine_subspace<FieldT> L = linear_subspace<FieldT>::standard_basis(L_dim);
    const affine_subspace<FieldT> H = linear_subspace<FieldT>::standard_basis(H_dim);

    /* Common input: a polynomial f, and naively computed sum \mu. */
    const polynomial<FieldT> f = polynomial<FieldT>::random_polynomial(f_degree);
    FieldT mu = FieldT(0);

    for (auto &el : H.all_elements())
    {
        mu += f.evaluation_at_point(el);
    }

    /* IOP registrations */
    iop_protocol<FieldT> IOP;
    const domain_handle L_handle = IOP.register_subspace(L);
    const oracle_handle h_handle = IOP.register_oracle(L_handle, h_degree, make_zk);
    const oracle_handle g_handle = IOP.register_oracle(L_handle, g_degree, make_zk);

    IOP.seal_interaction_registrations();

    const random_query_position_handle r_handle = IOP.register_random_query_position(L_handle);

    const query_handle h_at_r_handle = IOP.register_query(std::make_shared<oracle_handle>(h_handle), r_handle);
    const query_handle g_at_r_handle = IOP.register_query(std::make_shared<oracle_handle>(g_handle), r_handle);

    IOP.seal_query_registrations();

    /* Prover work */
    const linearized_polynomial<FieldT> Z_lin = vanishing_polynomial_from_subspace<FieldT>(H);

    std::pair<polynomial<FieldT>,
              polynomial<FieldT> > h_and_g =
        polynomial_over_linearized_polynomial<FieldT>(f, Z_lin);

    /* in fact the second component is g + \beta * X^{H_size-1}, but
       immediately remove the latter term */
    const polynomial<FieldT> &h = h_and_g.first;
    polynomial<FieldT> &g = h_and_g.second;
    EXPECT_EQ(g.num_terms(), H_size);

    const FieldT beta = g[H_size-1];
    UNUSED(beta);

    g.set_degree(H_size-2, true); /* g is now a degree H_size-2 polynomial */

    oracle<FieldT> h_oracle(additive_FFT_wrapper<FieldT>(h.coefficients(), L));
    oracle<FieldT> g_oracle(additive_FFT_wrapper<FieldT>(g.coefficients(), L));
    IOP.submit_oracle(h_handle, std::move(h_oracle));
    IOP.submit_oracle(g_handle, std::move(g_oracle));
    IOP.signal_prover_round_done();

    /* Verifier work */
    const std::size_t r_idx = IOP.obtain_query_position(r_handle);
    EXPECT_TRUE(r_idx < L_size);
    const FieldT r = L.element_by_index(r_idx);
    const FieldT h_at_r = IOP.obtain_query_response(h_at_r_handle);
    EXPECT_EQ(h_at_r, h.evaluation_at_point(r));

    const FieldT g_at_r = IOP.obtain_query_response(g_at_r_handle);
    EXPECT_EQ(g_at_r, g.evaluation_at_point(r));

    /* Obtain the linear term of Z_H. Computing the entire vanishing
       polynomial takes O(\log^2 |H|) time, and there is a way of
       doing this in O(\log |H|) time. But this is not on the critical
       path, so we just roll with it for now. */
    const linearized_polynomial<FieldT> Z_H = vanishing_polynomial_from_subspace(H);
    const FieldT c = (Z_H.num_terms() < 2 ? FieldT(0) : Z_H[1]);

    const FieldT f_at_r = f.evaluation_at_point(r);
    const FieldT Z_H_at_r = Z_H.evaluation_at_point(r);
    const FieldT r_to_H_minus = power<FieldT>(r, H_size-1);

    const FieldT lhs = c * f_at_r;
    const FieldT rhs = c * g_at_r + mu * r_to_H_minus + c * Z_H_at_r * h_at_r;
    EXPECT_EQ(lhs, rhs);
}

TEST(IOPTest, ZeroKnowledgeSumcheckTest) {
    typedef gf64 FieldT;

    const std::size_t H_dim = 10;
    const std::size_t L_dim = 12;
    const bool make_zk = true;

    const std::size_t H_size = 1ull<<H_dim;
    const std::size_t L_size = 1ull<<L_dim;

    const std::size_t f_degree = 1ull<<11; /* somewhere in the middle between the sizes of H and L */
    const std::size_t g_degree = H_size - 1;
    const std::size_t h_degree = f_degree - H_size;

    const affine_subspace<FieldT> L = linear_subspace<FieldT>::standard_basis(L_dim);
    const affine_subspace<FieldT> H = linear_subspace<FieldT>::standard_basis(H_dim);

    /* Common input: a polynomial f, and naively computed sum \mu. */
    const polynomial<FieldT> f = polynomial<FieldT>::random_polynomial(f_degree);
    FieldT mu = FieldT(0);

    for (auto &el : H.all_elements())
    {
        mu += f.evaluation_at_point(el);
    }

    /* IOP registrations */
    iop_protocol<FieldT> IOP;
    const domain_handle L_handle = IOP.register_subspace(L);

    const oracle_handle pad_handle = IOP.register_oracle(L_handle, f_degree, make_zk);
    const prover_message_handle pad_sum_handle = IOP.register_prover_message(1);
    const verifier_random_message_handle challenge_handle = IOP.register_verifier_random_message(1);
    const oracle_handle h_handle = IOP.register_oracle(L_handle, h_degree, make_zk);
    const oracle_handle g_handle = IOP.register_oracle(L_handle, g_degree, make_zk);

    IOP.seal_interaction_registrations();

    const random_query_position_handle r_handle = IOP.register_random_query_position(L_handle);

    const query_handle pad_at_r_handle = IOP.register_query(std::make_shared<oracle_handle>(pad_handle), r_handle);
    const query_handle h_at_r_handle = IOP.register_query(std::make_shared<oracle_handle>(h_handle), r_handle);
    const query_handle g_at_r_handle = IOP.register_query(std::make_shared<oracle_handle>(g_handle), r_handle);

    IOP.seal_query_registrations();

    /* Prover work */
    const polynomial<FieldT> pad = polynomial<FieldT>::random_polynomial(f_degree);
    oracle<FieldT> pad_oracle = IOP.submit_oracle(
        pad_handle,
        oracle<FieldT>(additive_FFT_wrapper<FieldT>(pad.coefficients(), L)));

    FieldT pad_sum = FieldT(0);
    for (auto &el : H.all_elements())
    {
        pad_sum += pad.evaluation_at_point(el);
    }
    IOP.submit_prover_message(pad_sum_handle, std::vector<FieldT>{pad_sum});
    IOP.signal_prover_round_done();

    const std::vector<FieldT> challenge_prover_view = IOP.obtain_verifier_random_message(challenge_handle);
    EXPECT_EQ(challenge_prover_view.size(), 1u);

    polynomial<FieldT> virtual_oracle_contents;
    virtual_oracle_contents.reserve(f_degree);
    for (size_t i = 0 ; i < f_degree; ++i)
    {
        virtual_oracle_contents.add_term(pad[i] + challenge_prover_view[0] * f[i]);
    }
    EXPECT_EQ(virtual_oracle_contents.evaluation_at_point(FieldT(1)),
              pad.evaluation_at_point(FieldT(1)) + challenge_prover_view[0] * f.evaluation_at_point(FieldT(1)));

    const linearized_polynomial<FieldT> Z_lin = vanishing_polynomial_from_subspace<FieldT>(H);
    std::pair<polynomial<FieldT>,
              polynomial<FieldT> > h_and_g =
        polynomial_over_linearized_polynomial<FieldT>(virtual_oracle_contents, Z_lin);

    /* in fact the second component is g + \beta * X^{H_size-1}, but
       immediately remove the latter term */
    const polynomial<FieldT> &h = h_and_g.first;
    polynomial<FieldT> &g = h_and_g.second;
    EXPECT_EQ(g.num_terms(), H_size);

    const FieldT beta = g[H_size-1];
    UNUSED(beta);

    g.set_degree(H_size-2, true); /* g is now a degree H_size-2 polynomial */

    oracle<FieldT> h_oracle(additive_FFT_wrapper<FieldT>(h.coefficients(), L));
    oracle<FieldT> g_oracle(additive_FFT_wrapper<FieldT>(g.coefficients(), L));
    IOP.submit_oracle(h_handle, std::move(h_oracle));
    IOP.submit_oracle(g_handle, std::move(g_oracle));
    IOP.signal_prover_round_done();

    /* Verifier work */
    const std::vector<FieldT> challenge_verifier_view = IOP.obtain_verifier_random_message(challenge_handle);
    EXPECT_EQ(challenge_verifier_view, challenge_prover_view);

    const std::size_t r_idx = IOP.obtain_query_position(r_handle);
    EXPECT_TRUE(r_idx < L_size);
    const FieldT r = L.element_by_index(r_idx);

    const FieldT pad_at_r = IOP.obtain_query_response(pad_at_r_handle);
    EXPECT_EQ(pad_at_r, pad.evaluation_at_point(r));
    const FieldT virtual_oracle_at_r = (pad_at_r +
                                        challenge_verifier_view[0] * f.evaluation_at_point(r));
    EXPECT_EQ(virtual_oracle_at_r, virtual_oracle_contents.evaluation_at_point(r));

    const FieldT h_at_r = IOP.obtain_query_response(h_at_r_handle);
    EXPECT_EQ(h_at_r, h.evaluation_at_point(r));

    const FieldT g_at_r = IOP.obtain_query_response(g_at_r_handle);
    EXPECT_EQ(g_at_r, g.evaluation_at_point(r));

    /* Obtain the linear term of Z_H. Computing the entire vanishing
       polynomial takes O(\log^2 |H|) time, and there is a way of
       doing this in O(\log |H|) time. But this is not on the critical
       path, so we just roll with it for now. */
    const linearized_polynomial<FieldT> Z_H = vanishing_polynomial_from_subspace(H);
    const FieldT c = (Z_H.num_terms() < 2 ? FieldT(0) : Z_H[1]);
    const FieldT Z_H_at_r = Z_H.evaluation_at_point(r);
    const FieldT r_to_H_minus = power<FieldT>(r, H_size-1);

    const FieldT lhs = c * virtual_oracle_at_r;
    const FieldT rhs = (c * g_at_r + (pad_sum + challenge_verifier_view[0] * mu) * r_to_H_minus +
                        c * Z_H_at_r * h_at_r);
    EXPECT_EQ(lhs, rhs);
}

}
