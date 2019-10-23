/**@file
*****************************************************************************
FRI interfaces.
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_PROTOCOLS_LDT_FRI_FRI_LDT_HPP_
#define LIBIOP_PROTOCOLS_LDT_FRI_FRI_LDT_HPP_

#include <algorithm>
#include <functional>

#include "libiop/algebra/fields/utils.hpp"
#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/iop/utilities/batching.hpp"
#include "libiop/iop/utilities/query_positions.hpp"
#include "libiop/protocols/ldt/multi_ldt_base.hpp"
#include "libiop/protocols/ldt/fri/fri_aux.hpp"
#include "libiop/protocols/ldt/fri/localizer_polynomial.hpp"

namespace libiop {

/** Notation key
 *   - eta     : We call this the localization_parameter.
 *               We support supplying a vector of localization parameters,
 *               indicating how much to reduce by for each round.
 *               This enables significant proof size savings.
 *   - L^(i)   : This is domains[i] in the code.
 *   - L_0^(i) : This is localizer_domains[i] in the code,
 *               except with the affine shift being the identity.
 *   - q^(i)   : This is the localizer polynomials[i] in the code.
 *               In the multiplicative case, this is X^{2^{localization param}}.
 *               In the additive case, it is the vanishing polynomial for L_0^(i), with no affine shift.
 *   - commit  : We instead call the commit phase, the interactive phase
 *   - multi_* : This implementation supports running multiple FRI instances that all use
 *               the same interactive randomness, and query positions (for proof size reasons).
 *               The prefix multi_ to a variable name means that the final index of the nested vector
 *               is the index for which LDT instance we are in.
 */

enum class FRI_soundness_type {
    /** Soundness error lower bounds as proven in the FRI paper */
    proven = 1,
    /** Heuristic soundness error refers to the upper bounds noted in the FRI and Aurora papers */
    heuristic = 2,
};

const char* FRI_soundness_type_to_string(FRI_soundness_type soundness_type);

template<typename FieldT>
class FRI_protocol_parameters : public multi_LDT_parameter_base<FieldT> {
    protected:
    size_t target_interactive_security_parameter_;
    size_t target_query_security_parameter_;
    FRI_soundness_type soundness_type_;
    size_t poly_degree_bound_;
    size_t codeword_domain_dim_;
    size_t RS_extra_dimensions_;
    field_type field_type_;
    std::vector<size_t> localization_parameters_;

    long double effective_proximity_parameter_;
    long double soundness_per_interaction_;

    /* Number of times to repeat the interactive phase, for low interactive soundness error */
    size_t num_interactive_repetitions_;
    /* Number of times to repeat the query phase, for low query soundness error */
    size_t num_query_repetitions_;
    bool override_security_parameter_ = false;

    public:
    // Needed to allow the LDT to be uninitialized in FRI_protocol
    FRI_protocol_parameters() {};

    FRI_protocol_parameters(const size_t interactive_soundness_bits,
                            const size_t query_soundness_bits,
                            const FRI_soundness_type soundness_type,
                            const size_t poly_degree_bound,
                            const size_t codeword_domain_dim,
                            const size_t RS_extra_dimensions,
                            const size_t absolute_proximity_parameter,
                            const std::vector<size_t> localization_parameter_array);

    /* Constructor with a constant localization parameter.
       It will create a localization array that localizes
       the polynomials with the provided localization parameter,
       until they are of size less than the 2^{localization parameter}. */
    FRI_protocol_parameters(const size_t interactive_soundness_bits,
                            const size_t query_soundness_bits,
                            const FRI_soundness_type soundness_type,
                            const size_t poly_degree_bound,
                            const size_t codeword_domain_dim,
                            const size_t RS_extra_dimensions,
                            const size_t absolute_proximity_parameter,
                            const size_t localization_parameter);

    /** UNSAFE!
     *  This is intended to allow experimentation with FRI parameterizations.
     *  If the supplied parameter is non-zero, it overrides the internal repetition parameter. */
    void override_security_parameters(const size_t interactive_repititions, const size_t query_repititions);
    size_t RS_extra_dimensions() const;
    size_t poly_degree_bound() const;
    field_type get_field_type() const;
    std::vector<size_t> get_localization_parameters() const;
    size_t query_repetitions() const;
    size_t interactive_repetitions() const;

    long double achieved_interactive_soundness() const;
    long double achieved_query_soundness() const;
    void print() const;

    // Methods to help protocols using FRI
    static std::vector<size_t> localization_parameter_to_array(
        const size_t localization_parameter,
        const size_t codeword_domain_dim,
        const size_t RS_extra_dimensions);
    /** Converts an arbitrary tested degree bound to the smallest degree bound
     *  that is greater than or equal to the provided one, and which FRI can test for. */
    static size_t next_testable_degree_bound(const size_t tested_degree_bound,
                                             const std::vector<size_t> localization_parameter_array);

    field_subset<FieldT> quotient_map_domain(field_subset<FieldT> codeword_domain) const;

    size_t queries_to_input_oracles() const;

    virtual ~FRI_protocol_parameters() {};
};

/* outer index is round number; inner index is index within the queried coset */
using query_handle_set = std::vector<std::vector<query_handle> >;

/* We make a query_set for each query, interaction, and LDT instance triple.*/
struct FRI_query_set {
    /* Which LDT instance and interaction are we in? */
    size_t interaction_index_;
    size_t LDT_index_;

    random_query_position_handle s0_position_handle_;
    query_handle_set f_at_s_coset_query_handles_;
};

template<typename FieldT>
class FRI_protocol : public multi_LDT_base<FieldT> {
protected:
    FRI_protocol_parameters<FieldT> params_;

    std::size_t poly_degree_bound_;
    std::size_t num_reductions_;

    std::vector<field_subset<FieldT> > domains_;
    std::vector<field_subset<FieldT> > localizer_domains_;

    std::vector<localizer_polynomial<FieldT> > localizer_polynomials_;

    std::vector<domain_handle> domain_handles_;
    /** These are indexed by round number, then by interaction number, and finally be LDT index.
     *  There is only one interaction index for the first round */
    std::vector<std::vector<std::vector<oracle_handle_ptr> > > oracle_handles_;
    std::vector<std::vector<verifier_random_message_handle> > verifier_challenge_handles_;

    std::vector<FRI_query_set> query_sets_;

    /* In the final round, the prover sends a polynomial for each interaction / LDT instance pair. */
    std::size_t final_polynomial_degree_bound_;
    std::vector<std::vector<prover_message_handle> > final_polynomial_handles_;
public:
    /* Initialization and registration */
    FRI_protocol(iop_protocol<FieldT> &IOP,
                 multi_LDT_parameter_base<FieldT> &params,
                 const domain_handle &codeword_domain_handle,
                 const std::vector<oracle_handle_ptr> &poly_handles);

    void register_interactions();
    void register_queries();

    /* Proving */
    void calculate_and_submit_proof();

    /* Verification */
    bool verifier_predicate();

    virtual ~FRI_protocol() {};
protected:
    void compute_domains();
    bool predicate_for_query_set(const FRI_query_set &Q);
    std::size_t num_queries_made_;
};

} // namespace libiop

#include "libiop/protocols/ldt/fri/fri_ldt.tcc"

#endif // LIBIOP_PROTOCOLS_LDT_FRI_FRI_LDT_HPP_
