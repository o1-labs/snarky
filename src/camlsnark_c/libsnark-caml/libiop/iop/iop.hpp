/**@file
*****************************************************************************
IOP interfaces.
*****************************************************************************
* @author     This file is part of libiop (see AUTHORS)
* @copyright  MIT license (see LICENSE file)
*****************************************************************************/
#ifndef LIBIOP_IOP_IOP_HPP_
#define LIBIOP_IOP_IOP_HPP_

#include <cstddef>
#include <functional>
#include <map>
#include <memory>
#include <set>
#include <vector>

#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/iop/oracles.hpp"

namespace libiop {

/* Handles for oracles, messages, queries */

enum iop_handle_type {
    domain_handle_type = 1,
    prover_message_type = 2,
    verifier_random_message_type = 3,

    query_type = 4,
};

template<iop_handle_type type>
class handle {
protected:
    std::size_t id_;

public:
    explicit handle() : id_(0) {}
    explicit handle(const std::size_t id) : id_(id) {}
    handle(const handle<type> &other) = default;
    std::size_t id() const { return this->id_; }

    virtual ~handle() = default;
};

typedef handle<domain_handle_type> domain_handle;
typedef handle<prover_message_type> prover_message_handle;
typedef handle<verifier_random_message_type> verifier_random_message_handle;

/* TODO: figure out how to do this properly */

enum query_position_type {
    random_query_type = 1,
    deterministic_query_type = 2
};

class query_position_handle {
protected:
    /** Query position id's are a sequence per query type.
     *  If there are deterministic query types used, then there will be
     *  be an id 0 deterministic query position, and id 0 random query position.
     */
    std::size_t id_;
    query_position_type query_type_;

public:
    explicit query_position_handle() :
        id_(0) {}
    explicit query_position_handle(const query_position_type query_type) :
        id_(0), query_type_(query_type) {}
    explicit query_position_handle(const std::size_t id, const query_position_type query_type) :
        id_(id), query_type_(query_type) {}
    query_position_handle(const query_position_handle &other) = default;
    std::size_t id() const { return this->id_; }
    query_position_type type() const { return this->query_type_; }

    virtual ~query_position_handle() = default;
};

class random_query_position_handle : public query_position_handle {
public:
    explicit random_query_position_handle() : query_position_handle(random_query_type) {};
    explicit random_query_position_handle(const std::size_t id) : query_position_handle(id, random_query_type) {}

    ~random_query_position_handle() = default;
};

class deterministic_query_position_handle : public query_position_handle {
public:
    explicit deterministic_query_position_handle() : query_position_handle(deterministic_query_type) {};
    explicit deterministic_query_position_handle(const std::size_t id) : query_position_handle(id, deterministic_query_type) {}

    ~deterministic_query_position_handle() = default;
};

/* TODO: same */

class oracle_handle_base {
protected:
    std::size_t id_;
    size_t uid_;

public:
    explicit oracle_handle_base() : id_(0), uid_(0) {}
    explicit oracle_handle_base(const std::size_t id) : id_(id), uid_(0) {}
    explicit oracle_handle_base(const size_t id, const size_t uid) : id_(id), uid_(uid) {}
    oracle_handle_base(const oracle_handle_base &other) = default;
    std::size_t id() const { return this->id_; }
    std::size_t uid() const { return this->uid_; }

    virtual ~oracle_handle_base() {}
};

class oracle_handle : public oracle_handle_base {
public:
    explicit oracle_handle() = default;
    explicit oracle_handle(const std::size_t id) : oracle_handle_base(id) {}
    explicit oracle_handle(const std::size_t id, const std::size_t uid) :
        oracle_handle_base(id, uid) {}

    ~oracle_handle() = default;
};

class virtual_oracle_handle : public oracle_handle_base {
public:
    explicit virtual_oracle_handle() = default;
    explicit virtual_oracle_handle(const std::size_t id) : oracle_handle_base(id) {}
    explicit virtual_oracle_handle(const std::size_t id, const std::size_t uid) :
        oracle_handle_base(id, uid) {}

    ~virtual_oracle_handle() = default;
};

typedef std::shared_ptr<oracle_handle_base> oracle_handle_ptr;

typedef handle<query_type> query_handle;

/* IOP protocol */

/* We model the protocols as a client-server architecture where
   iop_protocol acts as a mediator between the prover and the
   verifier. The proving/verification process can essentially be
   viewed as "RPC-in-the-head" with such mediator. */

class oracle_registration {
protected:
    domain_handle domain_;
    std::size_t degree_;
    bool make_zk_;
    /** Indicates if this oracle should be pre-processed.
     *  If so, its commitment should not appear in the transcript,
     *  as the verifier knows it. */
    bool indexed_;
public:
    explicit oracle_registration(const domain_handle &domain,
                                 const std::size_t degree,
                                 const bool make_zk) :
        domain_(domain), degree_(degree), make_zk_(make_zk), indexed_(false)
    {
    }

    explicit oracle_registration(const domain_handle &domain,
                                 const std::size_t degree,
                                 const bool make_zk,
                                 const bool indexed) :
        domain_(domain), degree_(degree), make_zk_(make_zk), indexed_(indexed)
    {
    }

    domain_handle domain() const { return this->domain_; }
    std::size_t degree() const { return this->degree_; }
    bool make_zk() const { return this->make_zk_; }
    bool indexed() const { return this->indexed_; }
};

class virtual_oracle_registration {
protected:
    domain_handle domain_;
    std::size_t degree_;
    std::vector<oracle_handle_ptr> constituent_oracles_;
public:
    explicit virtual_oracle_registration(const domain_handle &domain,
                                         const std::size_t degree,
                                         const std::vector<oracle_handle_ptr> &constituent_oracles) :
        domain_(domain),
        degree_(degree),
        constituent_oracles_(constituent_oracles)
    {
    }

    domain_handle domain() const { return this->domain_; }
    std::size_t degree() const { return this->degree_; }
    const std::vector<oracle_handle_ptr>& constituent_oracles() const { return this->constituent_oracles_; }
};

class prover_message_registration {
protected:
    std::size_t size_;
public:
    explicit prover_message_registration(const std::size_t size) : size_(size) {}

    std::size_t size() const { return this->size_; }
};

class verifier_random_message_registration {
protected:
    std::size_t size_;
public:
    explicit verifier_random_message_registration(const std::size_t size) : size_(size) {}

    std::size_t size() const { return this->size_; }
};

class random_query_position_registration {
protected:
    domain_handle domain_;
public:
    explicit random_query_position_registration(const domain_handle &domain) : domain_(domain) {}

    domain_handle domain() const { return this->domain_; }
};

typedef std::function<std::size_t(const std::vector<std::size_t>&)> deterministic_position_calculator;

class deterministic_query_position_registration {
protected:
    std::vector<query_position_handle> seed_positions_;
    deterministic_position_calculator position_calculator_;
public:
    explicit deterministic_query_position_registration(
        const std::vector<query_position_handle> &seed_positions,
        const deterministic_position_calculator &position_calculator) :
        seed_positions_(seed_positions),
        position_calculator_(position_calculator) {}

    std::vector<query_position_handle> seed_positions() const { return this->seed_positions_; };
    deterministic_position_calculator position_calculator() const { return this->position_calculator_; };
};

class query_registration {
protected:
    oracle_handle_ptr oracle_;
    const query_position_handle query_position_;
public:
    explicit query_registration(const oracle_handle_ptr &oracle,
                                const query_position_handle &query_position) :
        oracle_(oracle), query_position_(query_position) {}

    oracle_handle_ptr oracle() const { return this->oracle_; }
    const query_position_handle query_position() const { return this->query_position_; }
};

/* Parameters for each round */
template<typename FieldT>
class round_parameters {
public:
    /** BCS 16 specific optimization.
     *  Let H be the domain the codewords lie in, and Q be some subgroup of H.
     *  If you have the property that if the verifier queries position x,
     *  it also queries every element in the equivalence class of x in H / Q,
     *  then there is a proof size optimization that can be taken.
     *  Every leaf in the merkle tree which the prover constructs can contain every element in this query.
     *  Since the actual number of merkle tree leaves queried will then reduce by a factor of |Q|,
     *  the total number of zk leaf hashes required has been reduced by a factor of |Q|.
     *  (In the BCS transformation, every merkle tree leaf has a hash for zk properties appended to it)
     *
     *  In the additive case, this optimization is only supported when the basis vectors for Q are the
     *  same as the log_2(|Q|) first basis vectors of H.
     *  Because of this, we don't need to store the domain, only the size and its type.
     */
    const std::size_t quotient_map_size_ = 1;
    const field_subset_type quotient_map_type_;
    explicit round_parameters() :
        quotient_map_type_(field_subset<FieldT>(4).type()) {};
    explicit round_parameters(field_subset<FieldT> query_coset_domain) :
        quotient_map_size_(query_coset_domain.num_elements()),
        quotient_map_type_(query_coset_domain.type()) {};
};

/** The IOP prover index contains the evaluations for all of the index oracles.
 *  The evaluations are in the order in which the oracles were registered.
*/
template<typename FieldT>
struct iop_prover_index
{
    std::vector<std::vector<FieldT>> all_oracle_evals_;
    std::vector<std::vector<FieldT>> prover_messages_;
};

/* IOP protocol */
enum iop_message_direction {
    direction_from_verifier = 1,
    direction_from_prover = 2
};

enum iop_registration_state {
    registration_state_interactive = 1,
    registration_state_query = 2,
    registration_state_done = 3
};

struct domain_handle_comparator {
    bool operator()(const domain_handle &lhs,
                    const domain_handle &rhs) const
    {
        return lhs.id() < rhs.id();
    }
};

typedef std::map<domain_handle, std::vector<oracle_handle>, domain_handle_comparator> domain_to_oracles_map;

template<typename FieldT>
class iop_protocol {
protected:
    std::vector<field_subset<FieldT>> domains_;

    std::vector<oracle_registration> oracle_registrations_;
    std::vector<virtual_oracle_registration> virtual_oracle_registrations_;
    std::size_t next_oracle_uid_ = 1;
    std::vector<prover_message_registration> prover_message_registrations_;
    std::vector<verifier_random_message_registration> verifier_random_message_registrations_;
    std::vector<random_query_position_registration> random_query_position_registrations_;
    std::vector<deterministic_query_position_registration> deterministic_query_position_registrations_;
    std::vector<query_registration> query_registrations_;

    std::vector<std::map<std::size_t, FieldT> > virtual_oracle_evaluation_cache_;
    std::vector<bool> virtual_oracle_should_cache_evaluated_contents_; // TODO: Is there a better name for this

    std::map<std::size_t, std::size_t> random_query_positions_;
    std::map<std::size_t, std::size_t> deterministic_query_positions_;

    std::map<std::size_t, FieldT> query_responses_;
    std::map<std::size_t, std::vector<FieldT> > verifier_random_messages_;
    /* This cache doesn't clear since it is used within the multi_ldt,
     * which is at the end of the protocols */
    std::map<std::size_t, std::shared_ptr<std::vector<FieldT>> > virtual_oracle_evaluated_contents_cache_;

    /* just like in the IOP paper the verifier goes first */
    iop_message_direction message_direction_ = direction_from_verifier;
    std::size_t num_interaction_rounds_ = 0;
    iop_registration_state registration_state_ = registration_state_interactive;

    std::vector<std::size_t> num_oracles_at_end_of_round_;
    std::vector<std::size_t> num_prover_messages_at_end_of_round_;
    std::vector<std::size_t> num_verifier_random_messages_at_end_of_round_;

    void update_rounds_and_direction(const iop_message_direction new_message_direction);

    std::vector<oracle<FieldT> > oracles_;
    std::vector<std::shared_ptr<virtual_oracle<FieldT> > > virtual_oracles_;
    std::vector<std::vector<FieldT> > prover_messages_;

    /* TODO: consider if we want to just have std::shared_ptr above */
    std::vector<bool> oracles_present_;
    std::vector<bool> prover_messages_present_;
    std::size_t num_prover_rounds_done_ = 0;
    bool is_holographic_ = false;

    void assert_oracle_can_be_registered(
        const domain_handle &domain, const std::size_t degree);
public:
    iop_protocol() = default;

    domain_handle register_subspace(const affine_subspace<FieldT> &S);
    domain_handle register_coset(const multiplicative_coset<FieldT> &S);
    domain_handle register_domain(const field_subset<FieldT> &S);

    oracle_handle register_oracle(const domain_handle &domain,
                                  const std::size_t degree,
                                  const bool make_zk);
    oracle_handle register_index_oracle(const domain_handle &domain,
                                        const std::size_t degree);
    virtual_oracle_handle register_virtual_oracle(
        const domain_handle &domain,
        const std::size_t degree,
        const std::vector<oracle_handle_ptr> &constituent_oracles,
        std::shared_ptr<virtual_oracle<FieldT> > contents,
        const bool cache_evaluated_contents = false);
    prover_message_handle register_prover_message(const std::size_t size);
    verifier_random_message_handle register_verifier_random_message(const std::size_t size);
    random_query_position_handle register_random_query_position(const domain_handle &domain);
    /** It is the caller's responsibility to ensure there are no infinite cycles in the seed
     *  positions required. */
    deterministic_query_position_handle register_deterministic_query_position(
        const std::vector<query_position_handle> &seed_positions,
        const deterministic_position_calculator &position_calculator);
    query_handle register_query(const oracle_handle_ptr &oracle,
                                const query_position_handle &query_position);

    virtual void seal_interaction_registrations();
    virtual void seal_query_registrations();

    const oracle<FieldT>& submit_oracle(const oracle_handle_ptr &handle, oracle<FieldT> &&contents);
    const oracle<FieldT>& submit_oracle(const oracle_handle &handle, oracle<FieldT> &&contents);
    void submit_prover_message(const prover_message_handle &handle, std::vector<FieldT> &&contents);
    void submit_prover_index(iop_prover_index<FieldT> &index);
    void signal_index_registrations_done();
    virtual void signal_index_submissions_done();
    virtual void signal_prover_round_done();

    virtual std::vector<FieldT> obtain_verifier_random_message(const verifier_random_message_handle &random_message);
    std::size_t obtain_query_position(const query_position_handle &position);
    FieldT obtain_query_response(const query_handle &query);

    virtual std::vector<FieldT> receive_prover_message(const prover_message_handle &message);
    virtual void set_round_parameters(const round_parameters<FieldT> &params);

    std::size_t num_interaction_rounds() const;
    iop_registration_state registration_state() const;

    field_subset<FieldT> get_domain(const domain_handle &handle) const;

    std::size_t get_oracle_degree(const oracle_handle_ptr &handle) const;
    domain_handle get_oracle_domain(const oracle_handle_ptr &handle) const;
    std::shared_ptr<std::vector<FieldT>> get_oracle_evaluations(const oracle_handle_ptr &handle);
    virtual FieldT get_oracle_evaluation_at_point(
        const oracle_handle_ptr &handle,
        const std::size_t evaluation_position,
        const bool record=false);

    std::size_t num_symbols_across_all_oracles() const;
    std::size_t num_bytes_across_all_oracles() const;

    std::size_t size_in_bytes() const;
protected:
    virtual std::size_t obtain_random_query_position(const random_query_position_handle &position);
    std::size_t min_oracle_id(const std::size_t round) const;
    std::size_t max_oracle_id(const std::size_t round) const;
    domain_to_oracles_map oracles_in_round(const std::size_t round) const;

    std::map<std::size_t, std::set<std::size_t> > oracle_id_to_query_positions_; /* HACK */
};

} // namespace libiop

#include "libiop/iop/iop.tcc"

#endif // LIBIOP_IOP_IOP_HPP_
