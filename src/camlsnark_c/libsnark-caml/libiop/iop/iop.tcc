#include <algorithm>
#include <cstdlib>
#include <stdexcept>
#include <iostream>
#include <numeric>

namespace libiop {

template<typename FieldT>
domain_handle iop_protocol<FieldT>::register_subspace(const affine_subspace<FieldT> &S)
{
    field_subset<FieldT> wrapper(S);
    return this->register_domain(wrapper);
}

template<typename FieldT>
domain_handle iop_protocol<FieldT>::register_coset(const multiplicative_coset<FieldT> &S)
{
    field_subset<FieldT> wrapper(S);
    return this->register_domain(wrapper);
}

template<typename FieldT>
domain_handle iop_protocol<FieldT>::register_domain(const field_subset<FieldT> &domain)
{
    if (this->registration_state_ != registration_state_interactive)
    {
        throw std::logic_error("attempted to register a domain after "
                               "interactive registrations sealed");
    }

    this->domains_.emplace_back(domain);
    return domain_handle(this->domains_.size()-1);
}


template<typename FieldT>
void iop_protocol<FieldT>::update_rounds_and_direction(const iop_message_direction new_message_direction)
{
    if (this->message_direction_ == new_message_direction)
    {
        /* nothing to be done */
        return;
    }

    if (this->message_direction_ == direction_from_prover)
    {
        this->num_oracles_at_end_of_round_.emplace_back(
            this->oracle_registrations_.size());
        this->num_prover_messages_at_end_of_round_.emplace_back(
            this->prover_message_registrations_.size());

        /* we define round as verifier/prover message combo, so the
           new message from the verifier marks a new round */
        this->num_interaction_rounds_ += 1;
    }
    else
    {
        this->num_verifier_random_messages_at_end_of_round_.emplace_back(
            this->verifier_random_message_registrations_.size());
    }

    this->message_direction_ = new_message_direction;
}

template<typename FieldT>
void iop_protocol<FieldT>::assert_oracle_can_be_registered(
    const domain_handle &domain, const std::size_t degree)
{
    if (this->registration_state_ != registration_state_interactive)
    {
        throw std::logic_error("attempted to register an oracle after "
                               "interactive registrations sealed");
    }

    if (domain.id() >= this->domains_.size())
    {
        throw std::invalid_argument("domain not registered");
    }

    if (degree >= this->domains_[domain.id()].num_elements())
    {
        throw std::invalid_argument("attempting to register oracle whose degree exceeds domain size");
    }
}

template<typename FieldT>
oracle_handle iop_protocol<FieldT>::register_oracle(const domain_handle &domain, const std::size_t degree, const bool make_zk)
{
    this->assert_oracle_can_be_registered(domain, degree);
    this->update_rounds_and_direction(direction_from_prover);
    if (this->is_holographic_ && this->num_interaction_rounds_ == 0)
    {
        throw std::invalid_argument(
            "Cannot register non-index oracles in round 0 of a holographic IOP");
    }

    oracle_registration registration(domain, degree, make_zk);
    this->oracle_registrations_.emplace_back(std::move(registration));
    this->oracles_.emplace_back(oracle<FieldT>()); /* prepare an empty slot */
    this->oracles_present_.push_back(false);
    this->next_oracle_uid_ += 1;

    return oracle_handle(this->oracle_registrations_.size()-1, this->next_oracle_uid_ - 1);
}

template<typename FieldT>
oracle_handle iop_protocol<FieldT>::register_index_oracle(const domain_handle &domain, const std::size_t degree)
{
    if (this->num_prover_rounds_done_ != 0)
    {
        throw std::invalid_argument("index oracles must be created in the 0th round");
    }
    this->update_rounds_and_direction(direction_from_prover);
    this->is_holographic_ = true;
    const bool make_zk = false;
    const bool indexed = true;

    oracle_registration registration(domain, degree, make_zk, indexed);
    this->oracle_registrations_.emplace_back(std::move(registration));
    this->oracles_.emplace_back(oracle<FieldT>()); /* prepare an empty slot */
    this->oracles_present_.push_back(false);
    this->next_oracle_uid_ += 1;

    return oracle_handle(this->oracle_registrations_.size()-1, this->next_oracle_uid_ - 1);
}

template<typename FieldT>
virtual_oracle_handle iop_protocol<FieldT>::register_virtual_oracle(
        const domain_handle &domain,
        const std::size_t degree,
        const std::vector<oracle_handle_ptr> &constituent_oracles,
        std::shared_ptr<virtual_oracle<FieldT> > contents,
        const bool cache_evaluated_contents)
{
    this->assert_oracle_can_be_registered(domain, degree);

    virtual_oracle_registration registration(domain,
                                             degree,
                                             constituent_oracles);
    this->virtual_oracle_registrations_.emplace_back(std::move(registration));
    this->virtual_oracles_.emplace_back(contents);
    this->virtual_oracle_evaluation_cache_.emplace_back(std::map<std::size_t, FieldT>());
    this->virtual_oracle_should_cache_evaluated_contents_.push_back(cache_evaluated_contents);
    this->next_oracle_uid_ += 1;

    return virtual_oracle_handle(
        this->virtual_oracle_registrations_.size()-1, this->next_oracle_uid_ - 1);
}

template<typename FieldT>
prover_message_handle iop_protocol<FieldT>::register_prover_message(const std::size_t size)
{
    if (this->registration_state_ != registration_state_interactive)
    {
        throw std::logic_error("attempted to register a prover message after "
                               "interactive registrations sealed");
    }

    this->update_rounds_and_direction(direction_from_prover);

    prover_message_registration registration(size);
    this->prover_message_registrations_.emplace_back(std::move(registration));
    this->prover_messages_.emplace_back(std::vector<FieldT>()); /* prepare an empty slot */
    this->prover_messages_present_.push_back(false);

    return prover_message_handle(this->prover_message_registrations_.size()-1);
}

template<typename FieldT>
verifier_random_message_handle iop_protocol<FieldT>::register_verifier_random_message(const std::size_t size)
{
    if (this->registration_state_ != registration_state_interactive)
    {
        throw std::logic_error("attempted to register a verifier random message after "
                               "interactive registrations sealed");
    }

    this->update_rounds_and_direction(direction_from_verifier);

    verifier_random_message_registration registration(size);
    this->verifier_random_message_registrations_.emplace_back(std::move(registration));

    return verifier_random_message_handle(this->verifier_random_message_registrations_.size()-1);
}

template<typename FieldT>
random_query_position_handle iop_protocol<FieldT>::register_random_query_position(const domain_handle &domain)
{
    if (this->registration_state_ != registration_state_query)
    {
        throw std::logic_error("attempted to register a random query position while not in query registration state");
    }

    random_query_position_registration registration(domain);
    this->random_query_position_registrations_.emplace_back(std::move(registration));
    return random_query_position_handle(this->random_query_position_registrations_.size()-1);
}

template<typename FieldT>
deterministic_query_position_handle iop_protocol<FieldT>::register_deterministic_query_position(
    const std::vector<query_position_handle> &seed_positions,
    const deterministic_position_calculator &position_calculator)
{
    if (this->registration_state_ != registration_state_query)
    {
        throw std::logic_error("attempted to register a deterministic query position while not in query registration state");
    }

    deterministic_query_position_registration registration(seed_positions, position_calculator);
    this->deterministic_query_position_registrations_.emplace_back(std::move(registration));
    return deterministic_query_position_handle(this->deterministic_query_position_registrations_.size()-1);
}

template<typename FieldT>
query_handle iop_protocol<FieldT>::register_query(const oracle_handle_ptr &oracle,
                                                  const query_position_handle &query_position)
{
    if (this->registration_state_ != registration_state_query)
    {
        throw std::logic_error("attempted to register a query while not in query registration state");
    }
    query_registration registration(oracle, query_position);
    this->query_registrations_.emplace_back(std::move(registration));
    return query_handle(this->query_registrations_.size()-1);
}

template<typename FieldT>
void iop_protocol<FieldT>::seal_interaction_registrations()
{
    if (this->registration_state_ != registration_state_interactive)
    {
        throw std::logic_error("attempted to seal interaction registrations "
                               "while not in interaction registration state");
    }

    if (this->message_direction_ == direction_from_verifier)
    {
        throw std::logic_error("attempted to seal interaction registrations "
                               "where verifier sends the last interactive message");
    }

    /* num_verifier_random_messages_at_end_of_round_ have been updated
       by submitting a prover message/oracle. we update the
       corresponding data structures for the prover here */
    this->num_oracles_at_end_of_round_.emplace_back(this->oracle_registrations_.size());
    this->num_prover_messages_at_end_of_round_.emplace_back(this->prover_message_registrations_.size());
    ++(this->num_interaction_rounds_);

    this->registration_state_ = registration_state_query;
    return;
}

template<typename FieldT>
void iop_protocol<FieldT>::seal_query_registrations()
{
    if (this->registration_state_ != registration_state_query)
    {
        throw std::logic_error("attempted to seal query registrations "
                               "while not in query registration state");
    }
    this->registration_state_ = registration_state_done;
    return;
}

template<typename FieldT>
const oracle<FieldT>& iop_protocol<FieldT>::submit_oracle(const oracle_handle_ptr &handle, oracle<FieldT> &&contents)
{
    const oracle_handle &new_handle = static_cast<oracle_handle&>(*handle.get());
    return this->submit_oracle(new_handle, std::move(contents));
}

template<typename FieldT>
const oracle<FieldT>& iop_protocol<FieldT>::submit_oracle(const oracle_handle &handle, oracle<FieldT> &&contents)
{
    if (this->registration_state_ != registration_state_done)
    {
        throw std::logic_error("attempted to submit oracle without finishing all registrations");
    }

    if (this->oracles_present_[handle.id()])
    {
        throw std::invalid_argument("attempted to submit already submitted oracle");
    }

    const std::size_t oracle_id_begin = (this->num_prover_rounds_done_ == 0 ? 0 :
                                         this->num_oracles_at_end_of_round_[this->num_prover_rounds_done_ - 1]);
    if (handle.id() < oracle_id_begin)
    {
        throw std::invalid_argument("submitting an oracle for a previous round (should NOT happen: signal_prover_round_done should have already caught an incomplete prior round)");
    }

    if (handle.id() >= this->num_oracles_at_end_of_round_[this->num_prover_rounds_done_])
    {
        throw std::invalid_argument("submitting an oracle for a future round (did you forget to call signal_prover_round_done?)");
    }

    if (this->domains_[this->oracle_registrations_[handle.id()].domain().id()].num_elements() !=
        contents.evaluated_contents()->size())
    {
        throw std::invalid_argument("oracle evaluations don't match the domain size");
    }

    this->oracles_[handle.id()] = contents;
    this->oracles_present_[handle.id()] = true;

    return (this->oracles_[handle.id()]);
}

template<typename FieldT>
void iop_protocol<FieldT>::submit_prover_index(iop_prover_index<FieldT> &index)
{
    if (this->num_prover_rounds_done_ != 0)
    {
        throw std::invalid_argument("The IOP prover index should only be for round 0");
    }
    const std::size_t oracle_id_begin = 0;
    const std::size_t oracle_id_end = this->num_oracles_at_end_of_round_[0];
    if (index.all_oracle_evals_.size() != oracle_id_end - oracle_id_begin)
    {
        printf("Got: %lu\nExpected: %lu\n",
            index.all_oracle_evals_.size(), oracle_id_end - oracle_id_begin);
        throw std::invalid_argument("The IOP prover index provided the wrong number of evaluations");
    }

    for (size_t i = oracle_id_begin; i < oracle_id_end; i++)
    {
        oracle_handle ith_handle(i);
        this->submit_oracle(ith_handle, oracle<FieldT>(std::move(index.all_oracle_evals_[i])));
        std::vector<FieldT>().swap(index.all_oracle_evals_[i]);
    }

    const size_t prover_message_id_begin = 0;
    const size_t prover_message_id_end = this->num_prover_messages_at_end_of_round_[0];
    for (size_t i = prover_message_id_begin; i < prover_message_id_end; i++)
    {
        prover_message_handle ith_handle(i);
        std::vector<FieldT> prover_message = index.prover_messages_[i];
        this->submit_prover_message(ith_handle, std::move(prover_message));
    }
    this->signal_index_submissions_done();
}

template<typename FieldT>
void iop_protocol<FieldT>::submit_prover_message(const prover_message_handle &handle, std::vector<FieldT> &&contents)
{
    if (this->registration_state_ != registration_state_done)
    {
        throw std::logic_error("attempted to submit prover message without finishing all registrations");
    }

    if (this->prover_messages_present_[handle.id()])
    {
        throw std::invalid_argument("attempted to submit already submitted prover message");
    }

    const std::size_t message_id_begin = (this->num_prover_rounds_done_ == 0 ? 0 :
                                          this->num_prover_messages_at_end_of_round_[this->num_prover_rounds_done_ - 1]);
    if (handle.id() < message_id_begin)
    {
        throw std::invalid_argument("submitting a prover message for a previous round (should NOT happen: signal_prover_round_done should have already caught an incomplete prior round)");
    }

    if (handle.id() >= this->num_prover_messages_at_end_of_round_[this->num_prover_rounds_done_])
    {
        throw std::invalid_argument("submitting a prover message for a future round (did you forget to call signal_prover_round_done?)");
    }

    if (this->prover_message_registrations_[handle.id()].size() != contents.size())
    {
        throw std::invalid_argument("prover message submission does not match its registered size");
    }

    this->prover_messages_[handle.id()] = contents;
    this->prover_messages_present_[handle.id()] = true;
}

template<typename FieldT>
void iop_protocol<FieldT>::signal_index_registrations_done()
{
    if (!this->is_holographic_ || this->num_interaction_rounds_ != 0)
    {
        throw std::invalid_argument("Should only be used to end round 0 of a holographic IOP");
    }
    this->update_rounds_and_direction(direction_from_verifier);
    assert(this->num_interaction_rounds_ == 1);
}

template<typename FieldT>
void iop_protocol<FieldT>::signal_index_submissions_done()
{
    this->signal_prover_round_done();
}

template<typename FieldT>
void iop_protocol<FieldT>::signal_prover_round_done()
{
    if (this->registration_state_ != registration_state_done)
    {
        throw std::logic_error("attempted to signal prover round done without finishing all registrations");
    }

    if (this->num_prover_rounds_done_ >= this->num_interaction_rounds_)
    {
        throw std::logic_error("attempting to signal end of a round after protocol already finished");
    }

    const std::size_t oracle_id_begin = (this->num_prover_rounds_done_ == 0 ? 0 :
                                         this->num_oracles_at_end_of_round_[this->num_prover_rounds_done_ - 1]);
    const std::size_t oracle_id_end = this->num_oracles_at_end_of_round_[this->num_prover_rounds_done_];

    for (std::size_t id = oracle_id_begin; id < oracle_id_end; ++id)
    {
        if (!(this->oracles_present_[id]))
        {
            throw std::logic_error("signaling end of round without submitting all oracles in the round");
        }
    }

    const std::size_t message_id_begin = (this->num_prover_rounds_done_ == 0 ? 0 :
                                          this->num_prover_messages_at_end_of_round_[this->num_prover_rounds_done_ - 1]);
    const std::size_t message_id_end = this->num_prover_messages_at_end_of_round_[this->num_prover_rounds_done_];

    for (std::size_t id = message_id_begin; id < message_id_end; ++id)
    {
        if (!(this->prover_messages_present_[id]))
        {
            throw std::logic_error("signaling end of round without submitting all prover messages in the round");
        }
    }

    /* all sanity checks pass, advancing the round */
    ++(this->num_prover_rounds_done_);
}

template<typename FieldT>
std::vector<FieldT> iop_protocol<FieldT>::obtain_verifier_random_message(const verifier_random_message_handle &random_message)
{
    if (this->registration_state_ != registration_state_done)
    {
        throw std::logic_error("attempted to obtain verifier random message without finishing all registrations");
    }

    const std::size_t message_id_end =
        this->num_verifier_random_messages_at_end_of_round_[std::min(this->num_prover_rounds_done_,
                                                                     this->num_interaction_rounds_ - 1)];

    if (random_message.id() > message_id_end)
    {
        throw std::invalid_argument("attempted to obtain a verifier random message for a further round (did you forget to call signal_prover_round_done?)");
    }

    auto it = this->verifier_random_messages_.find(random_message.id());
    if (it == this->verifier_random_messages_.end())
    {
        const std::size_t message_length = this->verifier_random_message_registrations_[random_message.id()].size();
        const std::vector<FieldT> result = random_FieldT_vector<FieldT>(message_length);

        this->verifier_random_messages_[random_message.id()] = result;

        return result;
    }
    else
    {
        return it->second;
    }
}

template<typename FieldT>
std::size_t iop_protocol<FieldT>::num_interaction_rounds() const
{
    return this->num_interaction_rounds_;
}

template<typename FieldT>
iop_registration_state iop_protocol<FieldT>::registration_state() const
{
    return this->registration_state_;
}

template<typename FieldT>
std::size_t iop_protocol<FieldT>::obtain_query_position(const query_position_handle &position)
{
#if 0 // TODO: this function is also used in BCS verifier where the check shouldn't trigger; so we just disable it for now...
    if (this->num_prover_rounds_done_ != this->num_interaction_rounds_)
    {
        throw std::logic_error("attempting to obtain query position while prover interactions in progress (did you forget to call signal_prover_round_done?)");
    }

#endif
    if (position.type() == random_query_type)
    {
        auto it = this->random_query_positions_.find(position.id());
        if (it == this->random_query_positions_.end())
        {
            const random_query_position_handle random_position(position.id());
            const std::size_t result = this->obtain_random_query_position(random_position);
            this->random_query_positions_[position.id()] = result;
            return result;
        }
        else
        {
            return it->second;
        }
    }
    else if (position.type() == deterministic_query_type)
    {
        auto it = this->deterministic_query_positions_.find(position.id());
        if (it == this->deterministic_query_positions_.end())
        {
            const deterministic_query_position_registration& reg =
                this->deterministic_query_position_registrations_[position.id()];

            std::vector<std::size_t> seed_position_values;
            for (query_position_handle &seed_handle : reg.seed_positions())
            {
                seed_position_values.emplace_back(this->obtain_query_position(seed_handle));
            }

            const std::size_t result = reg.position_calculator()(seed_position_values);

            this->deterministic_query_positions_[position.id()] = result;
            return result;
        }
        else
        {
            return it->second;
        }
    }
    else
    {
        throw std::invalid_argument("query position type not supported");
    }
}

template<typename FieldT>
FieldT iop_protocol<FieldT>::obtain_query_response(const query_handle &query)
{
#if 0 // TODO: this function is also used in BCS verifier where the check shouldn't trigger;
      // so we just disable here and duplicate in bcs prover for now...
    if (this->num_prover_rounds_done_ != this->num_interaction_rounds_)
    {
        throw std::logic_error("attempting to obtain query response while prover interactions in progress (did you forget to call signal_prover_round_done?)");
    }
#endif

    auto it = this->query_responses_.find(query.id());
    if (it == this->query_responses_.end())
    {
        const oracle_handle_ptr oracle_h =
            this->query_registrations_[query.id()].oracle();

        const query_position_handle query_position =
            this->query_registrations_[query.id()].query_position();
        const std::size_t position_idx = this->obtain_query_position(query_position);

        const FieldT result = this->get_oracle_evaluation_at_point(oracle_h, position_idx, true);

        this->query_responses_[query.id()] = result;
        return result;
    }
    else
    {
        return it->second;
    }
}

template<typename FieldT>
std::vector<FieldT> iop_protocol<FieldT>::receive_prover_message(const prover_message_handle &message)
{
    if (this->num_prover_rounds_done_ != this->num_interaction_rounds_)
    {
        throw std::logic_error("attempting to receive prover message while interactions in progress (did you forget to call signal_prover_round_done?)");
    }

    if (!this->prover_messages_present_[message.id()])
    {
        throw std::logic_error("prover message not submitted (should have been caught by signal_prover_round_done checks");
    }
    else
    {
        return this->prover_messages_[message.id()];
    }
}

template<typename FieldT>
void iop_protocol<FieldT>::set_round_parameters(const round_parameters<FieldT> &params) {
    libiop::UNUSED(params);
}

template<typename FieldT>
field_subset<FieldT> iop_protocol<FieldT>::get_domain(const domain_handle &handle) const
{
    return this->domains_[handle.id()];
}

template<typename FieldT>
std::size_t iop_protocol<FieldT>::get_oracle_degree(const oracle_handle_ptr &handle) const
{
    if (std::dynamic_pointer_cast<oracle_handle>(handle))
    {
        return this->oracle_registrations_[handle->id()].degree();
    }
    else if (std::dynamic_pointer_cast<virtual_oracle_handle>(handle))
    {
        return this->virtual_oracle_registrations_[handle->id()].degree();
    }
    else
    {
        throw std::invalid_argument("oracle type not supported");
    }
}

template<typename FieldT>
domain_handle iop_protocol<FieldT>::get_oracle_domain(const oracle_handle_ptr &handle) const
{
    if (std::dynamic_pointer_cast<oracle_handle>(handle))
    {
        return this->oracle_registrations_[handle->id()].domain();
    }
    else if (std::dynamic_pointer_cast<virtual_oracle_handle>(handle))
    {
        return this->virtual_oracle_registrations_[handle->id()].domain();
    }
    else
    {
        throw std::invalid_argument("oracle type not supported");
    }
}

template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> iop_protocol<FieldT>::get_oracle_evaluations(const oracle_handle_ptr &handle)
{
    if (std::dynamic_pointer_cast<oracle_handle>(handle))
    {
        return this->oracles_[handle->id()].evaluated_contents();
    }
    else if (std::dynamic_pointer_cast<virtual_oracle_handle>(handle))
    {
        /** If virtual oracle has caching enabled, and is cached, use the cache. */
        if (this->virtual_oracle_should_cache_evaluated_contents_[handle->id()] &&
            this->virtual_oracle_evaluated_contents_cache_.count(handle->id()) == 1)
        {
            return this->virtual_oracle_evaluated_contents_cache_[handle->id()];
        }
        const virtual_oracle_registration& reg =
            this->virtual_oracle_registrations_[handle->id()];

        std::vector<std::shared_ptr<std::vector<FieldT>> > constituent_evaluations;
        for (auto &constituent_handle : reg.constituent_oracles())
        {
            constituent_evaluations.emplace_back(this->get_oracle_evaluations(constituent_handle));
        }

        const std::shared_ptr<std::vector<FieldT>> result = this->virtual_oracles_[handle->id()]->evaluated_contents(constituent_evaluations);

        if (this->virtual_oracle_should_cache_evaluated_contents_[handle->id()])
        {
            return this->virtual_oracle_evaluated_contents_cache_[handle->id()] = result;
        }

        return result;
    }
    else
    {
        throw std::invalid_argument("oracle type not supported");
    }
}

template<typename FieldT>
FieldT iop_protocol<FieldT>::get_oracle_evaluation_at_point(const oracle_handle_ptr &handle,
                                                            const std::size_t evaluation_position,
                                                            const bool record)
{
    if (std::dynamic_pointer_cast<oracle_handle>(handle))
    {
        field_subset<FieldT> domain = this->domains_[this->oracle_registrations_[handle->id()].domain().id()];
        if (domain.num_elements() <= evaluation_position) {
            throw std::invalid_argument("evaluation position is outside of domain");
        }

        if (record)
        {
            /* HACK */
            this->oracle_id_to_query_positions_[handle->id()].insert(evaluation_position);
        }

        return this->oracles_[handle->id()].evaluated_contents()->operator[](evaluation_position);
    }
    else if (std::dynamic_pointer_cast<virtual_oracle_handle>(handle))
    {
        std::map<std::size_t, FieldT> &evaluation_cache = this->virtual_oracle_evaluation_cache_[handle->id()];
        if(evaluation_cache.count(evaluation_position) == 1) {
            return evaluation_cache[evaluation_position];
        }
        const virtual_oracle_registration& reg =
            this->virtual_oracle_registrations_[handle->id()];

        std::vector<FieldT> constituent_evaluations;
        for (auto &constituent_handle : reg.constituent_oracles())
        {
            constituent_evaluations.emplace_back(this->get_oracle_evaluation_at_point(constituent_handle, evaluation_position, record));
        }

        const FieldT evaluation_point = this->get_domain(reg.domain()).element_by_index(evaluation_position);

        const FieldT result = this->virtual_oracles_[handle->id()]->evaluation_at_point(evaluation_position, evaluation_point, constituent_evaluations);
        evaluation_cache[evaluation_position] = result;
        return result;
    }
    else
    {
        throw std::invalid_argument("oracle type not supported");
    }
}

template<typename FieldT>
std::size_t iop_protocol<FieldT>::num_symbols_across_all_oracles() const
{
    std::size_t oracle_length = 0;

    for (auto &registration : this->oracle_registrations_)
    {
        oracle_length  += this->get_domain(registration.domain()).num_elements();
    }

    return oracle_length;
}

template<typename FieldT>
std::size_t iop_protocol<FieldT>::num_bytes_across_all_oracles() const
{
    return sizeof(FieldT) * (this->num_symbols_across_all_oracles());
}

template<typename FieldT>
std::size_t iop_protocol<FieldT>::size_in_bytes() const
{
    const std::size_t prover_messages_length =
        std::accumulate(this->prover_messages_.begin(),
                        this->prover_messages_.end(),
                        0,
                        [] (const std::size_t av,
                            const std::vector<FieldT> &msg) {
                            return av + msg.size();
                        });
    const std::size_t prover_messages_size =
        sizeof(FieldT) * prover_messages_length;

    const std::size_t query_responses_length = this->query_responses_.size();
    const std::size_t query_responses_size =
        sizeof(FieldT) * query_responses_length;

    return prover_messages_size + query_responses_size;
}

template<typename FieldT>
std::size_t iop_protocol<FieldT>::obtain_random_query_position(const random_query_position_handle &position)
{
    const std::size_t domain_size = this->domains_[
        this->random_query_position_registrations_[position.id()].domain().id()].num_elements();
    const std::size_t result = std::rand() % domain_size;
    return result;
}

template<typename FieldT>
std::size_t iop_protocol<FieldT>::min_oracle_id(const std::size_t round) const
{
    if (round >= this->num_interaction_rounds_)
    {
        throw std::invalid_argument("Specified round does not exist.");
    }

    return (round == 0 ? 0 :
            this->num_oracles_at_end_of_round_[round - 1]);
}

template<typename FieldT>
std::size_t iop_protocol<FieldT>::max_oracle_id(const std::size_t round) const
{
    if (round >= this->num_interaction_rounds_)
    {
        throw std::invalid_argument("Specified round does not exist.");
    }

    return this->num_oracles_at_end_of_round_[round];
}

template<typename FieldT>
domain_to_oracles_map iop_protocol<FieldT>::oracles_in_round(const std::size_t round) const
{
    if (round >= this->num_interaction_rounds_)
    {
        throw std::invalid_argument("Specified round does not exist.");
    }

    domain_to_oracles_map mapping;

    const std::size_t oracle_id_begin = this->min_oracle_id(round);
    const std::size_t oracle_id_end = this->max_oracle_id(round);
    for (std::size_t oracle_id = oracle_id_begin; oracle_id < oracle_id_end; ++oracle_id)
    {
        const domain_handle domain = this->oracle_registrations_[oracle_id].domain();
        mapping[domain].push_back(oracle_handle(oracle_id));
    }

    return mapping;
}

} // namespace libiop
