namespace libiop {

template<typename FieldT>
single_boundary_constraint<FieldT>::single_boundary_constraint(
    const field_subset<FieldT> &codeword_domain) :
    codeword_domain_(codeword_domain)
{
}

template<typename FieldT>
void single_boundary_constraint<FieldT>::set_evaluation_point_and_eval(
    const FieldT eval_point, const FieldT oracle_eval)
{
    this->eval_point_ = eval_point;
    this->oracle_evaluation_ = oracle_eval;
}

/* Multiplies each oracle evaluation vector by the corresponding random coefficient */
template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> single_boundary_constraint<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const
{
    if (constituent_oracle_evaluations.size() != 1)
    {
        throw std::invalid_argument("Single Boundary Constraint: "
            "Expected exactly 1 constituent oracle.");
    }

    std::vector<FieldT> all_shifted_elems;
    const FieldT shift = -this->eval_point_;
    if (this->codeword_domain_.type() == affine_subspace_type)
    {
        /** Creates a subspace with the correct shift */
        field_subset<FieldT> shifted_subspace(this->codeword_domain_.num_elements(),
            this->codeword_domain_.offset() + shift);
        all_shifted_elems = shifted_subspace.all_elements();
    } else if (this->codeword_domain_.type() == multiplicative_coset_type)
    {
        /** Gets all elements of the codeword domain,
         *  since these are likely cached, and then shifts them */
        all_shifted_elems = this->codeword_domain_.all_elements();
        for (size_t i = 0; i < this->codeword_domain_.num_elements(); i++)
        {
            all_shifted_elems[i] += shift;
        }
    }
    std::vector<FieldT> all_inverted_shifted_elems = batch_inverse<FieldT>(all_shifted_elems);

    /** boundary constraint is (f - claimed_eval) / (x - eval_pt) */
    std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>();
    result->reserve(this->codeword_domain_.num_elements());
    for (std::size_t i = 0; i < this->codeword_domain_.num_elements(); ++i) {
        result->emplace_back(
            (constituent_oracle_evaluations[0]->operator[](i) - this->oracle_evaluation_)
            * all_inverted_shifted_elems[i]);
    }

    return result;
}

template<typename FieldT>
FieldT single_boundary_constraint<FieldT>::evaluation_at_point(
    const std::size_t evaluation_position,
    const FieldT evaluation_point,
    const std::vector<FieldT> &constituent_oracle_evaluations) const
{
    libiop::UNUSED(evaluation_position);
    libiop::UNUSED(evaluation_point);

    if (constituent_oracle_evaluations.size() != 1)
    {
        throw std::invalid_argument("Single Boundary Constraint: "
            "Expected exactly 1 constituent oracle.");
    }

    const FieldT result = (constituent_oracle_evaluations[0] - this->oracle_evaluation_) *
        (evaluation_point - this->eval_point_).inverse();

    return result;
}

} // libiop
