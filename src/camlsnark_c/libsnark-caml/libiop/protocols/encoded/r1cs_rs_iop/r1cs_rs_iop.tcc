#include <cmath>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/lagrange.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/vanishing_polynomial.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/common/common.hpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

/** TODO: Rethink parameterization with holographic setting */
template<typename FieldT>
encoded_aurora_parameters<FieldT>::encoded_aurora_parameters(
    const size_t interactive_security_parameter,
    const size_t codeword_domain_dim,
    const size_t constraint_domain_dim,
    const size_t summation_domain_dim,
    const size_t query_bound,
    const bool make_zk,
    const bool holographic,
    const field_subset_type domain_type) :
    interactive_security_parameter_(interactive_security_parameter),
    codeword_domain_dim_(codeword_domain_dim),
    constraint_domain_dim_(constraint_domain_dim),
    summation_domain_dim_(summation_domain_dim),
    query_bound_(query_bound),
    make_zk_(make_zk),
    holographic_(holographic),
    domain_type_(domain_type)
{
    /** Force the non-zk query bound to be 0. */
    if (!this->make_zk_)
    {
        this->query_bound_ = 0;
    }
    if (this->holographic_) // Fractal / holographic lincheck
    {
        this->holographic_lincheck_params_ = holographic_lincheck_parameters<FieldT>(
            this->interactive_security_parameter_,
            this->constraint_domain_dim_,
            this->make_zk_,
            this->domain_type_);
    }
    else if(!this->holographic_) // Aurora / basic lincheck
    {
        this->multi_lincheck_params_ = basic_lincheck_parameters<FieldT>(
            this->interactive_security_parameter_,
            this->constraint_domain_dim_,
            this->make_zk_,
            this->domain_type_);
    }
}

template<typename FieldT>
size_t encoded_aurora_parameters<FieldT>::max_tested_degree_bound() const
{
    /** The max tested degree in the non-zk case are the low degree extensions, which have max degree |H|. */
    if (this->make_zk_ == false)
    {
        return 1ull << this->summation_domain_dim_;
    }
    /** In the zk case it is 2|H| + b - 1, from lincheck. (The codeword is the sumcheck masking polynomial)
     *  However if the query bound is large, then the row check degree, |constraint domain| + 2b - 1, may dominate.*/
    const size_t lincheck_degree_bound = 2 * (1ull << this->summation_domain_dim_) + this->query_bound_ - 1;
    const size_t rowcheck_degree_bound = (1ull << this->constraint_domain_dim_) + 2 * this->query_bound_ - 1;
    return std::max<size_t>(lincheck_degree_bound, rowcheck_degree_bound);
}

template<typename FieldT>
size_t encoded_aurora_parameters<FieldT>::max_constraint_degree_bound() const
{
    /** The max constraint degree is the same as the degree of the polynomial output by linncheck,
     *  unless the query bound is very large.
     *  In the latter case, it will then come from rowcheck, and be 2|constraint domain| + 2b - 1 */
    // TODO: Get this from lincheck params
    const size_t lincheck_degree_bound = 2 * (1ull << this->summation_domain_dim_) + this->query_bound_ - 1;
    if (this->make_zk_ == false)
    {
        return lincheck_degree_bound;
    }
    const size_t rowcheck_constraint_degree_bound = 2*(1ull << this->constraint_domain_dim_) + 2 * this->query_bound_ - 1;
    return std::max<size_t>(lincheck_degree_bound, rowcheck_constraint_degree_bound);
}

template<typename FieldT>
std::vector<size_t> encoded_aurora_parameters<FieldT>::locality_vector() const
{
    /** fw, f_Az, f_Bz, f_Cz */
    const size_t arithmetization_locality = 4;
    if (!this->holographic())
    {
        const size_t lincheck_repetitions =
            this->multi_lincheck_params_.multi_lincheck_repetitions();
        const size_t zk_locality = this->make_zk_ ? lincheck_repetitions : 0;
        const size_t lincheck_locality = lincheck_repetitions;
        return {arithmetization_locality + zk_locality, lincheck_locality};
    }
    else if (this->holographic())
    {
        // TODO: Get from lincheck params
        const size_t zk_locality = this->make_zk_ ? 1 : 0;
        const size_t lincheck_locality_round_1 = 1;
        const size_t lincheck_locality_round_2 = 2;
        const size_t indexed_round_locality = 9;
        return {indexed_round_locality, arithmetization_locality + zk_locality,
                lincheck_locality_round_1, lincheck_locality_round_2};
    }
    return {};
}

template<typename FieldT>
bool encoded_aurora_parameters<FieldT>::make_zk() const
{
    return this->make_zk_;
}

template<typename FieldT>
bool encoded_aurora_parameters<FieldT>::holographic() const
{
    return this->holographic_;
}

template<typename FieldT>
size_t encoded_aurora_parameters<FieldT>::query_bound() const
{
    return this->query_bound_;
}

template<typename FieldT>
class fz_virtual_oracle : public virtual_oracle<FieldT> {
protected:
    std::size_t primary_input_size_;
    field_subset<FieldT> input_variable_domain_;
    field_subset<FieldT> codeword_domain_;

    std::vector<FieldT> primary_input_;
    std::shared_ptr<lagrange_cache<FieldT>> L_X_for_input_domain_;
public:
    fz_virtual_oracle(
        const std::size_t primary_input_size,
        const field_subset<FieldT> &input_variable_domain,
        const field_subset<FieldT> &codeword_domain) :
        primary_input_size_(primary_input_size),
        input_variable_domain_(input_variable_domain),
        codeword_domain_(codeword_domain)
    {
        // Consistency check that we ordered the domains correctly
        if (input_variable_domain.num_elements() > codeword_domain.num_elements())
        {
            throw std::invalid_argument("Codeword domain must be bigger than the input variable domain.");
        }

        this->L_X_for_input_domain_ = std::make_shared<lagrange_cache<FieldT>>(
            this->input_variable_domain_, false);
    }

    void set_primary_input(const std::vector<FieldT> &primary_input)
    {
        if (primary_input.size() != this->primary_input_size_)
        {
            throw std::invalid_argument("Primary input size does not match the previously declared size.");
        }

        this->primary_input_ = primary_input;
    }

    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const
    {
        enter_block("fz evaluated contents");
        if (constituent_oracle_evaluations.size() != 1)
        {
            throw std::invalid_argument("fz_virtual_oracle has one constituent oracle.");
        }

        if (this->primary_input_.size() != this->primary_input_size_)
        {
            throw std::logic_error("Evaluation requested before primary_input is set.");
        }

        const std::shared_ptr<std::vector<FieldT>> &fw = constituent_oracle_evaluations[0];

        if (fw->size() != this->codeword_domain_.num_elements())
        {
            throw std::invalid_argument("Provided fw evaluations don't match the declared codeword domain size.");
        }

        const vanishing_polynomial<FieldT> input_vp(this->input_variable_domain_);
        std::vector<FieldT> input_vp_over_codeword_domain =
            input_vp.evaluations_over_field_subset(this->codeword_domain_);

        std::vector<FieldT> f_1v_evaluations({ FieldT::one() });
        f_1v_evaluations.insert(f_1v_evaluations.end(),
                                this->primary_input_.begin(), this->primary_input_.end());
        std::vector<FieldT> f_1v_coefficients =
            IFFT_over_field_subset<FieldT>(f_1v_evaluations, this->input_variable_domain_);
        const std::vector<FieldT> f_1v_over_codeword_domain =
            FFT_over_field_subset<FieldT>(f_1v_coefficients, this->codeword_domain_);

        std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>();
        result->reserve(this->codeword_domain_.num_elements());
        for (std::size_t i = 0; i < this->codeword_domain_.num_elements(); ++i)
        {
            result->emplace_back(
                fw->operator[](i) * input_vp_over_codeword_domain[i] + f_1v_over_codeword_domain[i]);
        }
        leave_block("fz evaluated contents");

        return result;
    }

    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const
    {
        UNUSED(evaluation_position);

        if (constituent_oracle_evaluations.size() != 1)
        {
            throw std::invalid_argument("fz_virtual_oracle has one constituent oracle.");
        }

        if (this->primary_input_.size() != this->primary_input_size_)
        {
            throw std::logic_error("Evaluation requested before primary_input is set.");
        }

        const FieldT fw_X = constituent_oracle_evaluations[0];
        const std::vector<FieldT> L_X_over_input_variable_domain =
            this->L_X_for_input_domain_->coefficients_for(evaluation_point);
        FieldT f1v_X = L_X_over_input_variable_domain[0]; // for the constant term
        for (std::size_t i = 0; i < this->primary_input_size_; ++i)
        {
            f1v_X += L_X_over_input_variable_domain[i+1] * this->primary_input_[i];
        }
        const vanishing_polynomial<FieldT> input_vp(this->input_variable_domain_);
        const FieldT input_vp_X = input_vp.evaluation_at_point(evaluation_point);

        return fw_X * input_vp_X + f1v_X;
    }
};

template<typename FieldT>
encoded_aurora_protocol<FieldT>::encoded_aurora_protocol(
    iop_protocol<FieldT> &IOP,
    const domain_handle &constraint_domain_handle,
    const domain_handle &variable_domain_handle,
    const domain_handle &codeword_domain_handle,
    const std::shared_ptr<r1cs_constraint_system<FieldT>> &constraint_system,
    const encoded_aurora_parameters<FieldT> &params) :
    IOP_(IOP),
    constraint_domain_handle_(constraint_domain_handle),
    variable_domain_handle_(variable_domain_handle),
    codeword_domain_handle_(codeword_domain_handle),
    constraint_system_(constraint_system),
    params_(params)
{
    /* TODO: check that codeword domains and variable/constraint domains do not overlap */
    this->constraint_domain_ = this->IOP_.get_domain(this->constraint_domain_handle_);
    this->variable_domain_ = this->IOP_.get_domain(this->variable_domain_handle_);
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);

    /** \bar{f}_{1,v} is a polynomial of degree k+1, which is interpolated from
     * non-power-of-two evaluations. There is a fast method to interpolate this
     * polynomial if the degree of f is a power of two. Hence we require that
     * k + 1 be a power of two to enable this faster interpolation.
     */
    if (!is_power_of_2(this->constraint_system_->num_inputs() + 1))
    {
        throw std::invalid_argument("number of inputs in the constraint system must be one less than a power of two.");
    }
    this->input_variable_domain_ = this->variable_domain_.get_subset_of_order(
        this->constraint_system_->num_inputs() + 1);

    this->register_witness_oracles();
}

template<typename FieldT>
void encoded_aurora_protocol<FieldT>::register_witness_oracles()
{
    /** We register all the oracles that the RS-encoded IOP should be sending in the first round,
     *  and virtual oracles that only depend on oracles sent thus far.
     *  In particular, this is f_w, f_Az, f_Bz, f_Cz, rowcheck, and oracles due to lincheck.
     */
    const std::size_t m = round_to_next_power_of_2(this->constraint_system_->num_constraints());
    const std::size_t n = round_to_next_power_of_2(this->constraint_system_->num_variables());
    const std::size_t k = this->constraint_system_->num_inputs();
    const std::size_t b = this->params_.query_bound();

    const std::size_t fw_degree = n - (k + 1) + b;
    this->fw_mask_degree_ = b;
    this->fw_handle_ = this->IOP_.register_oracle(this->codeword_domain_handle_, fw_degree, this->params_.make_zk());

    const std::size_t fABCz_degree = (m + b);
    this->fAz_handle_ = this->IOP_.register_oracle(this->codeword_domain_handle_, fABCz_degree, this->params_.make_zk());
    this->fBz_handle_ = this->IOP_.register_oracle(this->codeword_domain_handle_, fABCz_degree, this->params_.make_zk());
    this->fCz_handle_ = this->IOP_.register_oracle(this->codeword_domain_handle_, fABCz_degree, this->params_.make_zk());

    this->fz_oracle_ = std::make_shared<fz_virtual_oracle<FieldT> >(
        k,
        this->input_variable_domain_,
        this->codeword_domain_);

    const std::size_t fz_degree = fw_degree + k + 1;
    this->fz_oracle_handle_ = this->IOP_.register_virtual_oracle(
        this->codeword_domain_handle_,
        fz_degree,
        { std::make_shared<oracle_handle>(this->fw_handle_) },
        this->fz_oracle_);

    this->r1cs_A_ = std::make_shared<r1cs_sparse_matrix<FieldT> >(
        this->constraint_system_, r1cs_sparse_matrix_A);
    this->r1cs_B_ = std::make_shared<r1cs_sparse_matrix<FieldT> >(
        this->constraint_system_, r1cs_sparse_matrix_B);
    this->r1cs_C_ = std::make_shared<r1cs_sparse_matrix<FieldT> >(
        this->constraint_system_, r1cs_sparse_matrix_C);
    std::vector<std::shared_ptr<sparse_matrix<FieldT> >> matrices =
        {this->r1cs_A_, this->r1cs_B_, this->r1cs_C_};

    std::vector<oracle_handle_ptr> Mz_handles = {
        std::make_shared<oracle_handle>(this->fAz_handle_),
        std::make_shared<oracle_handle>(this->fBz_handle_),
        std::make_shared<oracle_handle>(this->fCz_handle_)
    };

    if (this->params_.holographic())
    {
        /* TODO: Get from elsewhere */
        this->holographic_multi_lincheck_ = std::make_shared<holographic_multi_lincheck<FieldT>>(
            this->IOP_,
            this->codeword_domain_handle_,
            /* TODO: Should be summation domain handle */
            this->constraint_domain_handle_,
            this->input_variable_domain_.dimension(),
            matrices,
            std::make_shared<virtual_oracle_handle>(this->fz_oracle_handle_),
            Mz_handles,
            this->params_.holographic_lincheck_params_);
    }
    else if (!this->params_.holographic())
    {
        this->multi_lincheck_ = std::make_shared<multi_lincheck<FieldT> >(
            this->IOP_,
            this->codeword_domain_handle_,
            this->constraint_domain_handle_,
            this->variable_domain_handle_,
            this->input_variable_domain_.dimension(),
            matrices,
            std::make_shared<virtual_oracle_handle>(this->fz_oracle_handle_),
            Mz_handles,
            this->params_.multi_lincheck_params_);
    }

    /** rowcheck degree is deg((f_Az * f_Bz - f_Cz) / Z_{constraint}) =
     * ((|constraint_domain| + b) * 2 - 1) - |constraint_domain| =
     * |constraint_domain| + b * 2 - 1  */
    const std::size_t rowcheck_degree =
        this->constraint_domain_.num_elements() + b * 2 - 1;

    this->rowcheck_oracle_ = std::make_shared<rowcheck_ABC_virtual_oracle<FieldT> >(
        this->codeword_domain_,
        this->constraint_domain_);
    this->rowcheck_oracle_handle_ = this->IOP_.register_virtual_oracle(
        this->codeword_domain_handle_,
        rowcheck_degree,
        Mz_handles,
        this->rowcheck_oracle_);
}

template<typename FieldT>
void encoded_aurora_protocol<FieldT>::set_index_oracles(
    const domain_handle &indexed_domain_handle,
    const std::vector<std::vector<oracle_handle_ptr>> indexed_handles)
{
    this->holographic_multi_lincheck_->set_index_oracles(indexed_domain_handle, indexed_handles);
}

template<typename FieldT>
void encoded_aurora_protocol<FieldT>::register_challenge()
{
    if (this->params_.holographic())
    {
        this->holographic_multi_lincheck_->register_challenge_alpha();
    } else if (!this->params_.holographic())
    {
        this->multi_lincheck_->register_challenge();
    }
}

template<typename FieldT>
void encoded_aurora_protocol<FieldT>::register_proof()
{
    if (this->params_.holographic())
    {
        /* TODO: Expose this better for IOP layer optimizations */
        this->holographic_multi_lincheck_->register_response_alpha();
        this->holographic_multi_lincheck_->register_challenge_beta();
        this->holographic_multi_lincheck_->register_response_beta();
    } else if (!this->params_.holographic())
    {
        this->multi_lincheck_->register_proof();
    }
}

template<typename FieldT>
std::vector<FieldT> create_fw_prime_evals(
    const std::vector<FieldT> &f_1v_over_variable_domain,
    const std::size_t primary_input_size, const r1cs_auxiliary_input<FieldT> &auxiliary_input,
    const field_subset<FieldT> &variable_domain)
{
    /** This function creates evaluations of \hat{f}_w(X) within the variable domain, placing FieldT(0)
     *  at the corresponding input variable locations. */
    std::vector<FieldT> fw_prime_evals;
    fw_prime_evals.resize(variable_domain.num_elements(), FieldT::zero());
    const std::size_t input_variable_dim = log2(primary_input_size + 1);
    /** Evaluations at the input variable locations are already initialized to zero.
     *  Evaluations of \hat{f}_w(X) are then placed at the witness locations */
    for (std::size_t i = 0; i < auxiliary_input.size(); ++i)
    {
        const std::size_t variable_index = variable_domain.reindex_by_subset(
            input_variable_dim, i + primary_input_size + 1);
        // division by input variable vanishing polynomial is handled later in the protocol.
        fw_prime_evals[variable_index] = (
            (auxiliary_input[i] - f_1v_over_variable_domain[variable_index]));
    }
    assert(fw_prime_evals.size() == variable_domain.num_elements());
    return fw_prime_evals;
}

template<typename FieldT>
void encoded_aurora_protocol<FieldT>::compute_fprime_ABCz_over_codeword_domain(
    std::vector<FieldT> &Az, std::vector<FieldT> &Bz, std::vector<FieldT> &Cz)
{
    /** This converts the evaluations of f_Az over the constraint domain to be
     *  over the codeword domain. The same is done for Bz, and Cz.
     *
     *  These matrices may be randomized due to fz' randomness from f_w in the zk case.
     */

    polynomial<FieldT> f_Az(std::move(
            IFFT_over_field_subset<FieldT>(Az, this->constraint_domain_)));
    polynomial<FieldT> f_Bz(std::move(
            IFFT_over_field_subset<FieldT>(Bz, this->constraint_domain_)));
    polynomial<FieldT> f_Cz(std::move(
            IFFT_over_field_subset<FieldT>(Cz, this->constraint_domain_)));

    if (this->params_.make_zk()) {
        // Add constraint_vp * R_A/B/Cz to each of the polynomials
        const vanishing_polynomial<FieldT> constraint_vp(this->constraint_domain_);
        f_Az += constraint_vp * this->R_Az_;
        f_Bz += constraint_vp * this->R_Bz_;
        f_Cz += constraint_vp * this->R_Cz_;
    }

    this->fprime_Az_over_codeword_domain_ =
        FFT_over_field_subset<FieldT>(f_Az.coefficients(), this->codeword_domain_);
    this->fprime_Bz_over_codeword_domain_=
        FFT_over_field_subset<FieldT>(f_Bz.coefficients(), this->codeword_domain_);
    this->fprime_Cz_over_codeword_domain_ =
        FFT_over_field_subset<FieldT>(f_Cz.coefficients(), this->codeword_domain_);
}

template<typename FieldT>
void encoded_aurora_protocol<FieldT>::submit_witness_oracles(
    const r1cs_primary_input<FieldT> &primary_input,
    const r1cs_auxiliary_input<FieldT> &auxiliary_input)
{
    this->fz_oracle_->set_primary_input(primary_input);

    enter_block("Submit witness oracles");
    if (this->params_.holographic())
    {
        this->holographic_multi_lincheck_->submit_sumcheck_masking_polynomials();
    }
    else if (!this->params_.holographic())
    {
        this->multi_lincheck_->submit_sumcheck_masking_polynomials();
    }

    enter_block("Compute randomized f_w");
    /* Randomization polynomials for the top-level R1CS protocol */
    if (this->params_.make_zk()) {
        this->R_Az_ = polynomial<FieldT>::random_polynomial(this->params_.query_bound());
        this->R_Bz_ = polynomial<FieldT>::random_polynomial(this->params_.query_bound());
        this->R_Cz_ = polynomial<FieldT>::random_polynomial(this->params_.query_bound());
    }
    /** [BCRSVW18] protocol 8.5, step 1
     * This calculates the coefficients of f_1v by interpolating the provided
     * evaluations. This imposes the requirement that the primary input is
     *  one less than a power of 2, in order to use an IFFT for efficient interpolation.
     */
    std::vector<FieldT> f_1v_evaluations({ FieldT::one() });
    f_1v_evaluations.insert(f_1v_evaluations.end(),
                            primary_input.begin(), primary_input.end());

    // TODO: Pass this to the fz_oracle to prevent it having to do the same IFFT
    this->f_1v_coefficients_ =
        IFFT_over_field_subset<FieldT>(f_1v_evaluations, this->input_variable_domain_);
    const std::vector<FieldT> f_1v_over_variable_domain =
        FFT_over_field_subset<FieldT>(this->f_1v_coefficients_, this->variable_domain_);

    /** [BCRSVW18] protocol 8.5, step 2
     * Calculates randomized f_w, f_z, f_Az, f_Bz, f_Cz
     * The implementation proceeds as follows:
     * 1) Compute fw
     * 2) Compute f_Az, f_Bz, and f_Cz
     * 3) Submit oracles
     */

    /** 1) Compute fw.
     * fw is computed over the codeword domain as follows:
     * i)  First we construct it from evaluations over the variable domain.
     *     The systematic component of the domain is the component corresponding to the auxiliary domain.
     *     The elements placed in the input variable positions are all 0.
     *     The evaluations placed in the witness locations are:
     *         witness[i] - f_{1,v}(variable_domain_elements[i + k])
     *     Note that the above does not divide by the input variable vanishing polynomial, that is done later.
     * ii) Perform an IFFT from the above evaluations, to get a polynomial representation of fw_prime.
     *     We then add R_w * Z_{variable_domain} to fw_prime,
     *     where R_w is a random polynomial of degree `query bound`.
     *     Then we divide this resultant polynomial by Z_{input_variable_domain},
     *     and set fw_prime to be this quotient. Note that there is no remainder,
     *     since the input variable evaluations were all set to 0.
     *     This also causes the systematic evaluations to be divided by Z_{input_variable_domain} as well.
     *     This makes fw_prime the polynomial representation of \bar{f}_w,
     *     with the expected degree and systematic evaluations.
     * iii) We do an FFT to evaluate fw_prime over the codeword domain. */

    const std::vector<FieldT> fw_prime_over_variable_domain = create_fw_prime_evals(
        f_1v_over_variable_domain,
        primary_input.size(), auxiliary_input,
        this->variable_domain_);
    assert(fw_prime_over_variable_domain.size() == this->variable_domain_.num_elements());
    polynomial<FieldT> fw_prime(
        std::move(
            IFFT_over_field_subset<FieldT>(fw_prime_over_variable_domain, this->variable_domain_)
        ));

    if (this->params_.make_zk()) {
        // fw = fw_prime + Z_{var} * R2
        const vanishing_polynomial<FieldT> var_vp(this->variable_domain_);
        this->fw_mask_ = polynomial<FieldT>::random_polynomial(this->fw_mask_degree_);
        fw_prime += var_vp * this->fw_mask_;
    }
    // ii) Divide by input variable vanishing polynomial, and set fw_prime to be the quotient
    const vanishing_polynomial<FieldT> input_vp(this->input_variable_domain_);
    fw_prime = polynomial_over_vanishing_polynomial(fw_prime, input_vp).first;
    // iii) evaluate it over the codeword domain
    this->fw_over_codeword_domain_ =
        FFT_over_field_subset<FieldT>(fw_prime.coefficients(), this->codeword_domain_);

    leave_block("Compute randomized f_w");

    /**  2) Calculate f_{Az}, f_{Bz}, f_{Cz} over the constraint domain
     *   i)   Construct z, and compute Az, Bz, Cz
     *   ii)  LDE extend A/B/Cz to f_{A/B/Cz}
     *   iii) Adds Z_{constraint domain} * R_{A/B/Cz} to each of the corresponding codewords
     *   iv)  FFT this into the codeword domain
    */
    enter_block("Compute A/B/Cz");

    std::vector<FieldT> variable_assignment({ FieldT::one() });
    variable_assignment.insert(variable_assignment.end(),
                            primary_input.begin(), primary_input.end());
    variable_assignment.insert(variable_assignment.end(),
                            auxiliary_input.begin(), auxiliary_input.end());

    std::vector<FieldT> Az;
    std::vector<FieldT> Bz;
    std::vector<FieldT> Cz;
    Az.reserve(this->constraint_domain_.num_elements());
    Bz.reserve(this->constraint_domain_.num_elements());
    Cz.reserve(this->constraint_domain_.num_elements());
    this->constraint_system_->create_Az_Bz_Cz_from_variable_assignment(
        variable_assignment, Az, Bz, Cz);

    leave_block("Compute A/B/Cz");

    enter_block("Compute f_{A/B/Cz} over codeword domain");
    this->compute_fprime_ABCz_over_codeword_domain(Az, Bz, Cz);
    leave_block("Compute f_{A/B/Cz} over codeword domain");

    /** 3) Submit all the oracles */
    enter_block("Call IOP oracle submission routines");
    this->IOP_.submit_oracle(this->fw_handle_, std::move(this->fw_over_codeword_domain_));
    this->IOP_.submit_oracle(this->fAz_handle_, std::move(this->fprime_Az_over_codeword_domain_));
    this->IOP_.submit_oracle(this->fBz_handle_, std::move(this->fprime_Bz_over_codeword_domain_));
    this->IOP_.submit_oracle(this->fCz_handle_, std::move(this->fprime_Cz_over_codeword_domain_));
    /* cross-compiler method to ensure memory is freed */
    std::vector<FieldT>().swap(this->fw_over_codeword_domain_);
    std::vector<FieldT>().swap(this->fprime_Az_over_codeword_domain_);
    std::vector<FieldT>().swap(this->fprime_Bz_over_codeword_domain_);
    std::vector<FieldT>().swap(this->fprime_Cz_over_codeword_domain_);
    leave_block("Call IOP oracle submission routines");

    leave_block("Submit witness oracles");
}

template<typename FieldT>
void encoded_aurora_protocol<FieldT>::calculate_and_submit_proof()
{
    /* Hand off everything to the multi_lincheck protocol */
    if (this->params_.holographic())
    {
        /* TODO */
        this->holographic_multi_lincheck_->calculate_response_alpha();
        this->IOP_.signal_prover_round_done();
        this->holographic_multi_lincheck_->calculate_response_beta();
    }
    else if (!this->params_.holographic())
    {
       this->multi_lincheck_->calculate_and_submit_proof();
    }
}

template<typename FieldT>
void encoded_aurora_protocol<FieldT>::construct_verifier_state(const r1cs_primary_input<FieldT> &primary_input)
{
    this->fz_oracle_->set_primary_input(primary_input);

    if (this->params_.holographic())
    {
        this->holographic_multi_lincheck_->construct_verifier_state();
    }
    else if (!this->params_.holographic())
    {
       this->multi_lincheck_->construct_verifier_state();
    }
}

template<typename FieldT>
std::vector<oracle_handle_ptr> encoded_aurora_protocol<FieldT>::get_all_oracle_handles()
{
    std::vector<oracle_handle_ptr> result;
    if (this->params_.holographic())
    {
        result = this->holographic_multi_lincheck_->get_all_oracle_handles();
    }
    else if (!this->params_.holographic())
    {
        result = this->multi_lincheck_->get_all_oracle_handles();
    }
    result.emplace_back(std::make_shared<oracle_handle>(this->fw_handle_));
    result.emplace_back(std::make_shared<oracle_handle>(this->fAz_handle_));
    result.emplace_back(std::make_shared<oracle_handle>(this->fBz_handle_));
    result.emplace_back(std::make_shared<oracle_handle>(this->fCz_handle_));
    result.emplace_back(std::make_shared<virtual_oracle_handle>(this->rowcheck_oracle_handle_));

    return result;
}

} // namespace libiop
