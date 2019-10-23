/** @file
 *****************************************************************************

 Declaration of interfaces for:
 - a R1CS constraint,
 - a R1CS variable assignment, and
 - a R1CS constraint system.

 See r1cs.hpp .

 *****************************************************************************
 * @author     This file is adapted from libsnark, developed by SCIPR Lab
 *             and contributors
 *             (see AUTHORS for libsnark and here for libiop).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef LIBIOP_RELATIONS_R1CS_TCC_
#define LIBIOP_RELATIONS_R1CS_TCC_

#include <algorithm>
#include <cassert>
#include <set>

#include "libiop/common/common.hpp"

namespace libiop {

template<typename FieldT>
r1cs_constraint<FieldT>::r1cs_constraint(const linear_combination<FieldT> &a,
                                         const linear_combination<FieldT> &b,
                                         const linear_combination<FieldT> &c) :
    a_(a), b_(b), c_(c)
{
}

template<typename FieldT>
r1cs_constraint<FieldT>::r1cs_constraint(const std::initializer_list<linear_combination<FieldT> > &A,
                                         const std::initializer_list<linear_combination<FieldT> > &B,
                                         const std::initializer_list<linear_combination<FieldT> > &C)
{
    for (auto lc_A : A)
    {
        a_.terms.insert(a_.terms.end(), lc_A.terms.begin(), lc_A.terms.end());
    }
    for (auto lc_B : B)
    {
        b_.terms.insert(b_.terms.end(), lc_B.terms.begin(), lc_B.terms.end());
    }
    for (auto lc_C : C)
    {
        c_.terms.insert(c_.terms.end(), lc_C.terms.begin(), lc_C.terms.end());
    }
}

template<typename FieldT>
bool r1cs_constraint<FieldT>::operator==(const r1cs_constraint<FieldT> &other) const
{
    return (this->a_ == other.a_ &&
            this->b_ == other.b_ &&
            this->c_ == other.c_);
}

template<typename FieldT>
size_t r1cs_constraint_system<FieldT>::num_inputs() const
{
    return primary_input_size_;
}

template<typename FieldT>
size_t r1cs_constraint_system<FieldT>::num_variables() const
{
    return primary_input_size_ + auxiliary_input_size_;
}


template<typename FieldT>
size_t r1cs_constraint_system<FieldT>::num_constraints() const
{
    return constraints_.size();
}

template<typename FieldT>
bool r1cs_constraint_system<FieldT>::is_valid() const
{
    if (this->num_inputs() > this->num_variables()) return false;

    for (size_t c = 0; c < constraints_.size(); ++c)
    {
        if (!(constraints_[c].a_.is_valid(this->num_variables()) &&
              constraints_[c].b_.is_valid(this->num_variables()) &&
              constraints_[c].c_.is_valid(this->num_variables())))
        {
            return false;
        }
    }

    return true;
}

template<typename FieldT>
void dump_r1cs_constraint(const r1cs_constraint<FieldT> &constraint,
                          const r1cs_variable_assignment<FieldT> &full_variable_assignment,
                          const std::map<size_t, std::string> &variable_annotations)
{
    printf("terms for a:\n"); constraint.a_.print_with_assignment(full_variable_assignment, variable_annotations);
    printf("terms for b:\n"); constraint.b_.print_with_assignment(full_variable_assignment, variable_annotations);
    printf("terms for c:\n"); constraint.c_.print_with_assignment(full_variable_assignment, variable_annotations);
}

template<typename FieldT>
bool r1cs_constraint_system<FieldT>::is_satisfied(const r1cs_primary_input<FieldT> &primary_input,
                                                  const r1cs_auxiliary_input<FieldT> &auxiliary_input) const
{
    assert(primary_input.size() == num_inputs());
    assert(primary_input.size() + auxiliary_input.size() == num_variables());

    const r1cs_variable_assignment<FieldT> full_variable_assignment =
        variable_assignment_from_inputs(primary_input, auxiliary_input);
    return this->is_satisfied(full_variable_assignment);
}

template<typename FieldT>
bool r1cs_constraint_system<FieldT>::is_satisfied(const r1cs_variable_assignment<FieldT> &full_variable_assignment) const
{
    for (size_t c = 0; c < constraints_.size(); ++c)
    {
        const FieldT ares = constraints_[c].a_.evaluate(full_variable_assignment);
        const FieldT bres = constraints_[c].b_.evaluate(full_variable_assignment);
        const FieldT cres = constraints_[c].c_.evaluate(full_variable_assignment);

        if (!(ares*bres == cres))
        {
#ifdef DEBUG
            auto it = constraint_annotations_.find(c);
            printf("constraint %zu (%s) unsatisfied\n", c, (it == constraint_annotations_.end() ? "no annotation" : it->second.c_str()));
            printf("<a,(1,x)> = "); ares.print();
            printf("<b,(1,x)> = "); bres.print();
            printf("<c,(1,x)> = "); cres.print();
            printf("constraint was:\n");
            dump_r1cs_constraint(constraints_[c], full_variable_assignment, variable_annotations_);
#endif // DEBUG
            return false;
        }
    }

    return true;
}

template<typename FieldT>
void r1cs_constraint_system<FieldT>::add_constraint(const r1cs_constraint<FieldT> &c)
{
    constraints_.emplace_back(c);
}

template<typename FieldT>
void r1cs_constraint_system<FieldT>::add_constraint(const r1cs_constraint<FieldT> &c, const std::string &annotation)
{
#ifdef DEBUG
    constraint_annotations_[constraints_.size()] = annotation;
#else
    UNUSED(annotation);
#endif
    constraints_.emplace_back(c);
}

template<typename FieldT>
bool r1cs_constraint_system<FieldT>::operator==(const r1cs_constraint_system<FieldT> &other) const
{
    return (this->constraints_ == other.constraints_ &&
            this->primary_input_size_ == other.primary_input_size_ &&
            this->auxiliary_input_size_ == other.auxiliary_input_size_);
}

template<typename FieldT>
naive_sparse_matrix<FieldT> r1cs_constraint_system<FieldT>::A_matrix() const
{
    naive_sparse_matrix<FieldT> matrix;
    for (std::size_t i = 0; i < this->num_constraints(); ++i)
    {
        const r1cs_constraint<FieldT> &constraint = this->constraints_[i];

        std::vector<linear_term<FieldT>> terms = constraint.a_.terms;
        std::map<std::size_t, FieldT> values;
        for (std::size_t j = 0; j < terms.size(); ++j)
        {
            values.insert(std::pair<std::size_t, FieldT>(terms[j].index_, terms[j].coeff_));
        }

        matrix.push_back(std::move(values));
    }
    return matrix;
}

template<typename FieldT>
naive_sparse_matrix<FieldT> r1cs_constraint_system<FieldT>::B_matrix() const
{
    naive_sparse_matrix<FieldT> matrix;
    for (std::size_t i = 0; i < this->num_constraints(); ++i)
    {
        const r1cs_constraint<FieldT> &constraint = this->constraints_[i];

        std::vector<linear_term<FieldT>> terms = constraint.b_.terms;
        std::map<std::size_t, FieldT> values;
        for (std::size_t j = 0; j < terms.size(); ++j)
        {
            values.insert(std::pair<std::size_t, FieldT>(terms[j].index_, terms[j].coeff_));
        }

        matrix.push_back(std::move(values));
    }
    return matrix;
}

template<typename FieldT>
naive_sparse_matrix<FieldT> r1cs_constraint_system<FieldT>::C_matrix() const
{
    naive_sparse_matrix<FieldT> matrix;
    for (std::size_t i = 0; i < this->num_constraints(); ++i)
    {
        const r1cs_constraint<FieldT> &constraint = this->constraints_[i];

        std::vector<linear_term<FieldT>> terms = constraint.c_.terms;
        std::map<std::size_t, FieldT> values;
        for (std::size_t j = 0; j < terms.size(); ++j)
        {
            values.insert(std::pair<std::size_t, FieldT>(terms[j].index_, terms[j].coeff_));
        }

        matrix.push_back(std::move(values));
    }
    return matrix;
}

template<typename FieldT>
void r1cs_constraint_system<FieldT>::create_Az_Bz_Cz_from_variable_assignment(
    const r1cs_variable_assignment<FieldT> &variable_assignment,
    std::vector<FieldT> &Az_out,
    std::vector<FieldT> &Bz_out,
    std::vector<FieldT> &Cz_out) const
{
    /** This assumes variable assignment z is structured as (1, v, w). */
    for (size_t i = 0; i < this->constraints_.size(); ++i)
    {
        FieldT Az_i = FieldT::zero();
        for (auto &lt : this->constraints_[i].a_.terms)
        {
            Az_i += variable_assignment[lt.index_] * lt.coeff_;
        }
        Az_out.emplace_back(Az_i);

        FieldT Bz_i = FieldT::zero();
        for (auto &lt : this->constraints_[i].b_.terms)
        {
            Bz_i += variable_assignment[lt.index_] * lt.coeff_;
        }
        Bz_out.emplace_back(Bz_i);

        FieldT Cz_i = FieldT::zero();
        for (auto &lt : this->constraints_[i].c_.terms)
        {
            Cz_i += variable_assignment[lt.index_] * lt.coeff_;
        }
        Cz_out.emplace_back(Cz_i);
    }
}

template<typename FieldT>
std::size_t r1cs_constraint_system<FieldT>::size_in_bytes() const
{
    std::size_t num_terms = 0;

    for (auto &C : this->constraints_)
    {
        num_terms += (C.a_.terms.size() +
                      C.b_.terms.size() +
                      C.c_.terms.size());
    }

    /*
      Assume that the most efficient representation spends:
       - 3 bits per constraint (to delineate A, B, and C)
       - log2(num_variables) + |FieldT| bits per term
    */
    const std::size_t size_in_bits = 3 * this->num_constraints() +
        num_terms * (log2(this->num_variables()) + 8*sizeof(FieldT));

    return size_in_bits / 8;
}

template<typename FieldT>
r1cs_variable_assignment<FieldT> variable_assignment_from_inputs(const r1cs_primary_input<FieldT> &primary_input,
                                                                 const r1cs_auxiliary_input<FieldT> &auxiliary_input)
{
    r1cs_variable_assignment<FieldT> full_variable_assignment = primary_input;
    full_variable_assignment.insert(full_variable_assignment.end(), auxiliary_input.begin(), auxiliary_input.end());
    return full_variable_assignment;
}

} // libiop

#endif // LIBIOP_RELATIONS_R1CS_TCC_
