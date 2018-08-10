#include <libsnark/caml/caml_mnt4.hpp>
#include <libsnark/gadgetlib1/gadgets/verifiers/r1cs_ppzksnark_verifier_gadget.hpp>
#include <libsnark/gadgetlib1/gadgets/verifiers/r1cs_se_ppzksnark_verifier_gadget.hpp>

extern "C" {
using namespace libsnark;

// verification key

void camlsnark_mnt4_emplace_bits_of_field(std::vector<bool>* v, FieldT &x) {
  size_t field_size = FieldT::size_in_bits();
  auto n = x.as_bigint();
  for (size_t i = 0; i < field_size; ++i) {
    v->emplace_back(n.test_bit(i));
  }
}

std::vector<bool>* camlsnark_mnt4_verification_key_other_to_bool_vector(
    r1cs_ppzksnark_verification_key<other_curve_ppT>* vk
) {
  return new std::vector<bool>(
      r1cs_ppzksnark_verification_key_variable<ppT>::get_verification_key_bits(*vk));
}

std::vector<FieldT>* camlsnark_mnt4_verification_key_other_to_field_vector(
    r1cs_ppzksnark_verification_key<other_curve_ppT>* r1cs_vk
) {
  const size_t input_size_in_elts = r1cs_vk->encoded_IC_query.rest.indices.size(); // this might be approximate for bound verification keys, however they are not supported by r1cs_ppzksnark_verification_key_variable
  const size_t vk_size_in_bits = r1cs_ppzksnark_verification_key_variable<ppT>::size_in_bits(input_size_in_elts);

  protoboard<FieldT> pb;
  pb_variable_array<FieldT> vk_bits;
  vk_bits.allocate(pb, vk_size_in_bits, "vk_bits");
  r1cs_ppzksnark_verification_key_variable<ppT> vk(pb, vk_bits, input_size_in_elts, "translation_step_vk");
  vk.generate_r1cs_witness(*r1cs_vk);

  return new std::vector<FieldT>(vk.all_vars.get_vals(pb));
}

// verification key variable
r1cs_ppzksnark_verification_key_variable<ppT>* camlsnark_mnt4_r1cs_ppzksnark_verification_key_variable_create(
    protoboard<FieldT>* pb,
    pb_variable_array<FieldT>* all_bits,
    int input_size) {
  return new r1cs_ppzksnark_verification_key_variable<ppT>(*pb, *all_bits, input_size, "verification_key_variable");
}

int camlsnark_mnt4_r1cs_ppzksnark_verification_key_variable_size_in_bits_for_input_size(int input_size) {
  return r1cs_ppzksnark_verification_key_variable<ppT>::size_in_bits(input_size);
}

void camlsnark_mnt4_r1cs_ppzksnark_verification_key_variable_delete(
    r1cs_ppzksnark_verification_key_variable<ppT>* vk) {
  delete vk;
}

void camlsnark_mnt4_r1cs_ppzksnark_verification_key_variable_generate_r1cs_constraints(
    r1cs_ppzksnark_verification_key_variable<ppT>* vk) {
  vk->generate_r1cs_constraints(false);
}

void camlsnark_mnt4_r1cs_ppzksnark_verification_key_variable_generate_r1cs_witness(
    r1cs_ppzksnark_verification_key_variable<ppT>* vkv,
    r1cs_ppzksnark_verification_key<other_curve_ppT>* vk) {
  vkv->generate_r1cs_witness(*vk);
}

// proof
r1cs_ppzksnark_proof_variable<ppT>* camlsnark_mnt4_r1cs_ppzksnark_proof_variable_create(
    protoboard<FieldT>* pb) {
  return new r1cs_ppzksnark_proof_variable<ppT>(*pb, "proof_variable");
}

void camlsnark_mnt4_r1cs_ppzksnark_proof_variable_delete(
    r1cs_ppzksnark_proof_variable<ppT>* p) {
  delete p;
}

void camlsnark_mnt4_r1cs_ppzksnark_proof_variable_generate_r1cs_constraints(
    r1cs_ppzksnark_proof_variable<ppT>* p) {
  p->generate_r1cs_constraints();
}

void camlsnark_mnt4_r1cs_ppzksnark_proof_variable_generate_r1cs_witness(
    r1cs_ppzksnark_proof_variable<ppT>* pv,
    r1cs_ppzksnark_proof<other_curve_ppT>* p) {
  pv->generate_r1cs_witness(*p);
}

// verifier
r1cs_ppzksnark_verifier_gadget<ppT>* camlsnark_mnt4_r1cs_ppzksnark_verifier_gadget_create(
    protoboard<FieldT>* pb,
    r1cs_ppzksnark_verification_key_variable<ppT>* vk,
    pb_variable_array<FieldT>* input,
    int elt_size,
    r1cs_ppzksnark_proof_variable<ppT>* proof,
    pb_variable<FieldT>* result) {
  return new r1cs_ppzksnark_verifier_gadget<ppT>(*pb, *vk, *input, elt_size, *proof, *result, "verifier_gadget");
}

void camlsnark_mnt4_r1cs_ppzksnark_verifier_gadget_delete(
    r1cs_ppzksnark_verifier_gadget<ppT>* g) {
  delete g;
}

void camlsnark_mnt4_r1cs_ppzksnark_verifier_gadget_generate_r1cs_constraints(
    r1cs_ppzksnark_verifier_gadget<ppT>* g) {
  g->generate_r1cs_constraints();
}

void camlsnark_mnt4_r1cs_ppzksnark_verifier_gadget_generate_r1cs_witness(
    r1cs_ppzksnark_verifier_gadget<ppT>* g) {
  g->generate_r1cs_witness();
}

// GM verifier gadget functions

// preprocessed verification key variable
r1cs_se_ppzksnark_preprocessed_r1cs_se_ppzksnark_verification_key_variable<ppT>* 
camlsnark_mnt4_r1cs_se_ppzksnark_preprocessed_r1cs_se_ppzksnark_verification_key_variable_create_known(
    protoboard<FieldT>* pb,
    r1cs_se_ppzksnark_verification_key<other_curve<ppT>>* vk)
{
  return new r1cs_se_ppzksnark_preprocessed_r1cs_se_ppzksnark_verification_key_variable<ppT>(
      *pb, *vk, "preprocessed_verification_key_variable");
}

r1cs_se_ppzksnark_preprocessed_r1cs_se_ppzksnark_verification_key_variable<ppT>* 
camlsnark_mnt4_r1cs_se_ppzksnark_preprocessed_r1cs_se_ppzksnark_verification_key_variable_create()
{
  return new r1cs_se_ppzksnark_preprocessed_r1cs_se_ppzksnark_verification_key_variable<ppT>();
}

void
camlsnark_mnt4_r1cs_se_ppzksnark_preprocessed_r1cs_se_ppzksnark_verification_key_variable_delete(
    r1cs_se_ppzksnark_preprocessed_r1cs_se_ppzksnark_verification_key_variable<ppT>* vk)
{
    delete vk;
}

// verifier
r1cs_se_ppzksnark_verifier_gadget<ppT>*
camlsnark_mnt4_r1cs_se_ppzksnark_verifier_gadget_create(
    protoboard<FieldT>* pb,
    r1cs_se_ppzksnark_verification_key_variable<ppT>* vk,
    pb_variable_array<FieldT>* input,
    int elt_size,
    r1cs_se_ppzksnark_proof_variable<ppT>* proof,
    pb_variable<FieldT>* result) {
  return new r1cs_se_ppzksnark_verifier_gadget<ppT>(
      *pb, *vk, *input, elt_size, *proof, *result,
      "se_verifier_gadget");
}

void camlsnark_mnt4_r1cs_se_ppzksnark_verifier_gadget_delete(
    r1cs_se_ppzksnark_verifier_gadget<ppT>* g) {
  delete g;
}

void camlsnark_mnt4_r1cs_se_ppzksnark_verifier_gadget_generate_r1cs_constraints(
    r1cs_se_ppzksnark_verifier_gadget<ppT>* g) {
  g->generate_r1cs_constraints();
}

void camlsnark_mnt4_r1cs_se_ppzksnark_verifier_gadget_generate_r1cs_witness(
    r1cs_se_ppzksnark_verifier_gadget<ppT>* g) {
  g->generate_r1cs_witness();
}

// online verifier
r1cs_se_ppzksnark_online_verifier_gadget<ppT>*
camlsnark_mnt4_r1cs_se_ppzksnark_online_verifier_gadget_create(
    protoboard<FieldT>* pb,
    r1cs_se_ppzksnark_preprocessed_r1cs_se_ppzksnark_verification_key_variable<ppT>* vk,
    pb_variable_array<FieldT>* input,
    int elt_size,
    r1cs_se_ppzksnark_proof_variable<ppT>* proof,
    pb_variable<FieldT>* result) {
  return new r1cs_se_ppzksnark_online_verifier_gadget<ppT>(
      *pb, *vk, *input, elt_size, *proof, *result,
      "se_online_verifier_gadget");
}

void camlsnark_mnt4_r1cs_se_ppzksnark_online_verifier_gadget_delete(
    r1cs_se_ppzksnark_online_verifier_gadget<ppT>* g) {
  delete g;
}

void camlsnark_mnt4_r1cs_se_ppzksnark_online_verifier_gadget_generate_r1cs_constraints(
    r1cs_se_ppzksnark_online_verifier_gadget<ppT>* g) {
  g->generate_r1cs_constraints();
}

void camlsnark_mnt4_r1cs_se_ppzksnark_online_verifier_gadget_generate_r1cs_witness(
    r1cs_se_ppzksnark_online_verifier_gadget<ppT>* g) {
  g->generate_r1cs_witness();
}

// proof
r1cs_se_ppzksnark_proof_variable<ppT>* camlsnark_mnt4_r1cs_se_ppzksnark_proof_variable_create(
    protoboard<FieldT>* pb) {
  return new r1cs_se_ppzksnark_proof_variable<ppT>(*pb, "proof_variable");
}

void camlsnark_mnt4_r1cs_se_ppzksnark_proof_variable_delete(
    r1cs_se_ppzksnark_proof_variable<ppT>* p) {
  delete p;
}

void camlsnark_mnt4_r1cs_se_ppzksnark_proof_variable_generate_r1cs_constraints(
    r1cs_se_ppzksnark_proof_variable<ppT>* p) {
  p->generate_r1cs_constraints();
}

void camlsnark_mnt4_r1cs_se_ppzksnark_proof_variable_generate_r1cs_witness(
    r1cs_se_ppzksnark_proof_variable<ppT>* pv,
    r1cs_se_ppzksnark_proof<other_curve_ppT>* p) {
  pv->generate_r1cs_witness(*p);
}

// verification key variable
r1cs_se_ppzksnark_verification_key_variable <ppT>* 
camlsnark_mnt4_r1cs_se_ppzksnark_verification_key_variable_create(
    protoboard<FieldT>* pb,
    int input_size)
{
  return new r1cs_se_ppzksnark_verification_key_variable<ppT>(
      *pb, input_size, "se_verification_key_variable");
}

void
camlsnark_mnt4_r1cs_se_ppzksnark_verification_key_variable_delete(
    r1cs_se_ppzksnark_verification_key_variable<other_curve<ppT>>* vk)
{
    delete vk;
}

void
camlsnark_mnt4_r1cs_se_ppzksnark_verification_key_variable_generate_r1cs_witness(
    r1cs_se_ppzksnark_verification_key_variable<ppT>* vk,
    r1cs_se_ppzksnark_verification_key<other_curve<ppT>>* r1cs_vk
    )
{
    vk->generate_r1cs_witness(*r1cs_vk);
}

std::vector<linear_combination<FieldT>>* 
camlsnark_mnt4_r1cs_se_ppzksnark_verification_key_variable_characterizing_vars_up_to_sign(
    r1cs_se_ppzksnark_verification_key_variable<ppT>* vk
    )
{
    std::vector<linear_combination<FieldT>>* res = new std::vector< linear_combination<FieldT> >();

    // Get all the G1 X coordinates
    for (size_t i = 0; i < vk->all_G1_vars.size(); ++i) {
      res->emplace_back(vk->all_G1_vars[i]->X);
    }

    // Get all the G2 X coordinates
    for (size_t i = 0; i < vk->all_G2_vars.size(); ++i) {
      pb_linear_combination_array<FieldT> vars = vk->all_G2_vars[i]->X->all_vars;
      for (size_t j = 0; j < vars.size(); ++j) {
        res->emplace_back(vars[j]);
      }
    }

    // Get all the GT c0 coordinates
    for (size_t i = 0; i < vk->all_GT_vars.size(); ++i) {
      pb_linear_combination_array<FieldT> vars = vk->all_GT_vars[i]->c0.all_vars;
      for (size_t j = 0; j < vars.size(); ++j) {
        res->emplace_back(vars[j]);
      }
    }

    assert (res->size() ==
        vk->all_G1_vars.size() +
        vk->all_G2_vars.size() * Fqe_variable<ppT>::num_variables() +
        vk->all_GT_vars.size() * Fqe_variable<ppT>::num_variables() );

    return res;
}

// NB! These field elements MUST be unpacked fully (i.e., with not with choose_preimage)
std::vector<linear_combination<FieldT>>* 
camlsnark_mnt4_r1cs_se_ppzksnark_verification_key_variable_sign_vars(
    r1cs_se_ppzksnark_verification_key_variable<ppT>* vk
    )
{
    std::vector<linear_combination<FieldT>>* res = new std::vector< linear_combination<FieldT> >();

    // Get all the G1 Y coordinates.
    // We don't actually have to constrain these to be non-zero, but for ease
    // we will anyway because there are only a few of them.
    for (size_t i = 0; i < vk->all_G1_vars.size(); ++i) {
      res->emplace_back(vk->all_G1_vars[i]->Y);
    }

    // Get all the G2 Y.c0 coordinates. The analysis of why this is
    // correct is similar to what is discussed below.
    for (size_t i = 0; i < vk->all_G2_vars.size(); ++i) {
      res->emplace_back( vk->all_G2_vars[i]->Y->c0 );
    }

    // Say a GT variable is (c0, c1) =  c0 + W c1.
    // It must be unitary (since it will be equal to the output of a pairing) so it
    // satisfies
    //
    // (c0 + W c1) * (c0 - W c1) = 1
    // c0^2 - W^2 c1^2 =1
    // c1^2 = (c0^2 - 1)/W^2
    //
    // So c1 is determined up to sign by c0.
    //
    // c1 can be thought of as some vector of FieldT's, so assuming the first coordinate
    // of that vector is non-zero, its sign determines c1
    // (since its sign will be distinct in c1 and -c1)
    for (size_t i = 0; i < vk->all_GT_vars.size(); ++i) {
      res->emplace_back( vk->all_GT_vars[i]->c1.c0 );
    }

    assert (res->size() ==
        vk->all_G1_vars.size() +
        vk->all_G2_vars.size() +
        vk->all_GT_vars.size() );

    return res;
}

std::vector< FieldT >*
camlsnark_mnt4_gm_verification_key_characterizing_elts_up_to_sign(
    r1cs_se_ppzksnark_verification_key<other_curve<ppT>> *vk)
{
    std::vector<libff::G1<other_curve<ppT>>> all_G1_elts = { vk->G_alpha, vk->G_gamma };
    std::vector<libff::G2<other_curve<ppT>>> all_G2_elts = { vk->H, vk->H_beta, vk->H_gamma };
    std::vector<libff::Fqk<other_curve<ppT>>> all_GT_elts = { vk->G_alpha_H_beta.unitary_inverse() };

    all_G1_elts.emplace_back(vk->query[0]);
    size_t input_size = vk->query.size() - 1;
    for (size_t i = 0; i < input_size; ++i) {
        all_G1_elts.emplace_back(vk->query[i+1]);
    }

    std::vector<FieldT>* res = new std::vector<FieldT>();

    // Get all the G1 X coordinates
    for (size_t i = 0; i < all_G1_elts.size(); ++i) {
      all_G1_elts[i].to_affine_coordinates();
      res->emplace_back(all_G1_elts[i].X());
    }

    // Get all the G2 X coordinates
    for (size_t i = 0; i < all_G2_elts.size(); ++i) {
      all_G2_elts[i].to_affine_coordinates();
      std::vector<FieldT> elts = all_G2_elts[i].X().all_base_field_elements();
      for (size_t j = 0; j < elts.size(); ++j) {
        res->emplace_back(elts[j]);
      }
    }

    // Get all the GT c0 coordinates
    for (size_t i = 0; i < all_GT_elts.size(); ++i) {
      std::vector<FieldT> elts = all_GT_elts[i].c0.all_base_field_elements();
      for (size_t j = 0; j < elts.size(); ++j) {
        res->emplace_back(elts[j]);
      }
    }

    return res;
}

std::vector<FieldT>*
camlsnark_mnt4_gm_verification_key_sign_elts(
    r1cs_se_ppzksnark_verification_key<other_curve<ppT>> *vk)
{
    std::vector<libff::G1<other_curve<ppT>>> all_G1_elts = { vk->G_alpha, vk->G_gamma };
    std::vector<libff::G2<other_curve<ppT>>> all_G2_elts = { vk->H, vk->H_beta, vk->H_gamma };
    std::vector<libff::Fqk<other_curve<ppT>>> all_GT_elts = { vk->G_alpha_H_beta.unitary_inverse() };

    all_G1_elts.emplace_back(vk->query[0]);
    size_t input_size = vk->query.size() - 1;
    for (size_t i = 0; i < input_size; ++i) {
        all_G1_elts.emplace_back(vk->query[i+1]);
    }

    std::vector<FieldT>* res = new std::vector<FieldT>();

    for (size_t i = 0; i < all_G1_elts.size(); ++i) {
      all_G1_elts[i].to_affine_coordinates();
      res->emplace_back(all_G1_elts[i].Y());
    }

    for (size_t i = 0; i < all_G2_elts.size(); ++i) {
      all_G2_elts[i].to_affine_coordinates();
      res->emplace_back(all_G2_elts[i].Y().c0);
    }

    for (size_t i = 0; i < all_GT_elts.size(); ++i) {
      res->emplace_back(all_GT_elts[i].c1.c0);
    }

    return res;
}

}
