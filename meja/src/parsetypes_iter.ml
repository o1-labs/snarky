open Core_kernel
open Parsetypes
open Ast_types

type iterator =
  { type_expr: iterator -> type_expr -> unit
  ; type_desc: iterator -> type_desc -> unit
  ; variant: iterator -> variant -> unit
  ; field_decl: iterator -> field_decl -> unit
  ; ctor_args: iterator -> ctor_args -> unit
  ; ctor_decl: iterator -> ctor_decl -> unit
  ; type_decl: iterator -> type_decl -> unit
  ; type_decl_desc: iterator -> type_decl_desc -> unit
  ; literal: iterator -> literal -> unit
  ; pattern: iterator -> pattern -> unit
  ; pattern_desc: iterator -> pattern_desc -> unit
  ; expression: iterator -> expression -> unit
  ; expression_desc: iterator -> expression_desc -> unit
  ; signature_item: iterator -> signature_item -> unit
  ; signature: iterator -> signature -> unit
  ; signature_desc: iterator -> signature_desc -> unit
  ; module_sig: iterator -> module_sig -> unit
  ; module_sig_desc: iterator -> module_sig_desc -> unit
  ; statement: iterator -> statement -> unit
  ; statements: iterator -> statements -> unit
  ; statement_desc: iterator -> statement_desc -> unit
  ; module_expr: iterator -> module_expr -> unit
  ; module_desc: iterator -> module_desc -> unit
  ; location: iterator -> Location.t -> unit
  ; longident: iterator -> Longident.t -> unit
  ; type0_decl: iterator -> Type0.type_decl -> unit }

let lid iter {Location.txt; loc} =
  iter.longident iter txt ; iter.location iter loc

let str iter ({Location.txt= _; loc} : str) = iter.location iter loc

let type_expr iter {type_desc; type_loc} =
  iter.location iter type_loc ;
  iter.type_desc iter type_desc

let type_desc iter = function
  | Ptyp_var name ->
      Option.iter ~f:(str iter) name
  | Ptyp_tuple typs ->
      List.iter ~f:(iter.type_expr iter) typs
  | Ptyp_arrow (typ1, typ2, _, _) ->
      iter.type_expr iter typ1 ; iter.type_expr iter typ2
  | Ptyp_ctor variant ->
      iter.variant iter variant
  | Ptyp_poly (vars, typ) ->
      List.iter ~f:(iter.type_expr iter) vars ;
      iter.type_expr iter typ
  | Ptyp_prover typ ->
      iter.type_expr iter typ

let variant iter {var_ident; var_params} =
  lid iter var_ident ;
  List.iter ~f:(iter.type_expr iter) var_params

let field_decl iter {fld_ident; fld_type; fld_loc} =
  iter.location iter fld_loc ;
  str iter fld_ident ;
  iter.type_expr iter fld_type

let ctor_args iter = function
  | Ctor_tuple typs ->
      List.iter ~f:(iter.type_expr iter) typs
  | Ctor_record fields ->
      List.iter ~f:(iter.field_decl iter) fields

let ctor_decl iter {ctor_ident; ctor_args; ctor_ret; ctor_loc} =
  iter.location iter ctor_loc ;
  str iter ctor_ident ;
  iter.ctor_args iter ctor_args ;
  Option.iter ~f:(iter.type_expr iter) ctor_ret

let type_decl iter {tdec_ident; tdec_params; tdec_desc; tdec_loc} =
  iter.location iter tdec_loc ;
  str iter tdec_ident ;
  List.iter ~f:(iter.type_expr iter) tdec_params ;
  iter.type_decl_desc iter tdec_desc

let type_decl_desc iter = function
  | Pdec_abstract ->
      ()
  | Pdec_alias typ ->
      iter.type_expr iter typ
  | Pdec_record fields ->
      List.iter ~f:(iter.field_decl iter) fields
  | Pdec_variant ctors ->
      List.iter ~f:(iter.ctor_decl iter) ctors
  | Pdec_open ->
      ()
  | Pdec_extend (_name, _decl, _ctors) ->
      assert false

(* TODO: re-enable this when the Type0 iterator is merged. *)
(*lid iter name ;
      iter.type0_decl iter decl ;
      List.iter ~f:(iter.ctor_decl iter) ctors*)

let literal (_iter : iterator) (_ : literal) = ()

let pattern iter {pat_desc; pat_loc} =
  iter.location iter pat_loc ;
  iter.pattern_desc iter pat_desc

let pattern_desc iter = function
  | Ppat_any ->
      ()
  | Ppat_variable name ->
      str iter name
  | Ppat_constraint (pat, typ) ->
      iter.type_expr iter typ ; iter.pattern iter pat
  | Ppat_tuple pats ->
      List.iter ~f:(iter.pattern iter) pats
  | Ppat_or (p1, p2) ->
      iter.pattern iter p1 ; iter.pattern iter p2
  | Ppat_int _ ->
      ()
  | Ppat_record fields ->
      List.iter fields ~f:(fun (name, pat) ->
          lid iter name ; iter.pattern iter pat )
  | Ppat_ctor (name, arg) ->
      lid iter name ;
      Option.iter ~f:(iter.pattern iter) arg

let expression iter {exp_desc; exp_loc} =
  iter.location iter exp_loc ;
  iter.expression_desc iter exp_desc

let expression_desc iter = function
  | Pexp_apply (e, args) ->
      iter.expression iter e ;
      List.iter args ~f:(fun (_label, e) -> iter.expression iter e)
  | Pexp_variable name ->
      lid iter name
  | Pexp_literal l ->
      iter.literal iter l
  | Pexp_fun (_label, p, e, _explicit) ->
      iter.pattern iter p ; iter.expression iter e
  | Pexp_newtype (name, e) ->
      str iter name ; iter.expression iter e
  | Pexp_seq (e1, e2) ->
      iter.expression iter e1 ; iter.expression iter e2
  | Pexp_let (p, e1, e2) ->
      iter.pattern iter p ; iter.expression iter e1 ; iter.expression iter e2
  | Pexp_constraint (e, typ) ->
      iter.type_expr iter typ ; iter.expression iter e
  | Pexp_tuple es ->
      List.iter ~f:(iter.expression iter) es
  | Pexp_match (e, cases) ->
      iter.expression iter e ;
      List.iter cases ~f:(fun (p, e) ->
          iter.pattern iter p ; iter.expression iter e )
  | Pexp_field (e, name) ->
      lid iter name ; iter.expression iter e
  | Pexp_record (bindings, default) ->
      Option.iter ~f:(iter.expression iter) default ;
      List.iter bindings ~f:(fun (name, e) ->
          lid iter name ; iter.expression iter e )
  | Pexp_ctor (name, arg) ->
      lid iter name ;
      Option.iter ~f:(iter.expression iter) arg
  | Pexp_unifiable {expression; name; id= _} ->
      str iter name ;
      Option.iter ~f:(iter.expression iter) expression
  | Pexp_if (e1, e2, e3) ->
      iter.expression iter e1 ;
      iter.expression iter e2 ;
      Option.iter ~f:(iter.expression iter) e3
  | Pexp_prover e ->
      iter.expression iter e

let signature iter = List.iter ~f:(iter.signature_item iter)

let signature_item iter {sig_desc; sig_loc} =
  iter.location iter sig_loc ;
  iter.signature_desc iter sig_desc

let signature_desc iter = function
  | Psig_value (name, typ) | Psig_instance (name, typ) ->
      str iter name ; iter.type_expr iter typ
  | Psig_type decl ->
      iter.type_decl iter decl
  | Psig_rectype decl ->
      List.iter ~f:(iter.type_decl iter) decl
  | Psig_module (name, msig) | Psig_modtype (name, msig) ->
      str iter name ; iter.module_sig iter msig
  | Psig_open name ->
      lid iter name
  | Psig_typeext (typ, ctors) ->
      iter.variant iter typ ;
      List.iter ~f:(iter.ctor_decl iter) ctors
  | Psig_request (typ, ctor) ->
      iter.type_expr iter typ ; iter.ctor_decl iter ctor
  | Psig_multiple sigs ->
      iter.signature iter sigs
  | Psig_prover sigs ->
      iter.signature iter sigs

let module_sig iter {msig_desc; msig_loc} =
  iter.location iter msig_loc ;
  iter.module_sig_desc iter msig_desc

let module_sig_desc iter = function
  | Pmty_sig sigs ->
      iter.signature iter sigs
  | Pmty_name name ->
      lid iter name
  | Pmty_alias name ->
      lid iter name
  | Pmty_abstract ->
      ()
  | Pmty_functor (name, fsig, msig) ->
      str iter name ; iter.module_sig iter fsig ; iter.module_sig iter msig

let statements iter = List.iter ~f:(iter.statement iter)

let statement iter {stmt_desc; stmt_loc} =
  iter.location iter stmt_loc ;
  iter.statement_desc iter stmt_desc

let statement_desc iter = function
  | Pstmt_value (p, e) ->
      iter.pattern iter p ; iter.expression iter e
  | Pstmt_instance (name, e) ->
      str iter name ; iter.expression iter e
  | Pstmt_type decl ->
      iter.type_decl iter decl
  | Pstmt_module (name, me) ->
      str iter name ; iter.module_expr iter me
  | Pstmt_modtype (name, mty) ->
      str iter name ; iter.module_sig iter mty
  | Pstmt_open name ->
      lid iter name
  | Pstmt_typeext (typ, ctors) ->
      iter.variant iter typ ;
      List.iter ~f:(iter.ctor_decl iter) ctors
  | Pstmt_request (typ, ctor, handler) ->
      iter.type_expr iter typ ;
      iter.ctor_decl iter ctor ;
      Option.iter handler ~f:(fun (p, e) ->
          Option.iter ~f:(iter.pattern iter) p ;
          iter.expression iter e )
  | Pstmt_multiple stmts ->
      iter.statements iter stmts
  | Pstmt_prover stmts ->
      iter.statements iter stmts

let module_expr iter {mod_desc; mod_loc} =
  iter.location iter mod_loc ;
  iter.module_desc iter mod_desc

let module_desc iter = function
  | Pmod_struct stmts ->
      iter.statements iter stmts
  | Pmod_name name ->
      lid iter name
  | Pmod_functor (name, fsig, me) ->
      str iter name ; iter.module_sig iter fsig ; iter.module_expr iter me

let location (_iter : iterator) (_ : Location.t) = ()

let longident iter = function
  | Longident.Lident _ ->
      ()
  | Ldot (l, _) ->
      iter.longident iter l
  | Lapply (l1, l2) ->
      iter.longident iter l1 ; iter.longident iter l2

(** Stub. This isn't part of the parsetypes, so we don't do anything by
    default.
*)
let type0_decl (_iter : iterator) (_ : Type0.type_decl) = ()

let default_iterator =
  { type_expr
  ; type_desc
  ; variant
  ; field_decl
  ; ctor_args
  ; ctor_decl
  ; type_decl
  ; type_decl_desc
  ; literal
  ; pattern
  ; pattern_desc
  ; expression
  ; expression_desc
  ; signature_item
  ; signature
  ; signature_desc
  ; module_sig
  ; module_sig_desc
  ; statement
  ; statements
  ; statement_desc
  ; module_expr
  ; module_desc
  ; location
  ; longident
  ; type0_decl }
