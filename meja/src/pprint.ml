open Core_kernel
open Ast_types
open Parsetypes
open Format
open Ast_print

let rec type_desc ?(bracket = false) fmt = function
  | Ptyp_var None ->
      fprintf fmt "_"
  | Ptyp_var (Some name) ->
      fprintf fmt "'%s" name.txt
  | Ptyp_tuple typs ->
      fprintf fmt "@[<1>%a@]" tuple typs
  | Ptyp_arrow (typ1, typ2, implicitness, label) ->
      if bracket then fprintf fmt "(" ;
      ( match implicitness with
      | Explicit ->
          fprintf fmt "%a%a" arg_label label type_expr_b typ1
      | Implicit ->
          fprintf fmt "%a{%a}" arg_label label type_expr typ1 ) ;
      arg_label_box_end fmt label ;
      fprintf fmt "@ -> %a" type_expr typ2 ;
      if bracket then fprintf fmt ")"
  | Ptyp_ctor v ->
      variant fmt v
  | Ptyp_poly (vars, typ) ->
      if bracket then fprintf fmt "(" ;
      fprintf fmt "/*@[%a.@]*/@ %a" (type_desc ~bracket:false)
        (Ptyp_tuple vars) type_expr typ ;
      if bracket then fprintf fmt ")"
  | Ptyp_prover typ ->
      fprintf fmt "@[<2>Prover {@ %a@ }@]" type_expr typ
  | Ptyp_conv (typ1, typ2) ->
      if bracket then fprintf fmt "(" ;
      fprintf fmt "%a@ --> %a" type_expr_b typ1 type_expr typ2 ;
      if bracket then fprintf fmt ")"
  | Ptyp_opaque typ ->
      fprintf fmt "@[<hv2>opaque(@,%a@,)@]" type_expr typ
  | Ptyp_alias (typ, name) ->
      fprintf fmt "(@[<hv1>%a@ as %s@])" type_expr typ name.txt
  | Ptyp_row (tags, closed, min_tags) -> (
      fprintf fmt "[@[<hv2>" ;
      ( match (closed, min_tags) with
      | Closed, None ->
          ()
      | Closed, Some _ ->
          pp_print_string fmt "< "
      | Open, None ->
          pp_print_string fmt "> "
      | Open, Some _ ->
          (* This is a nonsense: we already have a lower bound in [tags], so
             [min_tags] ought to be absent.
          *)
          assert false ) ;
      pp_print_list ~pp_sep:bar_sep row_tag fmt tags ;
      match min_tags with
      | None ->
          ()
      | Some min_tags ->
          let print_tag fmt tag = fprintf fmt "`%a" pp_name tag.Location.txt in
          fprintf fmt "@ > " ;
          pp_print_list ~pp_sep:bar_sep print_tag fmt min_tags )
  | Ptyp_row_subtract (typ, tags) ->
      let print_tag fmt tag = fprintf fmt "`%a" pp_name tag.Location.txt in
      fprintf fmt "[@[<hv2>%a - %a@]]" type_expr typ
        (pp_print_list ~pp_sep:bar_sep print_tag)
        tags

and tuple fmt typs =
  fprintf fmt "(@,%a@,)" (pp_print_list ~pp_sep:comma_sep type_expr) typs

and type_expr fmt typ = type_desc fmt typ.type_desc

and type_expr_b fmt typ = type_desc ~bracket:true fmt typ.type_desc

and variant fmt v =
  match v.var_params with
  | [] ->
      Longident.pp fmt v.var_ident.txt
  | _ ->
      fprintf fmt "@[<hv2>%a%a@]" Longident.pp v.var_ident.txt tuple
        v.var_params

and row_tag fmt {rtag_ident; rtag_arg; rtag_loc= _} =
  match rtag_arg with
  | [] ->
      fprintf fmt "`%a" pp_name rtag_ident.txt
  | _ ->
      fprintf fmt "@[<hv2>`%a%a@]" pp_name rtag_ident.txt tuple rtag_arg

let field_decl fmt decl =
  fprintf fmt "%s:@ @[<hv>%a@]" decl.fld_ident.txt type_expr decl.fld_type

let ctor_args fmt = function
  | Ctor_tuple [] ->
      ()
  | Ctor_tuple typs ->
      tuple fmt typs
  | Ctor_record fields ->
      fprintf fmt "{@[<2>%a@]}"
        (pp_print_list ~pp_sep:comma_sep field_decl)
        fields

let ctor_decl fmt decl =
  fprintf fmt "%a%a" pp_name decl.ctor_ident.txt ctor_args decl.ctor_args ;
  match decl.ctor_ret with
  | Some typ ->
      fprintf fmt "@ :@ @[<hv>%a@]" type_expr typ
  | None ->
      ()

let type_decl_desc fmt = function
  | Pdec_abstract ->
      ()
  | Pdec_alias typ ->
      fprintf fmt "@ =@ @[<hv>%a@]" type_expr typ
  | Pdec_record fields ->
      fprintf fmt "@ =@ {@[<hv2>%a@]}"
        (pp_print_list ~pp_sep:comma_sep field_decl)
        fields
  | Pdec_variant ctors ->
      fprintf fmt "@ =@ %a" (pp_print_list ~pp_sep:bar_sep ctor_decl) ctors
  | Pdec_open ->
      fprintf fmt "@ =@ .."
  | Pdec_extend (name, ctors) ->
      fprintf fmt "@ /*@[%a +=@ %a@]*/" Path.pp name.txt
        (pp_print_list ~pp_sep:bar_sep ctor_decl)
        ctors

let type_decl type_keyword fmt decl =
  fprintf fmt "%s %s" type_keyword decl.tdec_ident.txt ;
  (match decl.tdec_params with [] -> () | _ -> tuple fmt decl.tdec_params) ;
  type_decl_desc fmt decl.tdec_desc

let literal fmt = function
  | Int i ->
      pp_print_int fmt i
  | Int32 i ->
      fprintf fmt "%al" Int32.pp i
  | Int64 i ->
      fprintf fmt "%aL" Int64.pp i
  | Nativeint i ->
      fprintf fmt "%an" Nativeint.pp i
  | Float f ->
      pp_print_float fmt f
  | Bool false ->
      fprintf fmt "0b"
  | Bool true ->
      fprintf fmt "1b"
  | Field f ->
      fprintf fmt "%sf" f
  | Char c ->
      fprintf fmt "'%s'" (Char.escaped c)
  | String s ->
      fprintf fmt "\"%s\"" (String.escaped s)

let rec pattern_desc fmt = function
  | Ppat_any ->
      fprintf fmt "_"
  | Ppat_variable str ->
      fprintf fmt "%s" str.txt
  | Ppat_constraint (p, typ) ->
      fprintf fmt "%a@ : @[<hv2>%a@]" pattern_bracket p type_expr typ
  | Ppat_tuple pats ->
      fprintf fmt "(@[<hv1>@,%a@,@])"
        (pp_print_list ~pp_sep:comma_sep pattern)
        pats
  | Ppat_or (p1, p2) ->
      fprintf fmt "@[<hv0>%a@]@ | @[<hv0>%a@]" pattern p1 pattern p2
  | Ppat_integer (i, None) ->
      fprintf fmt "%s" i
  | Ppat_integer (i, Some suf) ->
      fprintf fmt "%s%c" i suf
  | Ppat_literal l ->
      literal fmt l
  | Ppat_record fields ->
      fprintf fmt "{@[<hv2>%a@]}"
        (pp_print_list ~pp_sep:comma_sep pattern_field)
        fields
  | Ppat_ctor (path, None) ->
      Longident.pp fmt path.txt
  | Ppat_ctor (path, Some ({pat_desc= Ppat_tuple _; _} as arg)) ->
      fprintf fmt "%a@ %a" Longident.pp path.txt pattern arg
  | Ppat_ctor (path, Some arg) ->
      fprintf fmt "%a%a" Longident.pp path.txt pattern arg
  | Ppat_row_ctor (name, []) ->
      fprintf fmt "`%a" pp_name name.txt
  | Ppat_row_ctor (name, args) ->
      fprintf fmt "`%a(@[<hv1>@,%a@,@]" pp_name name.txt
        (pp_print_list ~pp_sep:comma_sep pattern)
        args

and pattern_desc_bracket fmt pat =
  match pat with
  | Ppat_any
  | Ppat_variable _
  | Ppat_tuple _
  | Ppat_literal _
  | Ppat_record _
  | Ppat_ctor _ ->
      pattern_desc fmt pat
  | _ ->
      fprintf fmt "(@[<hv1>@,%a@,@])" pattern_desc pat

and pattern fmt pat = pattern_desc fmt pat.pat_desc

and pattern_bracket fmt pat = pattern_desc_bracket fmt pat.pat_desc

and pattern_field fmt (path, p) =
  fprintf fmt "@[<hv2>%a:@ @[<hv>%a@]@]" Longident.pp path.txt pattern p

let arg_label fmt = function
  | Asttypes.Nolabel ->
      ()
  | Labelled a ->
      fprintf fmt "%s=@," a
  | Optional a ->
      fprintf fmt "?%s=@," a

let rec expression_desc fmt = function
  | Pexp_apply
      ( e
      , [(Asttypes.Nolabel, {exp_desc= Pexp_variable {txt= Lident "()"; _}; _})]
      ) ->
      fprintf fmt "@[<hv2>@[<hv2>%a@]@,()@]" expression_bracket e
  | Pexp_apply (e, args) ->
      fprintf fmt "@[<hv2>@[<hv2>%a@]@,(@[<hv1>@,%a@,@])@]" expression_bracket
        e
        (pp_print_list ~pp_sep:comma_sep expression_args)
        args
  | Pexp_variable lid ->
      Longident.pp fmt lid.txt
  | Pexp_integer (i, None) ->
      fprintf fmt "%s" i
  | Pexp_integer (i, Some suf) ->
      fprintf fmt "%s%c" i suf
  | Pexp_literal l ->
      literal fmt l
  | Pexp_fun (label, p, e, explicitness) ->
      fprintf fmt "fun@ " ;
      ( match explicitness with
      | Explicit ->
          pp_print_char fmt '('
      | Implicit ->
          pp_print_char fmt '{' ) ;
      fprintf fmt "@[<hv2>%a%a@]" arg_label label pattern p ;
      ( match explicitness with
      | Explicit ->
          pp_print_char fmt ')'
      | Implicit ->
          pp_print_char fmt '}' ) ;
      fprintf fmt "@ =>@ {@[<hv2>@ %a;@ @]}" expression e
  | Pexp_newtype (name, e) ->
      fprintf fmt "fun@ (@[<hv2>type@ %s@])@ =>@ {@[<hv2>@ %a;@ @]}" name.txt
        expression e
  | Pexp_seq (e1, e2) ->
      fprintf fmt "%a;@;%a" expression e1 expression e2
  | Pexp_let (p, e1, e2) ->
      fprintf fmt "let@[<hv2>@ %a@] =@ @[<hv2>%a@];@;@]@ %a" pattern p
        expression e1 expression e2
  | Pexp_instance (name, e1, e2) ->
      fprintf fmt "let@[<hv2>@ %s@] =@ @[<hv2>%a@];@;@]@ %a" name.txt
        expression e1 expression e2
  | Pexp_constraint (e, typ) ->
      fprintf fmt "(@[<hv1>%a :@ %a@])" expression e type_expr typ
  | Pexp_tuple es ->
      fprintf fmt "(@[<hv1>@,%a@,@])"
        (pp_print_list ~pp_sep:comma_sep expression)
        es
  | Pexp_match (e, cases) ->
      fprintf fmt "@[<hv2>@[<h>switch@ (@[<hv1>@,%a@,@])@] {@;@[<hv>%a@]@;}@]"
        expression e
        (pp_print_list ~pp_sep:pp_print_space (fun fmt (p, e) ->
             fprintf fmt "| @[<hv2>%a@] =>@;<1 4>@[<hv2>%a@]" pattern p
               expression e ))
        cases
  | Pexp_field (e, lid) ->
      fprintf fmt "@[<hv2>%a@,@].%a" expression_bracket e Longident.pp lid.txt
  | Pexp_record (fields, None) ->
      fprintf fmt "@[<hv2>{@,@[<hv2>%a@]@,}@]"
        (pp_print_list ~pp_sep:comma_sep expression_field)
        fields
  | Pexp_record (fields, Some default) ->
      fprintf fmt "@[<hv2>{@,@[<hv2>...%a@,%a@]@,}@]" expression default
        (pp_print_list ~pp_sep:comma_sep expression_field)
        fields
  | Pexp_ctor (path, None) ->
      Longident.pp fmt path.txt
  | Pexp_ctor (path, Some args) ->
      fprintf fmt "%a%a" Longident.pp path.txt expression args
  | Pexp_row_ctor (ident, []) ->
      fprintf fmt "`%a" pp_name ident.txt
  | Pexp_row_ctor (ident, args) ->
      fprintf fmt "`%a(@[<hv1>%a@,@])" pp_name ident.txt
        (pp_print_list ~pp_sep:comma_sep expression)
        args
  | Pexp_unifiable {expression= Some e; _} ->
      expression fmt e
  | Pexp_unifiable {expression= None; name; _} ->
      fprintf fmt "(%s /* implicit */)" name.txt
  | Pexp_if (e1, e2, None) ->
      fprintf fmt "if@ (@[<hv1>@,%a@,@]) {@[<hv2>@,%a@,@]}" expression e1
        expression e2
  | Pexp_if (e1, e2, Some ({exp_desc= Pexp_if _; _} as e3)) ->
      (* `if (...) {...} else if (...) {...} ...` printing *)
      fprintf fmt "if@ (@[<hv1>@,%a@,@]) {@[<hv2>@,%a@,@]}@ else %a" expression
        e1 expression e2 expression e3
  | Pexp_if (e1, e2, Some e3) ->
      fprintf fmt
        "if@ (@[<hv1>@,%a@,@]) {@[<hv2>@,%a@,@]}@ else@ {@[<hv2>@,%a@,@]}"
        expression e1 expression e2 expression e3
  | Pexp_prover e ->
      fprintf fmt "@[<hv2>Prover {@,%a@,}@]" expression e

and expression_desc_bracket fmt exp =
  match exp with
  | Pexp_unifiable {expression= Some e; _} ->
      expression_bracket fmt e
  | Pexp_apply _
  | Pexp_variable _
  | Pexp_literal _
  | Pexp_constraint _
  | Pexp_tuple _
  | Pexp_field _
  | Pexp_record _
  | Pexp_ctor _ ->
      expression_desc fmt exp
  | Pexp_seq _ | Pexp_let _ ->
      fprintf fmt "{@[<hv1>@,%a@,@]}" expression_desc exp
  | _ ->
      fprintf fmt "(@[<hv1>@,%a@,@])" expression_desc exp

and expression fmt exp = expression_desc fmt exp.exp_desc

and expression_bracket fmt exp = expression_desc_bracket fmt exp.exp_desc

and expression_args fmt (label, e) =
  fprintf fmt "%a%a" arg_label label expression e

and expression_field fmt (label, e) =
  fprintf fmt "%a:@ %a" Longident.pp label.txt expression e

let conv_type fmt = function
  | Ptconv_with (mode, decl) ->
      let str =
        match mode with Checked -> "with" | Prover -> "with prover"
      in
      type_decl str fmt decl
  | Ptconv_to typ ->
      fprintf fmt "to @[<hv>%a@]" type_expr typ

let rec signature_desc fmt = function
  | Psig_value (name, typ) ->
      fprintf fmt "@[<2>let@ %a@ :@ @[<hv>%a;@]@]@;@;" pp_name name.txt
        type_expr typ
  | Psig_instance (name, typ) ->
      fprintf fmt "@[<2>instance@ %a@ :@ @[<hv>%a@];@]@;@;" pp_name name.txt
        type_expr typ
  | Psig_type decl ->
      fprintf fmt "@[<2>%a;@]@;@;" (type_decl "type") decl
  | Psig_convtype (decl, tconv, conv) ->
      fprintf fmt "@[<2>%a@ %a"
        (type_decl "convertible type")
        decl conv_type tconv ;
      Option.iter conv ~f:(fun conv -> fprintf fmt "@ by %s" conv.txt) ;
      fprintf fmt "@];"
  | Psig_rectype (decl :: decls) ->
      let print_and_decls =
        let pp_sep fmt () = pp_print_char fmt ';' ; pp_print_cut fmt () in
        pp_print_list ~pp_sep (type_decl "and")
      in
      fprintf fmt "@[<2>%a;%a@]@;@;" (type_decl "type") decl print_and_decls
        decls
  | Psig_rectype [] ->
      assert false
  | Psig_module (name, msig) ->
      let prefix fmt = fprintf fmt ":@ " in
      fprintf fmt "@[<hov2>module@ %s@ %a;@]@;@;" name.txt (module_sig ~prefix)
        msig
  | Psig_modtype (name, msig) ->
      let prefix fmt = fprintf fmt "=@ " in
      fprintf fmt "@[<hov2>module type@ %s@ %a;@]@;@;" name.txt
        (module_sig ~prefix) msig
  | Psig_open name ->
      fprintf fmt "@[<2>open %a@]@;@;" Longident.pp name.txt
  | Psig_typeext (typ, ctors) ->
      fprintf fmt "@[<2>type %a +=@[<hv2>@ %a@]@]@;@;" variant typ
        (pp_print_list ~pp_sep:bar_sep ctor_decl)
        ctors
  | Psig_request (typ, ctor) ->
      fprintf fmt "@[<2>request (%a)@[<hv2>@ %a@]@]@;@;" type_expr typ
        ctor_decl ctor
  | Psig_multiple sigs ->
      signature fmt sigs
  | Psig_prover sigs ->
      fprintf fmt "@[<2>Prover {@,%a@,}@]@;@;" signature sigs
  | Psig_convert (name, typ) ->
      (* TODO: review to make sure this is what we really want. *)
      signature_desc fmt (Psig_instance (name, typ))

and signature_item fmt sigi = signature_desc fmt sigi.sig_desc

and signature fmt sigs = List.iter ~f:(signature_item fmt) sigs

and module_sig_desc ~prefix fmt = function
  | Pmty_sig msig ->
      prefix fmt ;
      fprintf fmt "{@[<hv1>@;%a@]}" signature msig
  | Pmty_name name ->
      prefix fmt ; Longident.pp fmt name.txt
  | Pmty_alias name ->
      fprintf fmt "=@ " ; Longident.pp fmt name.txt
  | Pmty_abstract ->
      ()
  | Pmty_functor (name, f, m) ->
      let pp = module_sig ~prefix:(fun _ -> ()) in
      fprintf fmt "/* @[functor@ (%s :@ %a)@ =>@ %a@] */" name.txt pp f pp m

and module_sig ~prefix fmt msig = module_sig_desc ~prefix fmt msig.msig_desc
