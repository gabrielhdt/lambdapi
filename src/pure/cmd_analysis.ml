(************************************************************************)
(* The λΠ-modulo Interactive Proof Assistant *)
(************************************************************************)

(************************************************************************)
(* λΠ-modulo serialization Toplevel *)
(* Copyright Inria -- Dual License LGPL 2.1 / GPL3+                     *)
(* Written by: F. Blanqui, E. J. Gallego Arias, F. Lefoulon             *)
(************************************************************************)
(* Status: Experimental                                                 *)
(************************************************************************)

open! Lplib
open Core

let pmap = List.pmap (fun x -> x)
let concat_map = List.concat_map

type t = (P_terms.p_term, P_terms.p_rule) Syntax.p_command

(*Messy pattern matching to get the qidents throughout the document*)
let rec qidents_of_bound_p_args (args : P_terms.p_term Syntax.p_args) :
    Syntax.qident list * Pos.strloc list =
  match args with
  | idents, None, _ -> ([], pmap idents)
  | idents, Some ty, _ -> (qidents_of_p_term ty, pmap idents)

and qidents_of_p_term (term : P_terms.p_term) =
  match term.elt with
  | P_terms.P_Type -> []
  | P_terms.P_Iden (qid, _) -> [ qid ]
  | P_terms.P_Wild -> []
  | P_terms.P_Meta (_, Some arr) ->
    let terms = Array.to_list arr in
    concat_map qidents_of_p_term terms
  | P_terms.P_Meta (_, None) -> []
  | P_terms.P_Patt (_, Some arr) ->
    let terms = Array.to_list arr in
    concat_map qidents_of_p_term terms
  | P_terms.P_Patt (_, None) -> []
  | P_terms.P_Appl (f, arg) -> qidents_of_p_term f @ qidents_of_p_term arg
  | P_terms.P_Impl (t1, t2) -> qidents_of_p_term t1 @ qidents_of_p_term t2
  | P_terms.P_Abst (args, term) -> filter_bound_qidents args [ term ]
  | P_terms.P_Prod (args, term) -> filter_bound_qidents args [ term ]
  | P_terms.P_LLet (_, args, Some t1, t2, t3) ->
    filter_bound_qidents args [ t1; t2; t3 ]
  | P_terms.P_LLet (_, args, None, t1, t2) ->
    filter_bound_qidents args [ t1; t2 ]
  | P_terms.P_NLit _ -> []
  | P_terms.P_UnaO (_, term) -> qidents_of_p_term term
  | P_terms.P_BinO (t1, _, t2) -> qidents_of_p_term t1 @ qidents_of_p_term t2
  | P_terms.P_Wrap term -> qidents_of_p_term term
  | P_terms.P_Expl term -> qidents_of_p_term term

and filter_bound_qidents (args : P_terms.p_term Syntax.p_args list)
    (terms_list : P_terms.p_term list) =
  let qids, qargs = List.split (List.map qidents_of_bound_p_args args) in
  let qids, qargs = (List.concat qids, List.concat qargs) in
  let args = List.map (fun (id : Syntax.ident) -> id.elt) qargs in
  let filter_args (id : Syntax.qident) = not (List.mem (snd id.elt) args) in
  let get_qterm term = List.filter filter_args (qidents_of_p_term term) in
  let qterm = concat_map get_qterm terms_list in
  (* Format.eprintf "Bound identifiers :%s\n%!" (String.concat " " args); *)
  qids @ qterm

and qidents_of_p_config (cfg : P_terms.p_rule Syntax.p_config) =
  match cfg with
  | Syntax.P_config_builtin (_, qid) -> [ qid ]
  | Syntax.P_config_unop u ->
    let _, _, qid = u in
    [ qid ]
  | Syntax.P_config_binop b ->
    let _, _, _, qid = b in
    [ qid ]
  | Syntax.P_config_ident _ -> []
  | Syntax.P_config_quant qid -> [ qid ]
  | Syntax.P_config_unif_rule rule -> qidents_of_p_rule rule

and qidents_of_p_rule (rule : P_terms.p_rule) =
  let patt, term = rule.elt in
  qidents_of_p_term patt @ qidents_of_p_term term

let qidents_of_p_inductive (pind : P_terms.p_term Syntax.p_inductive) =
  let f (_, pt) = qidents_of_p_term pt in
  let _, pt, idptlist = pind.elt in
  qidents_of_p_term pt @ concat_map f idptlist

let qidents_of_cmd (cmd : t) =
  match cmd.elt with
  | Syntax.P_inductive (_, pil) -> concat_map qidents_of_p_inductive pil
  | Syntax.P_require (_, _) -> []
  | Syntax.P_require_as (_, _) -> []
  | Syntax.P_open _ -> []
  | Syntax.P_rules rules -> concat_map qidents_of_p_rule rules
  | Syntax.P_symbol {p_sym_arg;p_sym_typ;p_sym_trm;_} ->
    let some_or_empty = function Some arg -> [arg] | None -> [] in
    let terms_list = some_or_empty p_sym_typ @ some_or_empty p_sym_trm in
    filter_bound_qidents p_sym_arg terms_list
  | Syntax.P_set set -> qidents_of_p_config set
  | Syntax.P_query q ->
    let f (q : P_terms.p_term Syntax.p_query_aux) =
      match q with
      | Syntax.P_query_verbose _ -> []
      | Syntax.P_query_debug (_, _) -> []
      | Syntax.P_query_flag (_, _) -> []
      | Syntax.P_query_assert (_, assertion) ->
        let g (assertion : P_terms.p_term Syntax.p_assertion) =
          match assertion with
          | Syntax.P_assert_typing (t1, t2) ->
            qidents_of_p_term t1 @ qidents_of_p_term t2
          | Syntax.P_assert_conv (t1, t2) ->
            qidents_of_p_term t1 @ qidents_of_p_term t2
        in
        g assertion
      | Syntax.P_query_infer (term, _) -> qidents_of_p_term term
      | Syntax.P_query_normalize (term, _) -> qidents_of_p_term term
      | Syntax.P_query_prover _ -> []
      | Syntax.P_query_prover_timeout _ -> []
      | Syntax.P_query_print None -> []
      | Syntax.P_query_print (Some qid) -> [qid]
      | Syntax.P_query_proofterm -> []
    in
    f q.elt

(* This function is used to get the symbol identifiers in the document and
   then create a map from them *)
let get_qidents (doc : t) = qidents_of_cmd doc
