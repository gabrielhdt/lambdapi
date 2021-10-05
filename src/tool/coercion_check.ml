open Common
open Pos
open Core
open Term
open Lplib
open Timed

(** Logging function for typing. *)
let log = Debug.new_logger 'k' "coec" "coercion check"
let log = log.logger

(** [unbind_term t k] is term [t] with [k] bindings on top removed along with
    the context formed with these bindings. For instance,
    [unbind_term (λ x: a, λ y: b, t) 2] returns [([((y, b); (x, a))], t)]. *)
let unbind_term : term -> int -> ctxt * term = fun b k ->
  let rec unbind_term b k =
    let cob a b =
      let (x, b) = Bindlib.unbind b in
      let ctx, t = unbind_term b (k - 1) in
      (x, a, None) :: ctx, t
    in
    if k = 0 then [], b else
      match b with
      | Abst(a, b) | Prod(a, b) -> cob a b
      | _ -> invalid_arg "unbind_term: Not enough binders on given term"
  in
  let (c, t) = unbind_term b k in
  List.rev c, t

(** [unbind_with t c] replaces the top bindings in [t] by variables of [c]. No
    check is performed regarding the types. *)
let unbind_with : term -> ctxt -> term = fun b c ->
  let rec unbind_with b c =
    match c with
    | [] -> b
    | (x,_,_)::tl ->
        ( match b with
          | Abst(_, b) | Prod (_, b) ->
              let b = Bindlib.subst b (mk_Vari x) in
              unbind_with b tl
          | _ -> invalid_arg "unbind_with: Not enough binders on term" )
  in
  unbind_with b (List.rev c)

let ctxt_to_vars : ctxt -> term Bindlib.mvar = fun c ->
  List.map (fun (x,_,_) -> x) c |> Array.of_list

let check_preq : (module Infer.S) -> ctxt -> Sign.prereq -> Sign.prereq =
  fun (module Infer) ctx (n, u, v, w) ->
  if !Debug.log_enabled then
    log "Checking pre requisite %a%s" Print.pp_ctxt ctx n.elt;
  let vars = List.rev_map (fun (x, _, _) -> x) ctx |> Array.of_list in
  let u = Bindlib.msubst u (Array.map mk_Vari vars)
  and v = Bindlib.msubst v (Array.map mk_Vari vars)
  and w = Bindlib.msubst w (Array.map mk_Vari vars) in
  let v, _ = Infer.check_sort ctx (Pos.none v)
  and w, _ = Infer.check_sort ctx (Pos.none w) in
  let u = Infer.check ctx u v in
  let bind_v x = Bindlib.(unbox (bind_mvar vars (lift x))) in
  (n, bind_v u, bind_v v, bind_v w)

(** [preq_to_binding c p x] transform prerequisite [p] typable in context [c]
    into a binding of a context. *)
let preq_to_binding : ctxt -> Sign.prereq -> tvar -> tvar * term =
  fun ctx (_,_,_,w) v ->
  let vars =
    List.rev ctx |> List.to_seq |> Seq.map (fun (x,_,_) -> x) |> Seq.map mk_Vari
    |> Array.of_seq
  in
  (v, Bindlib.msubst w vars)

(** [check_ctxt inf c] refines the types of context [c] using type inference
    module [inf]. *)
let check_ctxt : (module Infer.S) -> ctxt -> ctxt = fun (module Infer) c ->
  let f checked (x, a, t) =
    assert (t = None);
    let (a,_) = Infer.check_sort checked (Pos.none a) in
    (x,a,None)::checked
  in
  List.fold_left f [] (List.rev c)

(** [check cion] runs sanity checks on coercion [cion]. In particular that the
    definition has the mentioned type. *)
let check : (module Infer.S) -> Sign.coercion -> term * (term, term) Bindlib.mbinder =
  fun (module Infer) {defn_ty; defn; prerequisites; arity; name; _} ->
  if !Debug.log_enabled then log "Checking coercion %s" name ;
  let ctxt, defn_ty_unbound = unbind_term defn_ty arity in
  let ctxt = check_ctxt (module Infer) ctxt in
  let prerequisites = Array.map (check_preq (module Infer) ctxt) prerequisites
  and defn_ty_unbound, _ = Infer.check_sort ctxt (Pos.none defn_ty_unbound)
  and vpreqs, defn = Bindlib.unmbind defn in
  let defn_unbound = unbind_with defn ctxt in
  (* TODO: create fresh symbols for prerequisites *)
  let preq_ctx : ctxt =
    Array.map2 (preq_to_binding ctxt) prerequisites vpreqs |>
    Array.map (fun (x,t) -> (x,t,None)) |> Array.to_list
  in
  let defn_unbound =
    Infer.check (preq_ctx @ ctxt) defn_unbound defn_ty_unbound
  in
  let defn,_ = Ctxt.to_abst ctxt defn_unbound in
  (* Replace metavariables by placeholders in defn and defn_ty *)
  let defn = Bindlib.(unbox (bind_mvar vpreqs (lift (LibTerm.meta2ph defn))))
  and defn_ty, _ = Ctxt.to_prod ctxt defn_ty_unbound in
  (LibTerm.meta2ph defn_ty, defn)
