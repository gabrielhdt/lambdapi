open Common
open Pos
open Lplib
open Timed
open Term

(* FIXME: types of meta environments have to be checked *)

let log = Debug.new_logger 'i' "infr" "Infer"
let log = log.logger

(** Type for unification constraints solvers. *)
type solver = problem -> constr list option

type coercion =
  { name : string
  ; precoercions : int list
  (** Arguments that must be searched among available coercions. *)
  ; defn : term (** Definition of the coercion. *)
  ; defn_ty : term (** Type of the definition. Must be a product type. *)
  ; source : int (** Argument of the definition that is coerced. *)
  ; arity : int (** Arity of the target type. *) }

(** Module that provide a lookup function to the refiner. *)
module type LOOKUP = sig
  val coercions : coercion list
  val solve : solver
  (** [solve pb] is specified in {!module:Unif}, see
      {!val:Unif.solve_noexn}. *)
end

module type S = sig
  exception NotTypable
  (** Raised when a term cannot be typed. *)

  val infer : ctxt -> term loc -> term * term
  (** [infer ctx t] returns a tuple [(u, a)] where [a] is the type of [t]
      inferred from context [ctx], [u] is [t] refined. [?pos] is used in error
      messages to indicate the position of [t].
      @raise NotTypable when the type of [t] cannot be inferred. *)

  val check : ?pos:Pos.pos -> ctxt -> term -> term -> term
  (** [check ?pos ctx t a] checks that [t] is of type [a] solving constraints
      with [solver] and returns [t] refined. The parameter [?pos] is used in
      error messages to indicate the position of [t].
      @raise NotTypable if [t] does not have type [a] or type of [t] cannot be
                        inferred. *)

  val check_sort : ctxt -> term loc -> term * term
  (** [type_enforce ctx t] verifies that [t] is a type and returns a tuple
      [(t',s)] where [t'] is [t] refined and [s] is the sort of [t]. *)

  val infer_noexn : constr list -> ctxt -> term ->
    (term * term * constr list) option
  (** [infer_noexn cs ctx t] returns a triplet [(t', t_ty, cs')] where [t_ty]
      is the inferred type of [t] with equations [cs] and in context [ctx],
      [t'] is [t] refined and [cs] is a new list of constraints that must be
      solved so that [t'] has type [t_ty]. If [t] is not typable, [None] is
      returned. *)

  val check_noexn : constr list -> ctxt -> term -> term ->
    (term * constr list) option
  (** [check_noexn ?lg cs ctx t t_ty] ensures that term [t] has type [t_ty] in
      context [ctx] and with equations [cs]. It returns term [t] refined and a
      list of new equations that must be solved. *)
end

module Make : functor (_ : LOOKUP) -> S =
functor
  (L : LOOKUP)
  ->
  struct
    let constraints = Stdlib.ref []

    exception NotTypable

    (** [unif ctx a b] solves the unification problem [ctx ⊢ a ≡ b]. Current
        implementation collects constraints in {!val:constraints} then solves
        them at the end of type checking. *)
    let unif : ctxt -> term -> term -> unit =
     fun ctx a b ->
     if not (Eval.eq_modulo ctx a b) then
     (* NOTE: eq_modulo is used because the unification algorithm counts on
        the fact that no constraint is added in some cases (see test
        "245b.lp"). We may however want to reduce the number of calls to
        [eq_modulo]. *)
       begin
         if !Debug.log_enabled then
           log (Extra.yel "add constraint %a") Print.pp_constr
             (ctx, a, b);
         Stdlib.(constraints := (ctx, a, b) :: !constraints)
       end

    (** {1 Handling coercions} *)

    (** [approx ctx a b] is used to tell whether a coercion defined on [b] can
        be used on [a]. The operation may instantiate meta-variables. *)
    let approx : ctxt -> term -> term -> bool = fun ctx a b ->
      Eval.eq_modulo ctx a b ||
      let tau = Time.save () in
      match L.solve {empty_problem with to_solve = [ctx, a, b]} with
      | Some [] -> true
      | _ -> Time.restore tau; false

    let unbind_meta : ctxt -> term -> int -> term list * term =
      fun ctx ty k ->
      let rec unbind_meta ctx ty k =
        if k <= 0 then [], ty else
        match unfold ty with
        | Prod(a, b) ->
            let m = LibTerm.Meta.make ctx a in
            let b = Bindlib.subst b m in
            let ms, r = unbind_meta ctx b (k - 1) in
            m :: ms, r
        | _ -> [], ty
      in
      let ms, ty = unbind_meta ctx ty k in
      List.rev ms, ty

    let rec coerce : ctxt -> term -> term -> term -> term =
      fun ctx t a b ->
      if Eval.eq_modulo ctx a b then t else
      let rec try_coercions cs =
        match cs with
        | [] -> raise Not_found
        | {defn_ty; source; precoercions; defn; arity; _ }::cs ->
            let meta_dom, range = unbind_meta ctx defn_ty source in
            let l = LibTerm.prod_arity defn_ty in
            let meta_range, range =
              unbind_meta ctx range (l - source - arity)
            in
            let kth =
              try List.(hd (rev meta_dom)) with Failure _ -> assert false
            in
            let source =
              match kth with Meta (m, _) -> !(m.meta_type) | _ -> assert false
            in
            try
              apply ctx meta_dom a b precoercions source range;
              unif ctx t kth;
              unif ctx b range;
              add_args defn (meta_dom @ meta_range)
            with Failure _ -> try_coercions cs
      in
      let eqs = Stdlib.(!constraints) in
      let tau = Time.save () in
      match L.solve {empty_problem with to_solve = (ctx, a, b) :: eqs} with
      | Some [] -> Stdlib.(constraints := []); t
      | _ -> Time.restore tau;
          (* REVIEW:  [constraints] ends up empty *)
          Stdlib.(constraints := eqs);
          if !Debug.log_enabled then
            log "Coerce [%a : %a ≡ %a]" Print.pp_term t
              Print.pp_term a Print.pp_term b;
          try try_coercions L.coercions
          with Not_found ->
            (* FIXME: when is this case encountered? Only when checking SR? *)
            (* Hope that the constraint will be solved later. *)
            unif ctx a b;
            Error.wrn None
              "No coercion found for problem @[<h>%a@ :@, %a@ ≡@ %a@]"
              Print.pp_term t Print.pp_term a Print.pp_term b;
            t

    (** [apply ctx ms a b reqs src tgt] performs the coercion from [src] to
        [tgt] with requirements [reqs] on the coercion problem [a ≡ b] with
        metavariables [ms] possibly appearing in [a] and [b]. The context
        [ctx] is required to instantiate variables.
        @raise Failure when the coercion cannot be applied. *)
    and apply : ctxt -> term list -> term -> term -> int list -> term ->
      term -> unit =
      fun ctx ms a b reqs src tgt ->
      if approx ctx a src && approx ctx b tgt then
        let instantiate_reqs i =
          let m = List.nth ms i in
          let meta = match m with Meta (m, _) -> m | _ -> assert false in
          match !(meta.meta_type) with
          | Prod(v,w) ->
              let x, w = Bindlib.unbind w in
              let v = coerce ctx (mk_Vari x) v w in
              (* Instantiate the pre-requisite [m] by the function that
                 coerces its argument. *)
              unif ctx m
                (mk_Abst (v, Bindlib.(bind_var x (lift v) |> unbox)))
          | _ ->
              Error.fatal_no_pos "Ill-formed coercion: %d-th argument \
                                  is not a function" i
        in
        List.iter instantiate_reqs reqs;
    else failwith "coercion application"

    (** {1 Other rules} *)

    (** [type_enforce ctx a] returns a tuple [(a',s)] where [a'] is refined
        term [a] and [s] is a sort (Type or Kind) such that [a'] is of type
        [s]. *)
    let rec type_enforce : ctxt -> term -> term * term =
     fun ctx a ->
      if !Debug.log_enabled then log "Type enforce [%a]" Print.pp_term a;
      let a, s = infer ctx a in
      let sort =
        match unfold s with
        | Kind -> mk_Kind
        | Type -> mk_Type
        | _ -> mk_Type
        (* FIXME The algorithm should be able to backtrack on the choice of
           this sort, first trying [Type], and if it does not succeed, trying
           [Kind]. *)
      in
      let a = coerce ctx a s sort in
      (a, s)

    (** [force ctx t a] returns a term [t'] such that [t'] has type [a], and
        [t'] is the refinement of [t]. *)
    and force : ctxt -> term -> term -> term =
     fun ctx te ty ->
      let default () =
        if !Debug.log_enabled then
          log "Force [%a] of [%a]" Print.pp_term te Print.pp_term ty;
        let t, a = infer ctx te in
        coerce ctx t a ty
      in
      match unfold te with
      | Abst (dom, t) -> (
          match Eval.whnf ctx ty with
          | Prod (e1, e2) ->
            if !Debug.log_enabled then
              log "Force-λ [%a] of [%a]" Print.pp_term te Print.pp_term ty;
              let dom, _ = type_enforce ctx dom in
              unif ctx dom e1;
              let x, t, e2 = Bindlib.unbind2 t e2 in
              let ctx = (x, dom, None) :: ctx in
              let t = force ctx t e2 in
              mk_Abst (dom, Bindlib.(lift t |> bind_var x |> unbox))
          | _ -> default () )
      | LLet (t_ty, t, u) ->
          let t_ty, _ = type_enforce ctx t_ty in
          let t = force ctx t t_ty in
          let x, u, ctx = Ctxt.unbind ctx t_ty (Some t) u in
          let ty = Bindlib.(lift ty |> bind_var x |> unbox) in
          let ty = Bindlib.subst ty t in
          let u = force ctx u ty in
          let u = Bindlib.(lift u |> bind_var x |> unbox) in
          mk_LLet (t_ty, t, u)
      | _ -> default ()

    and infer : ctxt -> term -> term * term =
     fun ctx t ->
      if !Debug.log_enabled then log "Infer [%a]" Print.pp_term t;
      match unfold t with
      | Patt _ -> assert false
      | TEnv _ -> assert false
      | Kind -> assert false
      | Wild -> assert false
      | TRef _ -> assert false
      | Type -> (mk_Type, mk_Kind)
      | Vari x ->
          let a = try Ctxt.type_of x ctx with Not_found -> assert false in
          (t, a)
      | Symb s -> (t, !(s.sym_type))
      (* All metavariables inserted are typed. *)
      | (Meta (m, ts)) as t ->
          let rec ref_esubst i range =
            (* Refine terms of the explicit substitution. *)
            if i >= Array.length ts then range else
            match unfold range with
            | Prod(ai, b) ->
                ts.(i) <- force ctx ts.(i) ai;
                let b = Bindlib.subst b ts.(i) in
                ref_esubst (i + 1) b
            | _ ->
                (* Meta type must be a product of arity greater or equal to
                   the environment *)
                assert false
          in
          let range = ref_esubst 0 !(m.meta_type) in
          (t, range)
      | LLet (t_ty, t, u) ->
          (* Check that [a] is a type, and refine it. *)
          let t_ty, _ = type_enforce ctx t_ty in
          (* Check that [t] is of type [a'], and refine it *)
          let t = force ctx t t_ty in
          (* Unbind [u] and get new context with [x: t_ty ≔ t] *)
          let x, u, ctx = Ctxt.unbind ctx t_ty (Some t) u in
          (* Infer type of [u'] and refine it. *)
          let u, u_ty = infer ctx u in
          let u_ty = Bindlib.(u_ty |> lift |> bind_var x |> unbox) in
          let u_ty = Bindlib.subst u_ty t in
          let u = Bindlib.(u |> lift |> bind_var x |> unbox) in
          (mk_LLet (t_ty, t, u), u_ty)
      | Abst (dom, b) ->
          (* Domain must by of type Type, we don’t use [type_enforce] *)
          let dom = force ctx dom mk_Type in
          let x, b, ctx = Ctxt.unbind ctx dom None b in
          let b, range = infer ctx b in
          let b = Bindlib.(lift b |> bind_var x |> unbox) in
          let range = Bindlib.(lift range |> bind_var x |> unbox) in
          (mk_Abst (dom, b), mk_Prod (dom, range))
      | Prod (dom, b) ->
          (* Domain must by of type Type, we don’t use [type_enforce] *)
          let dom = force ctx dom mk_Type in
          let x, b, ctx = Ctxt.unbind ctx dom None b in
          let b, b_s = type_enforce ctx b in
          let s =
            match unfold b_s with
            | Type -> mk_Type
            | Kind -> mk_Kind
            | b_s ->
                Error.wrn None
                  "Type error, sort mismatch: there is no rule of the form \
                   (TYPE, %a, _) in λΠ/R" Print.pp_term b_s;
                raise NotTypable

          in
          let b = Bindlib.(lift b |> bind_var x |> unbox) in
          (mk_Prod (dom, b), s)
      | Appl (t, u) -> (
          let t, t_ty = infer ctx t in
          match Eval.whnf ctx t_ty with
          | Prod (dom, b) ->
              let u = force ctx u dom in
              (mk_Appl (t, u), Bindlib.subst b u)
          | Meta (_, _) ->
              let u, u_ty = infer ctx u in
              let range = LibTerm.Meta.make_codomain ctx u_ty in
              unif ctx t_ty (mk_Prod (u_ty, range));
              (mk_Appl (t, u), Bindlib.subst range u)
          | t_ty ->
              (* XXX Slight variation regarding the rule from Matita *)
              let u, u_ty = infer ctx u in
              let range = LibTerm.Meta.make_codomain ctx u_ty in
              let t = coerce ctx t t_ty (mk_Prod (u_ty, range)) in
              (mk_Appl (t, u), Bindlib.subst range u) )

    (** [noexn f cs ctx args] initialises {!val:constraints} to [cs],
        calls [f ctx args] and returns [Some(r,cs)] where [r] is the value of
        the call to [f] and [cs] is the list of constraints gathered by
        [f]. Function [f] may raise [NotTypable], in which case [None] is
        returned. {!val:constraints} is reset before leaving the function. *)
    let noexn : (ctxt -> 'a -> 'b) -> constr list -> ctxt -> 'a ->
      ('b * constr list) option =
      fun f cs ctx args ->
      Stdlib.(constraints := cs);
      let r =
        try
          let r = Debug.time_of (fun () -> f ctx args) in
          let cs = Stdlib.(!constraints) in
          Some(r, List.rev cs)
        with NotTypable -> None
      in Stdlib.(constraints := []); r

    let infer_noexn cs ctx t =
      if !Debug.log_enabled then
        log "Top infer %a%a" Print.pp_ctxt ctx Print.pp_term t;
      Option.map (fun ((t,a), cs) -> (t, a, cs)) ((noexn infer) cs ctx t)

    let check_noexn cs ctx t a =
      if !Debug.log_enabled then log "Top check \"%a\"" Print.pp_typing
          (ctx, t, a);
      let force ctx (t, a) = force ctx t a in
      (noexn force) cs ctx (t, a)

    let infer : ctxt -> term loc -> term * term =
      fun ctx {pos; elt=t} ->
      match infer_noexn [] ctx t with
      | None -> Error.fatal pos "[%a] is not typable." Print.pp_term t
      | Some(t, a, to_solve) ->
          match L.solve {empty_problem with to_solve} with
            | None -> Error.fatal pos "[%a] is not typable."
                        Print.pp_term t
            | Some [] -> (t, a)
            | Some cs ->
                List.iter
                  (Error.wrn pos "Cannot solve [%a].@\n" Print.pp_constr)
                  cs;
                Error.fatal pos "[%a] is not typable." Print.pp_term a

    let check : ?pos:Pos.pos -> ctxt -> term -> term -> term =
      fun ?pos ctx t a ->
      match check_noexn [] ctx t a with
      | None -> Error.fatal pos "[%a] does not have type [%a]."
                  Print.pp_term t Print.pp_term a
      | Some(t, to_solve) ->
          match L.solve {empty_problem with to_solve} with
            | None -> Error.fatal pos "[%a] does not have type [%a]."
                        Print.pp_term t Print.pp_term a
            | Some [] -> t
            | Some cs ->
                List.iter
                  (Error.wrn pos "Cannot solve [%a].\n" Print.pp_constr)
                  cs;
                Error.fatal pos "[%a] does not have type [%a]."
                  Print.pp_term t Print.pp_term a

    let check_sort : ctxt -> term loc -> term * term =
      fun ctx {elt=t; pos} ->
      Stdlib.(constraints := []);
      let t, a = type_enforce ctx t in
      let to_solve = Stdlib.(!constraints) in
      match L.solve {empty_problem with to_solve} with
      | None -> Error.fatal pos "[%a] is not typable" Print.pp_term t
      | Some [] -> t, a
      | Some cs ->
          List.iter (Error.wrn None "Cannot solve [%a].\n" Print.pp_constr)
            cs;
          Error.fatal pos "[%a] is not typable." Print.pp_term a
  end

(** {1 Preset refiners} *)

(** A refiner without coercion generator nor unification. *)
module Bare =
  Make(struct let coercions = [] let solve _ = None end)

(** A reference to a refiner that can modified by other modules . *)
let default : (module S) Stdlib.ref = Stdlib.ref (module Bare: S)
