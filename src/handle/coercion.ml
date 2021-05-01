open Common
open Pos
open Parsing
open Syntax
open Core
open Term

(** [check cion] runs sanity checks on coercion [cion]. In particular that the
    definition has the mentioned type. *)
let check : Infer.coercion -> unit =
  fun _ ->
  (* Substitute sub-coercions by fresh variables of the appropriate type. *)
  (* Infer check *)
  assert false

(** [handle ss name defn defn_ty source arity requirements] parses and scopes
    the coercion defined by term [defn] of type [defn_ty] coercing on its
    [source]th argument, with a target type of arity [arity]. It requires to
    find coercions in [requirements] to be applied. *)
let handle : Sig_state.t -> p_ident -> p_term -> p_term -> int -> int ->
  (p_ident * p_term) list -> Sig_state.t =
  fun ss name defn defn_ty source arity requirements ->
  let scope_term ?(env=[]) ss =
    Scope.scope_term true ss env (lazy Lplib.Extra.IntMap.empty)
  in
  let defn, reqs = Scope.scope_coercion ss [] defn in
  let defn_ty =
    let module Refiner = (val Stdlib.(!Infer.default)) in
    let ty = scope_term ss defn_ty in
    Refiner.check_sort [] (Pos.make defn_ty.pos ty) |> fst
  in
  let process_req (id, ty) : Infer.prereq =
    match ty.elt with
      | P_Arro(a, b) ->
          let (name, env, coer) =
            List.find (fun (n, _, _) -> n.elt = id.elt)
              (Array.to_list reqs)
          in
          let ty_src =
            scope_term ~env ss a |> lift |>
            Bindlib.bind_mvar (Env.vars env) |> Bindlib.unbox in
          let ty_tgt =
            scope_term ~env ss b |> lift |>
            Bindlib.bind_mvar (Env.vars env) |> Bindlib.unbox
          in
          name, coer, ty_src, ty_tgt
      | _ ->
          Error.fatal ty.pos "Ill-formed requisite type:@ \
                              @[%a@ is not an arrow type@]"
            Pretty.term ty
  in
  let requirements = List.map process_req requirements |> Array.of_list in
  let cion =
    Infer.{ name = name.elt; source; arity; defn; defn_ty ; requirements }
  in
  let module Lookup = struct
    let coercions = cion :: Stdlib.(!Infer.coercions)
    let solve pb = Unif.solve_noexn pb
  end
  in
  Stdlib.((Infer.default) := (module (Infer.Make(Lookup)): Infer.S));
  Stdlib.(Infer.(coercions := cion :: !coercions));
  ss
