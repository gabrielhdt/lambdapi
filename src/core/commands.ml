(** Command representation.

    This module defines commands using core AST objects. *)

open Terms
open Syntax


(** Representation of an identifier. *)
type ident = string

(** Representation of a possibly qualified identifier *)
type qident = p_module_path * string

(** Representation of an unlocated unary operator. *)
type unop = string * priority * qident

(** Representation of an unlocatied binary operator. *)
type binop = string * assoc * priority * qident

(** Representation of a function argument. The boolean is true if
    the argument is marked as implicit (i.e., between curly braces). *)
type arg = ident option list * term option * bool

(** Rewriting rule representation. *)
type rule = (term * term)

(** Representation of an assertion. *)
type assertion =
  | Assert_typing of term * term
  (** The given term should have the given type. *)
  | Assert_conv   of term * term
  (** The two given terms should be convertible. *)

(** Representation of a query command. *)
type query =
  | Query_verbose   of int
  (** Sets the verbosity level. *)
  | Query_debug     of bool * string
  (** Toggles logging functions described by string according to boolean. *)
  | Query_flag      of string * bool
  (** Sets the boolean flag registered under the given name (if any). *)
  | Query_assert    of bool * assertion
  (** Assertion (must fail if boolean is [true]). *)
  | Query_infer     of term * eval_config
  (** Type inference command. *)
  | Query_normalize of term * eval_config
  (** Normalisation command. *)
  | Query_prover    of string
  (** Set the prover to use inside a proof. *)
  | Query_prover_timeout of int
  (** Set the timeout of the prover (in seconds). *)

(** Rewrite pattern specification. *)
type rw_patt =
  | Rw_Term           of term
  | Rw_InTerm         of term
  | Rw_InIdInTerm     of ident * term
  | Rw_IdInTerm       of ident * term
  | Rw_TermInIdInTerm of term * ident * term
  | Rw_TermAsIdInTerm of term * ident * term

(** Representation of a proof tactic. *)
type tactic =
  | Tac_refine  of term
  (** Refine the current goal using the given term. *)
  | Tac_intro   of ident option list
  (** Eliminate quantifiers using the given names for hypotheses. *)
  | Tac_apply   of term
  (** Apply the given term to the current goal. *)
  | Tac_simpl
  (** Normalize in the focused goal. *)
  | Tac_rewrite of bool * rw_patt option * term
  (** Apply rewriting using the given pattern and equational proof. The
     boolean indicates whether the equation has to be applied from left to
     right. *)
  | Tac_refl
  (** Apply reflexivity of equality. *)
  | Tac_sym
  (** Apply symmetry of equality. *)
  | Tac_focus   of int
  (** Focus on the given goal. *)
  | Tac_print
  (** Print the current goal. *)
  | Tac_proofterm
  (** Print the current proof term (possibly containing open goals). *)
  | Tac_why3    of string option
  (** Try to solve the current goal with why3. *)
  | Tac_query   of query
  (** Query. *)
  | Tac_fail
  (** A tactic that always fails. *)

(** Representation of a configuration command. *)
type config =
  | Config_builtin   of string * ident
  (** Sets the configuration for a builtin syntax (e.g., nat literals). *)
  | Config_unop      of unop
  (** Defines (or redefines) a unary operator (e.g., ["!"] or ["¬"]). *)
  | Config_binop     of binop
  (** Defines (or redefines) a binary operator (e.g., ["+"] or ["×"]). *)
  | Config_ident     of string
  (** Defines a new, valid identifier (e.g., ["σ"], ["€"] or ["ℕ"]). *)
  | Config_quant     of ident
  (** Defines a quantifier symbol (e.g., ["∀"], ["∃"]). *)
  | Config_unif_rule of rule
  (** Unification hint declarations. *)

(** Representation of a statement. *)
type statement = ident * arg list * term

(** Representation of a single command. *)
type command =
  | Require     of bool * p_module_path list
  | Require_as  of p_module_path * (string * bool)
  | Open        of p_module_path list
  | Symbol      of ident * arg list * term
  | Definition  of p_modifier list * bool * ident * arg list *
                    term option * term
  | Inductive   of p_modifier list * ident * term * (ident * term) list
  | Theorem     of p_modifier list * statement * tactic list *
                    p_proof_end
  | Set         of config
  | Query       of query
