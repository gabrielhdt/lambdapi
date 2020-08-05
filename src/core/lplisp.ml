(** Parsing of Lambdapi code as S-expressions.

    Syntax for terms:
    id      ::= alphanum+

    term    ::= id
              | ("lambda" args term)
              | ("prod" args term)
              | ("patt" id)

    command ::= ("symbol" id term)
              | ("definition" id term)
              | ("rule" term term)
              | query
    query ::= "infer" term. *)

(** Parsing of S-expressions. *)
module Sexp : sig

  type t = Atom of string | List of t list
  (** Type of S-expressions: either an atom or a list of sexps,
      [sexp ::= atom | "(" sexp* ")"]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt sexp] prints s-expression [sexp] to formatter [fmt]. *)

  val parse_file : string -> t list
  (** [parse_file file] parses content of file [file]. *)

  val parse_string : string -> t list
  (** [parse_string s] parses string [s]. *)
end = struct
  open MParser

  (** [fix f] computes the fixpoint of [f]. *)
  let rec fix f x = f (fix f) x

  (** [implode cs] concatenates characters in [cs] into a string. *)
  let implode cs =
    let buf = Buffer.create (List.length cs) in
    List.iter (Buffer.add_char buf) cs;
    Buffer.contents buf

  (** Type of S-expressions. *)
  type t =
    | Atom of string
    | List of t list

  let rec pp : Format.formatter -> t -> unit = fun fmt sexp ->
    match sexp with
    | Atom(at) -> Format.pp_print_string fmt at
    | List(s) ->
        Format.pp_print_char fmt '(';
        let pp_sep fmt = Format.pp_print_space fmt in
        Format.pp_print_list ~pp_sep pp fmt s;
        Format.pp_print_char fmt ')'

  let atom : string -> t = fun cs -> Atom(cs)
  let list : t list -> t = fun l -> List(l)

  let ident = many1 (alphanum <|> any_of "-_?!<>=") |>> implode

  let parens = between (char '(') (char ')')

  (* Declared with the fixpoint combinator. *)
  let sexp e = (ident |>> atom <|> ((parens (sep_by e spaces1)) |>> list))
  let sexp = fix sexp

  let sexps = sep_end_by sexp spaces

  let parse_file : string -> t list = fun file ->
    let inchan = open_in file in
    match parse_channel sexps inchan () with
    | Success(exps) -> exps
    | Failed(msg,_) ->
        Format.eprintf ("Parsing error: %s") msg;
        failwith "Parsing error"

  let parse_string : string -> t list = fun s ->
    match parse_string sexps s () with
    | Success(exps) -> exps
    | Failed(msg,_) ->
        Format.eprintf "Parsing error: %s" msg;
        failwith "Parsing error"
end

module Pretty : sig
  val cmd_sexp : Syntax.p_command -> Sexp.t
end = struct
  open Syntax

  let term_sexp : p_term -> Sexp.t = fun _ -> assert false

  let cmd_sexp : p_command -> Sexp.t = fun cmd ->
    match cmd.elt with
    | P_symbol(_,id,_,ty) ->
        Sexp.List([Atom("symbol"); Atom(id.elt); term_sexp ty])
    | _ -> failwith "not implemented"
end

module Parser : sig
  (** [parse_file f] parses file [f]. *)
  val parse_file : string -> Syntax.ast
  val parse_string : string -> Syntax.ast
  val parse_qident : string -> (Syntax.qident, string) result
end = struct

  open Syntax
  open Sexp

  let add_args : p_term -> p_term list -> p_term =
    List.fold_left (fun t u -> Pos.none (P_Appl(t,u)))

  let rec sexp_term : Sexp.t -> p_term = fun sexp ->
    match sexp with
    | Atom("type") -> Pos.none (P_Type)
    | Atom(id) -> Pos.none (P_Iden(Pos.none ([], id), false))
    | List([Atom("lambda"); args; body]) ->
        Pos.none (P_Abst(sexp_args args, sexp_term body))
    | List([Atom("prod"); args; body]) ->
        Pos.none (P_Prod(sexp_args args, sexp_term body))
    | List([Atom("patt"); Atom(id)]) ->
        Pos.none (P_Patt(Some(Pos.none id), None))
    | List(op :: args) ->
        let op = sexp_term op in
        let args = List.map sexp_term args in
        add_args op args
    | List([]) -> failwith "Ill formed term sexp"

  (** [sexp_args s] parses a list of arguments of the form [((x t) (y u) z)]
      where [x], [y] and [z] are identifiers and [t] and [u] are types. *)
  and sexp_args : Sexp.t -> p_arg list = function
    | List(args) -> List.map sexp_arg args
    | atom -> [sexp_arg atom]

  and sexp_arg : Sexp.t -> p_arg = function
    | Atom(id) -> ([Some(Pos.none id)], None, false)
    | List([Atom(id); typ]) ->
        ([Some(Pos.none id)], Some(sexp_term typ), false)
    | _ -> failwith "Ill formed argument."

  let sexp_command : Sexp.t -> Syntax.p_command = fun sexp ->
    match sexp with
    | List([Atom("symbol"); Atom(id); t]) ->
        Pos.none (P_symbol([], Pos.none id, [], sexp_term t))
    | List([Atom("definition"); Atom(id); t]) ->
        Pos.none (P_definition([], false, Pos.none id, [], None, sexp_term t))
    | List([Atom("rule"); lhs; rhs]) ->
        Pos.none(P_rules([Pos.none (sexp_term lhs, sexp_term rhs)]))
    | List([Atom("infer"); t]) ->
        let evalconf = { strategy = SNF; steps = None } in
        Pos.none (P_query(Pos.none (P_query_infer(sexp_term t, evalconf))))
    | s ->
        Format.eprintf "[%a]" Sexp.pp s;
        failwith "Ill formed sexp."

  let parse_file : string -> ast = fun file ->
    let sexps = Sexp.parse_file file in
    List.map sexp_command sexps

  let parse_string : string -> ast = fun s ->
    let sexps = Sexp.parse_string s in
    List.map sexp_command sexps

  let parse_qident : string -> (qident, string) result = fun _ -> assert false
end

include Parser
