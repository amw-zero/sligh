open Core
open Lexer
open Lexing

let rec string_of_boolexp t = match t with
  | BTrue -> "true"
  | BFalse -> "false"
  | BIf (t1, t2, t3) -> Printf.sprintf "if %s then %s else %s" (string_of_boolexp t1) (string_of_boolexp t2) (string_of_boolexp t3)

let rec string_of_ts_expr e = match e with
  | TSIDen(i) -> i
  | TSNum(n) -> string_of_int n
  | TSLet(v, ie) -> "let " ^ v ^ " = " ^ string_of_ts_expr ie

let rec string_of_expr e = match e with
  | TS(tse) -> "ts: " ^ string_of_ts_expr tse
  | Let(name, body) -> "let " ^ name ^ " = " ^ string_of_expr body
  | Iden(_) -> "iden"
  | Num(n) -> string_of_int n
  | BoolExp(_) -> "boolexp"

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  let ctx = new Lexer.lexer_context in
  try Parser.prog (Lexer.lexer ctx) lexbuf with
  | SyntaxError msg ->
    print_endline "SYntaxErr";
    Printf.printf "%s: %s\n" (print_position lexbuf) msg;
    None
  | Parser.Error ->
    print_endline "ParserErr";
    Printf.printf "%s: syntax error\n" (print_position lexbuf);
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    print_endline "parse_and_print";
    string_of_expr value |> print_endline;
    parse_and_print lexbuf
  | None -> ()

let evaluate_e expr =
  let lexbuf = Lexing.from_string expr in
  parse_and_print lexbuf

let evaluate expr =
  let lexbuf = Lexing.from_string expr in
  let ctx = new Lexer.lexer_context in
  match Parser.prog (Lexer.read ctx) lexbuf with
  | Some value ->
    let parsed = string_of_expr value in
    Printf.printf "Parsed term: %s\n" parsed;
  | None -> ()
