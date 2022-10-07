open Lexer
open Lexing

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  let ctx = new Lexer.lexer_context in
  try Parser.prog (Lexer.lexer ctx) lexbuf with
  | SyntaxError msg ->
    print_endline "SyntaxErr";
    Printf.printf "%s: %s\n" (print_position lexbuf) msg;
    []
  | Parser.Error ->
    print_endline "ParserErr";
    Printf.printf "%s: syntax error\n" (print_position lexbuf);
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | statements ->
    List.iter (fun e ->  Util.string_of_expr e |> print_endline) statements;
    parse_and_print lexbuf

let print expr =
  Lexing.from_string expr |> parse_and_print

let _compile lexbuf =
  let statements = parse_with_error lexbuf in

  (* First we analyze the model, extracting information useful to metaprogramming an implementation *)
  let init_model = Model.new_model () in
  let model = List.fold_left Model.analyze init_model statements in
  Model.print_model model

let compile expr =
  let lexbuf = Lexing.from_string expr in
  _compile lexbuf
  