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

let parse_to_string expr = 
  let lexbuf = Lexing.from_string expr in
  let stmts = parse_with_error lexbuf in
  let output = List.map (fun e ->  Util.string_of_expr e) stmts in
  String.concat "\n" output
    
let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | stmts ->
    List.iter (fun e ->  Util.string_of_expr e |> print_endline) stmts

let print expr =
  Lexing.from_string expr |> parse_and_print

let compile expr =
  (* TODO: rename domain -> process *)
  let lexbuf = Lexing.from_string expr in
  let init_files = File.new_files () in
  let init_model = Model.new_model () in
  let init_interp_env = Interpreter.new_environment_with_builtins () in

  let stmts = parse_with_error lexbuf in
  
  let model_ast = Model.filter stmts in
  let model_proc = List.fold_left Model.analyze init_model model_ast in
  Model.print_model model_proc;

  (* let impl_expr = Implementation.filter stmts in *)

  let interp_env = List.fold_left Interpreter.build_env init_interp_env stmts in
  let interp_env = Interpreter.add_model_to_env model_proc interp_env in
  Interpreter.print_env interp_env;

  let file_map = List.fold_left File.build_files init_files stmts in
  File.print file_map;
  File.output file_map interp_env

  
