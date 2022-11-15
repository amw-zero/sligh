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
  let statements = parse_with_error lexbuf in
  let output = List.map (fun e ->  Util.string_of_expr e) statements in
  String.concat "\n" output
    
let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | statements ->
    List.iter (fun e ->  Util.string_of_expr e |> print_endline) statements

let print expr =
  Lexing.from_string expr |> parse_and_print

let compile expr =
  let lexbuf = Lexing.from_string expr in
  let init_procs = Process.new_processes () in
  let init_model = Model.new_model () in
  let init_interp_env = Interpreter.new_environment_with_builtins () in

  let statements = parse_with_error lexbuf in
  
  let model = List.fold_left Model.analyze init_model statements in
  Model.print_model model;

  let interp_env = List.fold_left Interpreter.build_env init_interp_env statements in
  let interp_env = Interpreter.add_model_to_env model interp_env in
  Interpreter.print_env interp_env;

  let procs_map = List.fold_left Process.build_procs init_procs statements in
  Process.print procs_map;
  Process.output procs_map interp_env;
