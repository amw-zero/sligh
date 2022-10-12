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
  let init_env_map = Environment.new_environment () in
  let init_model = Model.new_model () in
  let init_interp_env = Interpreter.new_environment () in

  let statements = parse_with_error lexbuf in
  
  let model = List.fold_left Model.analyze init_model statements in
  Model.print_model model;

  let interp_env = List.fold_left Interpreter.build_env init_interp_env statements in
  Interpreter.print_env interp_env;

  (* 
    It's important to compile the Environment last, because Environment's will be
    derived from the Model, so the Model has to be analyzed and built first. Environment's
    will also likely have interpreted Sligh code, so that all needs to be executable, i.e.
    a proper Interpreter.Env needs to be set up.
  *)
  let env_map = List.fold_left Environment.build_env init_env_map statements in
  Environment.print_env env_map;
  Environment.output_env env_map interp_env;

