let compile expr =
  let lexbuf = Lexing.from_string expr in
  let init_files = File.new_files () in
  let init_process = Process.new_process () in
  let init_interp_env = Interpreter.new_environment_with_builtins () in

  let stmts = Parse.parse_with_error lexbuf in
  
  (* Extract and convert Model to Process *)
  let model_ast = Process.filter_model stmts in
  File.output_str "model" (Codegen.string_of_model model_ast);

  let model_proc = List.fold_left Process.analyze_model init_process model_ast in
  print_endline "Model process:";
  Process.print_process model_proc;
  print_endline "";

  (* Extract Impl *)
  let impl_expr = Implementation.filter stmts in 

  let interp_env = List.fold_left Interpreter.build_env init_interp_env stmts in
  let interp_env = Interpreter.add_model_to_env model_proc interp_env in

  (* Macroexpand and output Impl *)
  let evaled_impl = Interpreter.evaln impl_expr interp_env in
  File.output_tsexpr "impl" evaled_impl;

  (* Macroexpand and output auxiliary files *)
  let file_map = List.fold_left File.build_files init_files stmts in
  File.print file_map;
  File.output file_map interp_env

  (* To improve: return generated filenames in previous steps *)
  (* Certification.generate model_proc "model.ts" "impl.ts" interp_env *)

let interp str =
  let lexbuf = Lexing.from_string str in
  let init_interp_env = Interpreter.new_environment_with_builtins () in
  let stmts = Parse.parse_with_error lexbuf in
  
  let interp_env = List.fold_left Interpreter.build_env init_interp_env stmts in
  let last =  List.rev stmts |> List.hd in
  Interpreter.eval last interp_env |> fst
  