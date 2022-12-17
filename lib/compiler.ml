let compile lexbuf =
  let init_files = File.new_files () in
  let init_process = Process.new_process () in
  let init_interp_env = Interpreter.new_environment_with_builtins () in
  let init_effect_env = Effects.new_effect_env () in
  let stmts = Parse.parse_with_error lexbuf in

  let effect_env = List.fold_left Effects.analyze init_effect_env stmts in
  Effects.print_env effect_env;
  
  (* Extract and convert Model to Process *)
  let model_ast = Process.filter_model stmts in

  let model_proc = List.fold_left Process.analyze_model init_process model_ast in
  print_endline "Model process:";
  Process.print_process model_proc;
  print_endline "";

  let model_ast = List.map (fun mstmt -> Effects.apply "model" effect_env mstmt) model_ast in
  File.output_str "model" (Codegen.string_of_model model_ast);

  (* Extract Impl *)
  let impl_expr = Implementation.filter stmts in 

  let interp_env = List.fold_left Interpreter.build_env init_interp_env stmts in
  let interp_env = Interpreter.add_model_to_env model_proc interp_env in

  (* Macroexpand and output Impl *)
  let evaled_impl = Interpreter.evaln impl_expr interp_env |> Interpreter.val_as_tsexprs in

  Printf.printf "After macro expansion: %s" (String.concat "\n\n" (List.map Util.string_of_ts_expr evaled_impl));


  let evaled_impl = Effects.apply "impl" effect_env (Core.TS(evaled_impl)) in

  let impl_str = Codegen.string_of_expr evaled_impl in
  File.output_str "impl" impl_str;

  (* Macroexpand and output auxiliary files *)
  (* apply effects to files too *)
  let file_map = List.fold_left File.build_files init_files stmts in
  File.print file_map;
  File.output file_map interp_env;

  (* To improve: return generated filenames in previous steps *)
  Certification.generate model_proc "model.ts" "impl.ts" interp_env

let compile_str expr =
  Lexing.from_string expr |> compile
  
let compile_file fn =
  let fh = open_in fn in
  let lex = Lexing.from_channel fh in
  lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};

  let res = compile lex in
  close_in fh;

  res

let interp str =
  let lexbuf = Lexing.from_string str in
  let init_interp_env = Interpreter.new_environment_with_builtins () in
  let stmts = Parse.parse_with_error lexbuf in
  
  let interp_env = List.fold_left Interpreter.build_env init_interp_env stmts in
  let last =  List.rev stmts |> List.hd in
  Interpreter.eval last interp_env |> fst
  