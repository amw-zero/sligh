(* Compiles and performs certifying model transformation *)
let compile lexbuf impl_out cert_out =
  let init_files = File.new_files () in
  let init_process = Process.new_process () in
  let init_interp_env = Interpreter.new_environment_with_builtins () in
  let init_effect_env = Effects.new_effect_env () in
  let stmts = Parse.parse_with_error lexbuf in

  let effect_env = List.fold_left Effects.analyze init_effect_env stmts in
  (* Effects.print_env effect_env; *)
  
  (* Extract and convert Model to Process *)
  let model_ast = Process.filter_model stmts in

  let model_proc = List.fold_left Process.analyze_model init_process model_ast in
  (* print_endline "Model process:";
  Process.print_process model_proc;
  print_endline ""; *)

  let interp_env = List.fold_left Interpreter.build_env init_interp_env stmts in

  let model_ast = List.filter_map (fun mstmt -> Effects.apply "model" effect_env interp_env mstmt) model_ast in
  (* Consider converting to TS via tsexpr_of_expr here first *)
  File.output_str "model" (Codegen.string_of_model model_ast);

  (* Extract Impl *)
  let impl_expr = Implementation.filter stmts in

  let interp_env = Interpreter.add_model_to_env model_proc interp_env in

  (* Macroexpand and output Impl *)
  let evaled_impl = Interpreter.evaln impl_expr interp_env |> Interpreter.val_as_tsexprs in
  (* Printf.printf "|--- Impl AST before ME ---|: %s\n|--- end impl ---|\n\n" (String.concat "\n" (List.map Util.string_of_ts_expr evaled_impl)); *)

  let evaled_impl = Effects.apply "impl" effect_env interp_env (Core.TS(evaled_impl)) |> Option.get in
  (* Printf.printf "|--- Impl AST after ME ---|: %s\n|--- end impl ---|\n\n" (Util.string_of_expr evaled_impl); *)

  let impl_str = Codegen.string_of_expr evaled_impl in
  File.output_str impl_out impl_str;

  (* Macroexpand and output auxiliary files *)
  (* apply effects to files too *)
  let file_map = List.fold_left File.build_files init_files stmts in
  (* File.print file_map; *)
  File.output file_map interp_env effect_env;

  (* To improve: return generated filenames in previous steps *)
  Certification.generate model_proc "model.ts" "impl.ts" cert_out interp_env

(* Compilers certifying specification *)
let compile_spec input_file impl_in cert_out =
  let fh = open_in input_file in
  let lex = Lexing.from_channel fh in
  lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = input_file};

  let init_process = Process.new_process () in
  let init_interp_env = Interpreter.new_environment_with_builtins () in
  let stmts = Parse.parse_with_error lex in
  let interp_env = List.fold_left Interpreter.build_env init_interp_env stmts in
  let model_ast = Process.filter_model stmts in
  let model_proc = List.fold_left Process.analyze_model init_process model_ast in
  let interp_env = Interpreter.add_model_to_env model_proc interp_env in

  File.output_str "model" (Codegen.string_of_model model_ast);

  close_in fh;
  Certification.generate_spec "model.ts" model_proc impl_in cert_out interp_env

let compile_str expr =
  compile (Lexing.from_string expr) "impl.ts" "refine_cert.ts"

let compile_file fn impl_out cert_out =
  let fh = open_in fn in
  let lex = Lexing.from_channel fh in
  lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};

  let res = compile lex impl_out cert_out in
  close_in fh;

  res

let compile_cert_model_transform = compile_file

(* Model transformation can probably replace the other more structured approaches *)
let compile_model_transform input_file transform_script out_file =
  let fh = open_in input_file in
  let lex = Lexing.from_channel fh in
  lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = input_file};

  let init_process = Process.new_process () in
  let init_interp_env = Interpreter.new_environment_with_builtins () in
  let model_ast = Parse.parse_with_error lex in
  let (_, interp_env) = List.fold_left (fun (_, env) s -> Interpreter.eval s env) (VVoid, init_interp_env) model_ast in

  let model_proc = List.fold_left Process.analyze_model init_process model_ast in

  let interp_env = Interpreter.add_model_to_env model_proc interp_env in

  let transform_fh = open_in transform_script in
  let trans_lex = Lexing.from_channel transform_fh in
  trans_lex.Lexing.lex_curr_p <- {trans_lex.Lexing.lex_curr_p with Lexing.pos_fname = transform_script};
  let trans_ast = Parse.parse_with_error trans_lex in

  let (code, _) = List.fold_left
    (fun (code_vals, env) stmt ->
      let (value, next_env) = Interpreter.eval stmt env in
      match value with
      | Interpreter.VTS(tss) -> (tss :: code_vals, next_env)
      | _ -> (code_vals, next_env)
    )
    ([] , interp_env)
    trans_ast in

  let _ = List.iter (fun tss -> File.output_tsexpr_list out_file tss) code in

  close_in fh;
  close_in transform_fh

let compile_model input_file out_file = 
  let fh = open_in input_file in
  let lex = Lexing.from_channel fh in
  lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = input_file};
  let model_ast = Parse.parse_with_error lex in
  let compiled = Codegen.string_of_model model_ast in

  File.output_str out_file compiled

let interp str =
  let lexbuf = Lexing.from_string str in
  let init_interp_env = Interpreter.new_environment_with_builtins () in
  let stmts = Parse.parse_with_error lexbuf in
  
  let interp_env = List.fold_left Interpreter.build_env init_interp_env stmts in
  let last =  List.rev stmts |> List.hd in
  Interpreter.eval last interp_env |> fst  
  