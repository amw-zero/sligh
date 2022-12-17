open Core

module Files = Map.Make(String)

let new_files () = Files.empty

let build_files ps stmt = match stmt with
  | File(e) -> Files.add e.fname e.fbody ps
  | _ -> ps

let print ps =
  Files.iter (fun k v -> Printf.printf "%s -> %s\n" k (Util.string_of_stmt_list v)) ps

let output_str file_name strg =  
  let fname = Printf.sprintf "%s.ts" file_name in
  let open_chan = open_out fname in
  Printf.fprintf open_chan "%s\n" strg;
  close_out open_chan

let output_tsexpr_list file_name tss =
  output_str file_name (String.concat "\n\n" (List.map Codegen.string_of_ts_expr tss))

let output_tsexpr file_name (e: Interpreter.value) =
  match e with
  | VTS(tss) -> output_tsexpr_list file_name tss
  | _ -> print_endline "Unable to generate code for non-TS expr"

let output_file file_name file_body interp_env effect_env =
  let file_expr = Interpreter.evaln file_body interp_env |> Interpreter.val_as_tsexprs in
  let file_expr = Effects.apply file_name effect_env (Core.TS(file_expr)) in
  let file_str = Codegen.string_of_expr file_expr in

  output_str file_name file_str

let output fs interp_env effect_env =
  Files.iter (fun file_name file_body -> output_file file_name file_body interp_env effect_env) fs
