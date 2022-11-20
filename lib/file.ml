open Core

module Files = Map.Make(String)

let new_files () = Files.empty

let build_files ps stmt = match stmt with
  | File(e) -> Files.add e.ename e.ebody ps
  | _ -> ps

let print ps =
  Files.iter (fun k v -> Printf.printf "%s -> %s\n" k (Util.string_of_stmt_list v)) ps

let output_tsexpr_list file_name tss =
  let fname = Printf.sprintf "%s.ts" file_name in
  let open_chan = open_out fname in
  Printf.fprintf open_chan "%s\n" (String.concat "\n\n" (List.map Codegen.string_of_ts_expr tss));
  close_out open_chan

let output_file file_name file_body interp_env =
  let file_expr = Interpreter.evaln file_body interp_env in

  match file_expr with
  | VTS(tss) -> output_tsexpr_list file_name tss
  | _ -> print_endline "Unable to generate code for non-TS expr"

let output fs interp_env =
  Files.iter (fun file_name file_body -> output_file file_name file_body interp_env) fs