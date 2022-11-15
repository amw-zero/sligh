open Core

module Processes = Map.Make(String)

let new_processes () = Processes.empty

let build_procs ps stmt = match stmt with
  | Process(e) -> Processes.add e.ename e.ebody ps
  | _ -> ps

let print ps =
  Processes.iter (fun k v -> Printf.printf "%s -> %s\n" k (Util.string_of_stmt_list v)) ps

let output_tsexpr_list proc_name tss =
  let fname = Printf.sprintf "%s.ts" proc_name in
  let open_chan = open_out fname in
  Printf.fprintf open_chan "%s\n" (String.concat "\n\n" (List.map Codegen.string_of_ts_expr tss));
  close_out open_chan

let output_process proc_name proc_body interp_env =
  let proc_body = Interpreter.evaln proc_body interp_env in

  match proc_body with
  | VTS(tss) -> output_tsexpr_list proc_name tss
  | _ -> print_endline "Unable to generate code for non-TS expr"

let output ps interp_env =
  Processes.iter (fun proc_name proc_body -> output_process proc_name proc_body interp_env) ps