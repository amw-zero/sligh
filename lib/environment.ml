open Core

module Env = Map.Make(String)

let new_environment () = Env.empty

let build_env em stmt = match stmt with
  | Env(es) -> List.fold_left (fun m ec -> Env.add ec.ename ec.ebody m) em es
  | _ -> em

let print_env em =
  Env.iter (fun k v -> Printf.printf "%s -> %s\n" k (Util.string_of_stmt_list v)) em

let output_env_process env_name env_body =
  print_endline "Output env body";
  let fname = Printf.sprintf "%s.ts" env_name in
  let open_chan = open_out fname in
  Printf.fprintf open_chan "%s\n" (String.concat "\n\n" (List.map Codegen.string_of_expr env_body));
  close_out open_chan

let output_env em =
  print_endline "Outputting env";
  Env.iter output_env_process em