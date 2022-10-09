open Core

module Env = Map.Make(String)

let new_environment () = Env.empty

let build_env em stmt = match stmt with
  | Env(es) -> List.fold_left (fun m ec -> Env.add ec.ename ec.ebody m) em es
  | _ -> em

let print_env em =
  Env.iter (fun k v -> Printf.printf "%s -> %s\n" k (Util.string_of_stmt_list v)) em