let filter stmts =
  List.filter_map (fun stmt -> match stmt with
    | Core.Implementation(e) -> (match e with
      | TS(_) -> Some(e)
      | _ -> None)
    | _ -> None) stmts
