let filter stmts =
  List.filter_map (fun stmt -> match stmt with
    | Core.Implementation(e) -> Some(e)
    | _ -> None) stmts
