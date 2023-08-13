let rec _compile ast =
  match ast with
  | Draw.Segment(p1, p2) ->
      Printf.sprintf "<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\">"
        (string_of_int p1.x) (string_of_int p1.y) (string_of_int p2.x) (string_of_int p2.y)
  
  | Duplicate(a1) -> 
    let output = _compile a1 in

    Printf.sprintf "%s\n%s" output output
  | _ -> ""

let compile ast =
  Printf.sprintf "<xml>%s</xml>" (_compile ast)