open Core

let rec string_of_boolexp t = match t with
  | BTrue -> "true"
  | BFalse -> "false"
  | BIf (t1, t2, t3) -> Printf.sprintf "if %s then %s else %s" (string_of_boolexp t1) (string_of_boolexp t2) (string_of_boolexp t3)

let rec string_of_expr e = match e with
  | TS(_) -> "ts"
  | Let(name, body) -> "let " ^ name ^ " = " ^ string_of_expr body
  | Iden(_) -> "iden"
  | Num(n) -> string_of_int n
  | BoolExp(_) -> "boolexp"

let evaluate expr =
  let lexbuf = Lexing.from_string expr in
  match Parser.prog Lexer.read lexbuf with
  | Some value ->
    let parsed = string_of_expr value in
    Printf.printf "Parsed term: %s\n" parsed;
  | None -> ()
