open Core

let rec string_of_boolexp t = match t with
  | BTrue -> "true"
  | BFalse -> "false"
  | BIf (t1, t2, t3) -> Printf.sprintf "if %s then %s else %s" (string_of_boolexp t1) (string_of_boolexp t2) (string_of_boolexp t3)

let rec string_of_ts_expr e = match e with
  | TSIden(i) -> "ts-" ^ i
  | TSNum(n) -> "ts-" ^ string_of_int n
  | TSLet(v, ie) -> "ts-let ts-" ^ v ^ " = " ^ string_of_ts_expr ie
  | TSStmtList(ss) -> String.concat "\n" (List.map string_of_ts_expr ss)

let string_of_typed_attr ta =
  Printf.sprintf "%s: %s" ta.name ta.typ

let rec string_of_expr e = match e with
  | TS(tse) -> "ts: " ^ String.concat "\n" (List.map string_of_ts_expr tse)
  | Let(name, body) -> "let " ^ name ^ " = " ^ string_of_expr body
  | Iden(i) -> i
  | Num(n) -> string_of_int n
  | BoolExp(_) -> "boolexp"
  | StmtList(ss) -> String.concat "\n" (List.map string_of_expr ss)
  | Domain(n, defs) -> "domain " ^ n ^ String.concat "\n" (List.map string_of_domain_def defs) ^ "\nend\n"
  | Call(n, args) -> n ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | Env(ss) -> "environment:\n\t" ^ String.concat "\n" (List.map string_of_expr ss) ^ "\nend"
and string_of_domain_def def = match def with
| DomainAttr({ name; typ }) -> Printf.sprintf "%s: %s" name typ
| DomainAction({ aname; body; args}) -> Printf.sprintf "def %s(%s):\n\t%s" aname (String.concat ", " (List.map string_of_typed_attr args)) (string_of_expr body)
