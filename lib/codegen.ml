open Core

let print_list l = String.concat "\n" l

let rec string_of_boolexp t = match t with
  | BTrue -> "true"
  | BFalse -> "false"
  | BIf (t1, t2, t3) -> Printf.sprintf "if %s then %s else %s" (string_of_boolexp t1) (string_of_boolexp t2) (string_of_boolexp t3)

let string_of_type t = match t with
  | STInt -> "Int"
  | STCustom s -> s
  | STString -> "String"
  | STDecimal -> "Decimal"

let string_of_typed_attr ta =
  Printf.sprintf "%s: %s" ta.name (string_of_type ta.typ)

let rec string_of_ts_expr e = match e with
  | TSIden(i, t) -> (match t with
    | Some(t) -> Printf.sprintf "%s: %s" i (string_of_tstype t)
    | None -> Printf.sprintf "%s" i)
  | TSNum(n) -> string_of_int n
  | TSLet(v, ie) -> Printf.sprintf "let %s = %s" v (string_of_ts_expr ie)
  | TSStmtList(ss) -> String.concat "\n" (List.map string_of_ts_expr ss)
  | TSClass(n, ds) -> Printf.sprintf "class %s{%s}" n (String.concat "\n" (List.map string_of_tsclassdef ds))
  | TSMethodCall(recv, m, args) -> Printf.sprintf "%s.%s(%s)" recv m (List.map string_of_ts_expr args |> print_list)
  | TSArray(es) -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_ts_expr es))
  | TSString(s) -> Printf.sprintf "\"%s\"" s
  | SLExpr(_) -> "SLExpr"

and string_of_tstype tst = match tst with
  | TSTNumber -> "number"
  | TSTCustom c -> c
  | TSTString -> "string"

and string_of_ts_typed_attr ta = Printf.sprintf "%s: %s" ta.tsname (string_of_tstype ta.tstyp)

and string_of_tsclassdef cd = match cd with
  | TSClassProp(n, typ) -> Printf.sprintf "%s: %s" n (string_of_tstype typ)
  | TSClassMethod(nm, args, body) -> Printf.sprintf "%s(%s) { %s }" nm (String.concat ", " (List.map string_of_ts_typed_attr args)) (List.map string_of_ts_expr body |> print_list)
  | CDSLExpr(_) -> "CDSLExpr remove"

  (* Only supporting codegen to TS right now *)
let rec string_of_expr e = match e with
  | Let(name, body) -> Printf.sprintf "let %s = %s;\n" name (string_of_expr body)
  | Iden(i, too) -> (match too with
    | Some(t) -> Printf.sprintf "%s: %s" i (string_of_type t)
    | None -> i)
  | Num(n) -> string_of_int n
  | BoolExp(_) -> "boolexp"
  | StmtList(ss) -> string_of_stmt_list ss
  | Process(n, defs) -> Printf.sprintf "class %s {\n  %s\n}" n (String.concat "\n" (List.map string_of_proc_def defs)) 
  | Entity(n, attrs) -> Printf.sprintf "interface %s {\n\t%s\n}" n (print_list (List.map string_of_typed_attr attrs))
  | Call(n, args) -> n ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | FuncDef({fdname; fdargs; fdbody}) -> Printf.sprintf "function %s(%s):\n\t%s\nend\n" fdname (String.concat ", " (List.map string_of_typed_attr fdargs)) (string_of_stmt_list fdbody)
  | Access(e, i) -> Printf.sprintf "%s.%s" (string_of_expr e) i
  | _ -> failwith (Printf.sprintf "Unable to generate code for expr: %s" (Util.string_of_expr e))
  and string_of_proc_def def = match def with
  | ProcAttr({ name; typ }) -> Printf.sprintf "%s: %s" name (string_of_type typ)
  | ProcAction({ aname; body; args}) -> Printf.sprintf "%s(%s) {\n\t%s\n}" aname (String.concat ", " (List.map string_of_typed_attr args)) (string_of_expr body)
  and string_of_stmt_list sl =
    let rev_list = List.rev sl in
    let ret_stmt = List.hd rev_list in 
    let rest = List.tl rev_list in
    let ret_str = Printf.sprintf "return %s;" (string_of_expr ret_stmt) in
    let rest_strs = List.map string_of_expr rest in
    let all_strs = ret_str :: rest_strs in

    String.concat "\n" (List.rev all_strs)

let string_of_model model_ast = String.concat "\n\n" (List.map string_of_expr model_ast)