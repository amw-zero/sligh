open Core

let print_list delim l = String.concat delim l

let rec string_of_boolexp t = match t with
  | BTrue -> "true"
  | BFalse -> "false"
  | BIf (t1, t2, t3) -> Printf.sprintf "if %s then %s else %s" (string_of_boolexp t1) (string_of_boolexp t2) (string_of_boolexp t3)

let string_of_type t = match t with
  | STInt -> "Int"
  | STCustom s -> s
  | STString -> "String"
  | STDecimal -> "Decimal"
  | STVariant(n, _) -> Printf.sprintf "Variant: %s" n
  | STGeneric(g, ts) -> Printf.sprintf "%s(%s)" g (String.concat ", " ts)

let string_of_typed_attr ta =
  Printf.sprintf "%s: %s" ta.name (string_of_type ta.typ)

let string_of_pattern_binding pb = match pb with
  | PBVar(n) -> n
  | PBAny -> "_"

let string_of_variant_tag vt = Printf.sprintf "| %s(%s)" vt.tname (String.concat ", " (List.map string_of_typed_attr vt.tattrs))

(* Debug representation of expressions *)
let rec string_of_expr e = match e with
  | TS(tse) -> "ts: " ^ String.concat "\n" (List.map string_of_ts_expr tse)
  | Let(name, body) -> "let " ^ name ^ " = " ^ string_of_expr body
  | Assignment(name, value) -> Printf.sprintf "%s := %s" name (string_of_expr value)
  | Iden(i, too) -> (match too with
    | Some(t) -> Printf.sprintf "%s: %s" i (string_of_type t)
    | None -> i)
  | Num(n) -> string_of_int n
  | If(e1, e2, e3) -> (match e3 with
    | Some(elseE) -> Printf.sprintf "if  %s:\n %s\nelse:\n  %send" (string_of_expr e1) (string_of_expr e2) (string_of_expr elseE)
    | None -> Printf.sprintf "if %s:\n %s\nend" (string_of_expr e1) (string_of_expr e2))
  | StmtList(ss) -> Printf.sprintf "Stmtlist: %s\nendlist" (string_of_stmt_list ss)
  | Process(n, defs) -> "process " ^ n ^ String.concat "\n" (List.map string_of_proc_def defs) ^ "\nend\n"
  | Entity(n, attrs) -> Printf.sprintf "entity %s\n\t%s" n (print_list "\n" (List.map string_of_typed_attr attrs))
  | Variant(n, vs) -> Printf.sprintf "data %s:\n%s\nend" n (String.concat "\n" (List.map string_of_variant_tag vs))
  | Call(n, args) -> n ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | File(e) -> "file:\n\t" ^ Printf.sprintf "%s: %s\n" e.fname (string_of_stmt_list e.fbody) ^ "\nend"
  | FuncDef({fdname; fdargs; fdbody}) -> Printf.sprintf "def %s(%s):\n\t%s\nend\n" fdname (String.concat ", " (List.map string_of_typed_attr fdargs)) (string_of_stmt_list fdbody)
  | Access(e, i) -> Printf.sprintf "%s.%s" (string_of_expr e) i
  | Implementation(e) -> Printf.sprintf "impl: %s" (string_of_expr e)
  | String(s) -> s
  | Case(e, branches) -> Printf.sprintf "case %s:\n%s\nend" (string_of_expr e) (String.concat "\n\n" (List.map string_of_case_branch branches))
  | Array(es) -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_expr es))
  | Effect(_) -> "To implement: Effect"
and string_of_proc_def def = match def with
| ProcAttr({ name; typ }) -> Printf.sprintf "%s: %s" name (string_of_type typ)
| ProcAction(a) -> string_of_proc_action a
and string_of_proc_action { aname; body; args} = Printf.sprintf "def %s(%s):\n\t%s" aname (String.concat ", " (List.map string_of_typed_attr args)) (String.concat "\n" (List.map string_of_expr body))
and string_of_stmt_list sl = String.concat "\n" (List.map string_of_expr sl)
and string_of_case_branch b = Printf.sprintf "| %s: %s" (string_of_value_pattern b.pattern) (string_of_expr b.value)
and string_of_value_pattern vp = Printf.sprintf "%s(%s)" vp.vname (String.concat ", " (List.map string_of_pattern_binding vp.var_bindings))
and string_of_ts_expr e = match e with
  | TSIden(i) -> string_of_tsiden i
  | TSNum(n) -> "ts-" ^ string_of_int n
  | TSLet(v, ie) -> "ts-let ts-" ^ v ^ " = " ^ string_of_ts_expr ie
  | TSStmtList(ss) -> String.concat "\n" (List.map string_of_ts_expr ss)
  | TSClass(n, ds) -> Printf.sprintf "ts-class %s\n\t%s" n (String.concat "\n" (List.map string_of_tsclassdef ds))
  | TSMethodCall(recv, m, args) -> Printf.sprintf "ts-%s.%s(%s)" recv m (List.map string_of_ts_expr args |> print_list "\n")
  | TSFuncCall(f, args) -> Printf.sprintf "%s(%s)" f (List.map string_of_ts_expr args |> print_list "\n")
  | TSArray(es) -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_ts_expr es))
  | TSString(s) -> s
  | TSIf(e1, e2, e3) -> (match e3 with
    | Some(elseE) -> Printf.sprintf "if (%s) {\n %s}\nelse {\n%s\n}" (string_of_ts_expr e1) (string_of_ts_expr e2) (string_of_ts_expr elseE)
    | None -> Printf.sprintf "if (%s) {\n %s\n}" (string_of_ts_expr e1) (string_of_ts_expr e2))
  | TSAccess(e1, e2) -> Printf.sprintf "%s.%s" (string_of_ts_expr e1) (string_of_ts_expr e2)
  | TSAssignment(e1, e2) -> Printf.sprintf "%s = %s" (string_of_ts_expr e1) (string_of_ts_expr e2)
  | TSInterface(n, attrs) -> Printf.sprintf "ts-interface %s {\n %s\n}" n (String.concat "\n" (List.map string_of_ts_typed_attr attrs))
  | TSClosure(args, body) -> Printf.sprintf "(%s) => {\n  %s\n}" (String.concat ", " (List.map string_of_tsiden args)) (print_list "\n" (List.map string_of_ts_expr body))
  | TSObject(props) -> Printf.sprintf "{%s}" (String.concat ",\n" (List.map string_of_obj_prop props))
  | TSAwait(e) -> Printf.sprintf "await %s" (string_of_ts_expr e)
  | TSAsync(e) -> Printf.sprintf "async %s" (string_of_ts_expr e)
  | TSNew(c, args) -> Printf.sprintf "new %s(%s)" c (String.concat ", " (List.map string_of_ts_expr args))
  | SLSpliceExpr(_) -> "SLSpliceExpr"
  | SLExpr(e) -> string_of_expr e

and string_of_obj_prop p = Printf.sprintf "%s: %s" p.oname (string_of_ts_expr p.oval)

and string_of_tsiden {iname; itype} = match itype with
| Some(t) -> Printf.sprintf "ts-%s: %s" iname (string_of_tstype t)
| None -> Printf.sprintf "ts-%s" iname

and string_of_tsclassdef cd = match cd with
| TSClassProp(n, typ) -> Printf.sprintf "ts-%s: ts-%s" n (string_of_tstype typ)
| TSClassMethod(nm, args, body) -> Printf.sprintf "ts-class-meth %s(%s) {\n\t%s\n}" nm (String.concat "," (List.map string_of_ts_typed_attr args)) (List.map string_of_ts_expr body |> print_list "\n")
| CDSLExpr(_) -> "CDSLExpr remove"

and string_of_ts_typed_attr ta = Printf.sprintf "%s: %s" ta.tsname (string_of_tstype ta.tstyp)

and string_of_tstype tst = match tst with
  | TSTNumber -> "number"
  | TSTCustom c -> c
  | TSTString -> "string"
