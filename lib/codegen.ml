open Core

let print_list delim  l = String.concat delim l

let string_of_generic_type n = match n with
  | "Set" -> "Array"
  | _ -> failwith (Printf.sprintf "Unknown generic type mapping: %s" n)

let rec string_of_type t = match t with
  | STInt -> "number"
  | STCustom s -> s
  | STString -> "string"
  | STDecimal -> "number"
  | STVariant(n, _) -> Printf.sprintf "Variant: %s" n
  | STGeneric(n, ts) -> Printf.sprintf "%s<%s>" (string_of_generic_type n) (String.concat ", " (List.map string_of_type ts))

let string_of_typed_attr ta =
  Printf.sprintf "%s: %s" ta.name (string_of_type ta.typ)

let string_of_variant_tag vt =
  Printf.sprintf "export type %s = {\n%s\n}"
    vt.tname
    (String.concat "\n" ((Printf.sprintf "type: \"%s\";" vt.tname) :: (List.map string_of_typed_attr vt.tattrs)))

let string_of_symbol_import si = match si.alias with
  | Some(a) -> Printf.sprintf "%s as %s" si.symbol a
  | None -> si.symbol

(* attrs are state variables of current process context *)
let string_of_iden i attrs =
  match List.find_opt (fun attr -> attr.name = i) attrs with
  | Some(_) -> Printf.sprintf "this.%s" i
  | None -> i

  (* Only supporting codegen to TS right now *)
let rec string_of_expr e attrs env = match e with
  | Let(name, body) -> Printf.sprintf "let %s = %s;" name (string_of_expr body attrs env)
  | Assignment(name, value) ->
    Printf.sprintf "this.%s = %s" name (string_of_expr value attrs env)
  | Iden(i, too) -> (match too with
    | Some(t) -> Printf.sprintf "%s: %s" i (string_of_type t)
    | None -> string_of_iden i attrs)
  | Num(n) -> string_of_int n
  | Bool(b) -> string_of_bool b
  | If(e1, e2, e3) -> (match e3 with
    | Some(else_e) ->
      let cond = string_of_expr e1 attrs env in
      let then_str = string_of_expr e2 attrs env in
      let else_str = string_of_expr else_e attrs env in

      Printf.sprintf {|
      (() => {
        if (%s) {
          return %s
        } else {
          return %s
        }
      })()
      |} cond then_str else_str
    | None -> Printf.sprintf "if %s:\n %s\nend" (string_of_expr e1 attrs env) (string_of_expr e2 attrs env))
  | StmtList(ss) -> string_of_stmt_list ss attrs env
  | Process(n, defs) -> 
    (* This is a little more complicated than necessary because state variables are compiled to member variables
       and there's no way to know if an identifier is a state variable without referencing the lsit of process 
       variabless. Separating concrete from abstract syntax would improve this. *)
    let attrs = Process.filter_attrs defs in

    Printf.sprintf "export class %s {\n %s\n  %s\n}" n (process_constructor defs)
      (String.concat "\n" (List.map (fun d -> string_of_proc_def d attrs env) defs))
  | Entity(n, eattrs) ->  Printf.sprintf "export interface %s {\n\t%s\n}" n (print_list "\n" (List.map string_of_typed_attr eattrs))
  | Variant(n, vs) ->
    Printf.sprintf "export type %s = %s\n\n%s"
      n
      (String.concat " | " (List.map (fun vt -> vt.tname) vs))
      (String.concat "\n" (List.map string_of_variant_tag vs))
  | Call(name, args) ->
    if List.exists (fun n -> n = name) Interpreter.builtin_funcs then
      string_of_builtin name args attrs env
    else 
      name ^ "(" ^ String.concat ", " (List.map (fun a -> string_of_expr a attrs env) args) ^ ")"
  | FuncDef({fdname; fdargs; fdbody}) -> 
    Printf.sprintf "function %s(%s) {\n\t%s\n}\n"
      fdname
      (String.concat ", "
        (List.map string_of_typed_attr fdargs))
      (string_of_stmt_list fdbody attrs env)
  | Access(e, i) -> Printf.sprintf "%s.%s" (string_of_expr e attrs env) i
  | String(s) -> Printf.sprintf "\"%s\"" s
  | TS(tses) -> String.concat "\n\n" (List.map (fun e -> string_of_ts_expr e env) tses)
  | _ -> failwith (Printf.sprintf "Unable to generate code for expr: %s" (Util.string_of_expr e))
and string_of_proc_def def attrs env = match def with
| ProcAttr({ name; typ }) -> Printf.sprintf "%s: %s" name (string_of_type typ)
| ProcAction({ aname; body; args}) -> 
    Printf.sprintf "%s(%s) {\n\t%s\n}" 
      aname
      (String.concat ", " (List.map string_of_typed_attr args))
      (String.concat "\n" (List.map (fun e -> string_of_expr e attrs env) body))

and expr_as_str e attrs env = match e with
  | Iden(i, _) -> i
  | _ -> failwith (Printf.sprintf "Can't convert expr to string: %s" (string_of_expr e attrs env))

and string_of_stmt_list sl attrs env =
  let rev_list = List.rev sl in
  let ret_stmt = List.hd rev_list in 
  let rest = List.tl rev_list in
  let ret_str = Printf.sprintf "let ret = %s; return ret;" (string_of_expr ret_stmt attrs env) in
  let rest_strs = List.map (fun e -> string_of_expr e attrs env) rest in
  let all_strs = ret_str :: rest_strs in

  String.concat "\n" (List.rev all_strs)

and string_of_builtin n args attrs (env: Env.env) =
  match n with
  | "new" ->
    (* More hassle than necessary to get schema definitions.  *)
    let schema_name = expr_as_str (List.nth args 0) attrs env in
    let schema_names = Env.SchemaEnv.find schema_name env.schemas |> List.map (fun ta -> ta.name) in
    let bound_pairs = List.combine schema_names (List.tl args) in

    Printf.sprintf "{ %s }"
      (String.concat ", "
        (List.map
          (fun (prop_name, prop_value) ->
            Printf.sprintf "%s: %s" prop_name (string_of_expr prop_value attrs env))
          bound_pairs))
  | "append" -> 
    let arr = string_of_expr (List.nth args 0) attrs env in
    let elem = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    (() => {
    let a = [...%s];
    a.push(%s);

    return a;
    })();
    |} arr elem
  | "delete" ->
    let arr = string_of_expr (List.nth args 0) attrs env in
    let elem = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    %s.filter((e) => e.id === %s)
    |} arr elem
  | "map" ->
    let arr = string_of_expr (List.nth args 0) attrs env in
    let map_func = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    %s.map((a) => %s(a))
    |} arr map_func
  | "equals" ->
    let larg = string_of_expr (List.nth args 0) attrs env in
    let rarg = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    %s === %s
    |} larg rarg
  | _ -> failwith (Printf.sprintf "Attempted to compile unknown builtin func: %s" n)  

and string_of_ts_expr e env = match e with
  | TSIden({iname; itype}) -> (match itype with
    | Some(t) -> Printf.sprintf "%s: %s" iname (string_of_tstype t)
    | None -> Printf.sprintf "%s" iname)
  | TSNum(n) -> string_of_int n
  | TSBool(b) -> string_of_bool b
  | TSIf(e1, e2, e3) -> (match e3 with
    | Some(elseE) -> Printf.sprintf "if (%s) {\n %s}\nelse {\n%s\n}" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env) (string_of_ts_expr elseE env)
    | None -> Printf.sprintf "if (%s) {\n %s\n}" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env))
  | TSLet(v, ie) -> Printf.sprintf "let %s = %s" v (string_of_ts_expr ie env)
  | TSStmtList(ss) -> String.concat "\n" (List.map (fun e -> string_of_ts_expr e env) ss)
    (* let rev_list = List.rev ss in
    let ret_stmt = List.hd rev_list in 
    let rest = List.tl rev_list in
    let ret_str = Printf.sprintf "return %s;" (string_of_ts_expr ret_stmt env) in
    let rest_strs = List.map (fun e -> string_of_ts_expr e env) rest env in
    let all_strs = ret_str :: rest_strs in

    String.concat "\n" (List.rev all_strs) *)
  | TSClass(n, ds) -> Printf.sprintf "class %s{%s}" n (String.concat "\n" (List.map (fun d -> string_of_tsclassdef d env) ds))
  | TSMethodCall(recv, m, args) -> Printf.sprintf "%s.%s(%s)" recv m (List.map (fun e -> string_of_ts_expr e env) args |> print_list ", ")
  | TSFuncCall(f, args) -> Printf.sprintf "%s(%s)" f (List.map (fun e -> string_of_ts_expr e env) args |> print_list ", ")
  | TSArray(es) -> Printf.sprintf "[%s]" (String.concat ", " (List.map (fun e -> string_of_ts_expr e env) es))
  | TSReturn(e) -> Printf.sprintf "return %s" (string_of_ts_expr e env)
  | TSString(s) -> Printf.sprintf "\"%s\"" s
  | TSAccess(e1, e2) -> Printf.sprintf "%s.%s" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env)
  | TSAssignment(e1, e2) -> Printf.sprintf "%s = %s;" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env)
  | TSInterface(n, attrs) -> Printf.sprintf "interface %s {\n %s\n}" n (String.concat "\n" (List.map string_of_ts_typed_attr attrs))
  | TSClosure(args, body, is_async) -> if is_async then
    Printf.sprintf "async (%s) => {\n  %s\n}" (String.concat ", " (List.map string_of_tsparam args)) (print_list "\n" (List.map (fun e -> string_of_ts_expr e env) body))
  else
    Printf.sprintf "(%s) => {\n  %s\n}" (String.concat ", " (List.map string_of_tsparam args)) (print_list "\n" (List.map (fun e -> string_of_ts_expr e env) body))
  | TSAwait(e) -> Printf.sprintf "await %s" (string_of_ts_expr e env)
  | TSExport(e) -> Printf.sprintf "export %s" (string_of_ts_expr e env)
  | TSAliasImport(imports, file) -> Printf.sprintf "import { %s } from \"%s\";" (String.concat ", " (List.map string_of_symbol_import imports)) file
  | TSDefaultImport(import, file) -> Printf.sprintf "import %s from \"%s\";" import file
  | TSObject(props) -> Printf.sprintf "{%s}" (String.concat ",\n" (List.map (fun p -> string_of_obj_prop p env) props))
  | TSNew(c, args) -> Printf.sprintf "new %s(%s)" c (String.concat ", " (List.map (fun e -> string_of_ts_expr e env) args))
  | SLSpliceExpr(_) -> "SLSpliceExpr"
  (* Process context gets broken here - might be ok *)
  | SLExpr(e) -> string_of_expr e [] env

and string_of_obj_prop p env = Printf.sprintf "%s: %s" p.oname (string_of_ts_expr p.oval env)

and string_of_tsparam tsp = match tsp with
| TSPTypedAttr(ta) -> string_of_ts_typed_attr ta
| TSPObjectPat(op) -> string_of_tsobject_pat op

and string_of_tsobject_pat op = 
  Printf.sprintf "{ %s }: %s" (String.concat ", " (List.map string_of_tsobject_pat_prop op.opprops)) (string_of_tstype op.optyp)

and string_of_tsobject_pat_prop opp = Printf.sprintf "%s: %s" opp.oppname opp.oppvalue  

and string_of_tsiden {iname; itype} = match itype with
  | Some(t) -> Printf.sprintf "%s: %s" iname (string_of_tstype t)
  | None -> Printf.sprintf "%s" iname  

and string_of_tstype tst = match tst with
  | TSTNumber -> "number"
  | TSTCustom c -> c
  | TSTString -> "string"
  | TSTGeneric(n, types) -> Printf.sprintf "%s<%s>"
    n
    (String.concat ", " (List.map string_of_tstype types))

and string_of_ts_typed_attr ta = Printf.sprintf "%s: %s" ta.tsname (string_of_tstype ta.tstyp)

and string_of_tsclassdef cd env = match cd with
  | TSClassProp(n, typ) -> Printf.sprintf "%s: %s" n (string_of_tstype typ)
  | TSClassMethod(nm, args, body, is_async) -> if is_async then
      Printf.sprintf "async %s(%s) { %s }" nm (String.concat ", " (List.map string_of_ts_typed_attr args)) (List.map (fun e -> string_of_ts_expr e env) body |> print_list "\n")
    else
      Printf.sprintf "%s(%s) { %s }" nm (String.concat ", " (List.map string_of_ts_typed_attr args)) (List.map (fun e -> string_of_ts_expr e env) body |> print_list "\n")
  | CDSLExpr(_) -> "CDSLExpr remove"  
and process_constructor defs = 
  let attrs = Process.filter_attrs defs in
  let ctor_args = String.concat ", " (List.map string_of_typed_attr attrs) in
  let ctor_body = String.concat "\n" (List.map (fun attr -> 
      Printf.sprintf "this.%s = %s;" attr.name attr.name) attrs) in

  Printf.sprintf "constructor(%s) {\n  %s\n}" ctor_args ctor_body  

let string_of_model model_ast env = String.concat "\n\n" (List.map (fun e -> string_of_expr e [] env) model_ast)

let rec tstype_of_sltype typ = match typ with
  | Some(t) -> (match t with
    | STInt -> Some(TSTNumber)
    | STString -> Some(TSTString)
    | STDecimal -> Some(TSTNumber)
    | STCustom(c) -> Some(TSTCustom(c))
    | STVariant(n, _) -> Some(TSTCustom(n))
    | STGeneric(n, types) -> Some(TSTGeneric(n, List.filter_map (fun t -> tstype_of_sltype (Some(t))) types)))
  | None -> None

(* Currently unused, but converts a Sligh expression to a TS one *)
let rec tsexpr_of_expr e = match e with
  | Let(var, e) -> TSLet(var, tsexpr_of_expr e)
  | StmtList(es) -> TSStmtList(List.map tsexpr_of_expr es)
  | Iden(name, typ) -> TSIden({iname=name; itype=tstype_of_sltype typ})
  | Num(i) -> TSNum(i)
  | Bool(b) -> TSBool(b)
  | If(e1, e2, e3) -> (match e3 with
    | Some(elseE) -> TSIf(tsexpr_of_expr e1, tsexpr_of_expr e2, Some(tsexpr_of_expr elseE))
    | None -> TSIf(tsexpr_of_expr e1, tsexpr_of_expr e2, None))
  | Array(es) -> TSArray(List.map tsexpr_of_expr es)
  | String(s) -> TSString(s)
  | Assignment(v, e) -> TSAssignment(TSIden({iname=Printf.sprintf "this.%s" v;itype=None}), tsexpr_of_expr e)

  (* Unsure about this - why doesn't Access have an expr on the right hand side? *)
  | Access(e, accessor) -> TSAccess(tsexpr_of_expr e, TSIden({iname=accessor; itype=None}))

  (* Unsure if this should be StmtList *)
  | TS(tses) -> TSStmtList(tses)

  (* Let the Codegen engine handle calls for now, because of UCS hard to tell if func or method call*)
  | Call(name, args) -> TSFuncCall(name, List.map tsexpr_of_expr args)

  (* Not handling these, but should *)
  | FuncDef(_) -> failwith "Not handling FuncDef to TS"
  | Case(_, _) -> failwith "Not handling Case to TS"
  
  (* Not handling these, and probably should never *)
  | Effect(_) -> failwith "Not handling Effect to TS"
  | Implementation(_) -> failwith "Not handling Implementation to TS"
  | File(_) -> failwith "Not handling File to TS"
  | Process(_, _) -> failwith "Not handling Process to TS - maybe convert to class"
  | Entity(_, _) -> failwith "Not handling Entity to TS"
  | Variant(_, _) -> failwith "Not handling Variant to TS"
