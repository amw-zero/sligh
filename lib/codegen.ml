open Core

let print_list delim  l = String.concat delim l

let string_of_generic_type n = match n with
  | "Set" -> "Array"
  | _ -> n

let rec string_of_type t = match t with
  | STInt -> "number"
  | STCustom s -> s
  | STString -> "string"
  | STDecimal -> "number"
  | STBool -> "boolean"
  | STVariant(n, _) -> Printf.sprintf "Variant: %s" n
  | STGeneric(n, ts) -> (match n with
    | "Id" -> string_of_type (List.nth ts 0)
    | _ -> Printf.sprintf "%s<%s>" (string_of_generic_type n) (String.concat ", " (List.map string_of_type ts)))

let string_of_typed_attr ta =
  Printf.sprintf "%s: %s" ta.name (string_of_type ta.typ)

let string_of_variant_tag vt =
  Printf.sprintf "export type %s = {\n%s\n}"
    vt.tname
    (String.concat "\n" ((Printf.sprintf "type: \"%s\";" vt.tname) :: (List.map string_of_typed_attr vt.tattrs)))

let string_of_symbol_import si = match si.alias with
  | Some(a) -> Printf.sprintf "%s as %s" si.symbol a
  | None -> si.symbol

let default_val_for_type t = match t with
| STInt -> Num(0)
| STCustom _ -> failwith "Not implemented: default val for STCustom"
| STString -> String("")
| STBool -> Bool(false)
| STGeneric(n, _) -> (match n with
  | "Set" -> Array([])
  | _ -> failwith "Not implemented: default val for STGeneric")
| STDecimal -> Num(0)
| STVariant(_, _) -> failwith "Not implemented: default value for STVariant"

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
  | Plus(e1, e2) -> Printf.sprintf "%s + %s" (string_of_expr e1 attrs env) (string_of_expr e2 attrs env)
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
       and there's no way to know if an identifier is a state variable without referencing the list of process 
       variables. Separating concrete from abstract syntax would improve this. *)
    let attrs = Process.filter_attrs defs in

    Printf.sprintf "export class %s {\n %s\n  %s\n}" n (process_constructor defs env)
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
  | Array(es) -> Printf.sprintf "[%s]" (String.concat ", " (List.map (fun e -> string_of_expr e [] env) es))
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
    %s.filter((e) => e.id !== %s)
    |} arr elem
  | "map" ->
    let arr = string_of_expr (List.nth args 0) attrs env in
    let map_func = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    %s.map((a) => %s(a))
    |} arr map_func
  | "update" ->
    let arr = string_of_expr (List.nth args 0) attrs env in
    let finder = string_of_expr (List.nth args 1) attrs env in
    let updater = string_of_expr (List.nth args 2) attrs env in

    Printf.sprintf {|
    (() => {
      const index = %s.findIndex((a) => %s(a));
      if (index === -1) {
        return %s
      }
      let ret = [...%s];
      ret[index] = %s(ret[index]);

      return ret;
    })();
    |} arr finder arr arr updater
  | "equals" ->
    let larg = string_of_expr (List.nth args 0) attrs env in
    let rarg = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    %s === %s
    |} larg rarg
  | "equalsStr" ->
    let larg = string_of_expr (List.nth args 0) attrs env in
    let rarg = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    %s === %s
    |} larg rarg

  | "notEqualsStr" ->
    let larg = string_of_expr (List.nth args 0) attrs env in
    let rarg = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    %s !== %s
    |} larg rarg    
  | "index" ->
    let larg = string_of_expr (List.nth args 0) attrs env in
    let rarg = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    %s[%s]
    |} larg rarg
  | "filter" ->
    let larg = string_of_expr (List.nth args 0) attrs env in
    let rarg = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    %s.filter(%s)
    |} larg rarg
  | _ -> failwith (Printf.sprintf "Attempted to compile unknown builtin func: %s" n)  

and string_of_ts_expr e env = match e with
  | TSIden({iname; itype}) -> (match itype with
    | Some(t) -> Printf.sprintf "%s: %s" iname (string_of_tstype t)
    | None -> Printf.sprintf "%s" iname)
  | TSNum(n) -> string_of_int n
  | TSBool(b) -> string_of_bool b
  | TSEqual(e1, e2) -> Printf.sprintf "%s === %s" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env)
  | TSNotEqual(e1, e2) -> Printf.sprintf "%s !== %s" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env)
  | TSIf(e1, e2, e3) -> (match e3 with
    | Some(elseE) -> Printf.sprintf "if (%s) {\n %s}\nelse {\n%s\n}" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env) (string_of_ts_expr elseE env)
    | None -> Printf.sprintf "if (%s) {\n %s\n}" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env))
  | TSLet(v, ie) -> Printf.sprintf "let %s = %s" v (string_of_ts_expr ie env)
  | TSPlus(tse1, tse2) -> Printf.sprintf "%s + %s" (string_of_ts_expr tse1 env) (string_of_ts_expr tse2 env)
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
  | TSArray(es) -> Printf.sprintf "[%s]" (String.concat ", " (List.map (fun e -> string_of_tsexpr_or_spread e env) es))
  | TSReturn(e) -> Printf.sprintf "return %s" (string_of_ts_expr e env)
  | TSString(s) -> Printf.sprintf "\"%s\"" s
  | TSAccess(e1, e2) -> Printf.sprintf "%s.%s" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env)
  | TSAssignment(e1, e2) -> Printf.sprintf "%s = %s;" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env)
  | TSIndex(e1, e2) -> Printf.sprintf "%s[%s]" (string_of_ts_expr e1 env) (string_of_ts_expr e2 env)
  | TSInterface(n, attrs) -> Printf.sprintf "interface %s {\n %s\n}" n (String.concat "\n" (List.map string_of_ts_typed_attr attrs))
  | TSCast(e, cast) -> Printf.sprintf "%s as %s" (string_of_ts_expr e env) cast
  | TSClosure(args, body, is_async) -> if is_async then
      Printf.sprintf "async (%s) => {\n  %s\n}" (String.concat ", " (List.map string_of_tsparam args)) (print_list "\n" (List.map (fun e -> string_of_ts_expr e env) body))
    else
      Printf.sprintf "(%s) => {\n  %s\n}" (String.concat ", " (List.map string_of_tsparam args)) (print_list "\n" (List.map (fun e -> string_of_ts_expr e env) body))
  | TSImmediateInvoke(c) -> (match c with
      | TSClosure(_, _, _) ->
        Printf.sprintf "(%s)()" (string_of_ts_expr c env)
      | _ -> failwith "Can only immediately invoke a closure")
  | TSAwait(e) -> Printf.sprintf "await %s" (string_of_ts_expr e env)
  | TSExport(e) -> Printf.sprintf "export %s" (string_of_ts_expr e env)
  | TSAliasImport(imports, file) -> Printf.sprintf "import { %s } from \"%s\";" (String.concat ", " (List.map string_of_symbol_import imports)) file
  | TSDefaultImport(import, file) -> Printf.sprintf "import %s from \"%s\";" import file
  | TSObject(props) -> Printf.sprintf "{%s}" (String.concat ",\n" (List.map (fun p -> string_of_obj_prop p env) props))
  | TSNew(c, args) -> Printf.sprintf "new %s(%s)" c (String.concat ", " (List.map (fun e -> string_of_ts_expr e env) args))
  | SLSpliceExpr(_) -> "SLSpliceExpr"
  (* Process context gets broken here - might be ok *)
  | SLExpr(e) -> string_of_expr e [] env

and string_of_tsexpr_or_spread e env =  match e with
| TSEOSExpr(e) -> string_of_ts_expr e env
| TSEOSSpread(a) -> Printf.sprintf "...%s" a 

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
  | TSTBool -> "boolean"
  | TSTGeneric(n, types) -> Printf.sprintf "%s<%s>"
    n
    (String.concat ", " (List.map string_of_tstype types))

and string_of_ts_typed_attr ta = Printf.sprintf "%s: %s" ta.tsname (string_of_tstype ta.tstyp)

and string_of_tsfunc_decl_arg a env = match a.default_val with
| Some(e) -> Printf.sprintf "%s = %s" (string_of_ts_typed_attr a.tattr) (string_of_ts_expr e env)
| None -> string_of_ts_typed_attr a.tattr

and string_of_tsclassdef cd env = match cd with
  | TSClassProp(n, typ) -> Printf.sprintf "%s: %s" n (string_of_tstype typ)
  | TSClassMethod(nm, args, body, is_async) -> 
    let method_def =  Printf.sprintf "%s(%s) { %s }" nm (String.concat ", " (List.map (fun a -> string_of_tsfunc_decl_arg a env) args)) (List.map (fun e -> string_of_ts_expr e env) body |> print_list "\n") in
    if is_async then
      Printf.sprintf "async %s" method_def
    else
      method_def
  | CDSLExpr(_) -> "CDSLExpr remove"  
and process_constructor defs env = 
  let attrs = Process.filter_attrs defs in
  let with_defaults = List.map (fun a -> (a, default_val_for_type a.typ)) attrs in
  let ctor_args = String.concat ", " (List.map (fun a -> 
    Printf.sprintf "%s = %s" (string_of_typed_attr (fst a)) (string_of_expr (snd a) [] env)
  ) with_defaults ) in
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
    | STBool -> Some(TSTBool)
    | STVariant(n, _) -> Some(TSTCustom(n))
    | STGeneric(n, types) -> (match n with      
      | "Id" ->
        let typ = List.nth types 0 in
        tstype_of_sltype (Some(typ))
      | _ -> Some(TSTGeneric(string_of_generic_type n, List.filter_map (fun t -> tstype_of_sltype (Some(t))) types))))
  | None -> None

let rec tsexpr_of_expr env e  = 
  let to_tsexpr = tsexpr_of_expr env in
  let to_tsexpr_or_spread = tsexpr_or_spread_of_expr env in
  match e with
  | Let(var, e) -> TSLet(var, to_tsexpr e)
  | StmtList(es) -> TSStmtList(List.map to_tsexpr es)
  | Iden(name, typ) -> TSIden({iname=name; itype=tstype_of_sltype typ})
  | Num(i) -> TSNum(i)
  | Bool(b) -> TSBool(b)
  | If(e1, e2, e3) -> (match e3 with
    | Some(elseE) -> TSIf(to_tsexpr e1, to_tsexpr e2, Some(to_tsexpr elseE))
    | None -> TSIf(to_tsexpr e1, to_tsexpr e2, None))
  | Array(es) -> TSArray(List.map to_tsexpr_or_spread es)
  | String(s) -> TSString(s)
  | Assignment(v, e) -> TSAssignment(TSIden({iname=Printf.sprintf "%s" v;itype=None}), to_tsexpr e)

  (* Unsure about this - why doesn't Access have an expr on the right hand side? *)
  | Access(e, accessor) -> TSAccess(to_tsexpr e, TSIden({iname=accessor; itype=None}))

  (* Unsure if this should be StmtList *)
  | TS(tses) -> TSStmtList(tses)

  | Call(name, args) ->
    if List.exists (fun n -> n = name) Interpreter.builtin_funcs then
      tsexpr_of_builtin_call name args env
    else
      TSFuncCall(name, List.map to_tsexpr args)

  | Plus(e1, e2) -> TSPlus(to_tsexpr e1, to_tsexpr e2)
  | FuncDef({fdname; fdargs; fdbody}) ->
    let last_expr = List.rev fdbody |> List.hd in
    let other_exprs = List.rev fdbody |> List.tl |> List.rev in

    let body_with_return = List.concat [
      List.map to_tsexpr other_exprs;
      [TSReturn(to_tsexpr last_expr)]
    ] in

    TSLet(fdname, TSClosure(List.map (fun a -> TSPTypedAttr(tstyped_attr_of_typed_attr a)) fdargs, body_with_return, false))

    (* Not handling these, but should *)
  | Case(_, _) -> failwith "Not handling Case to TS"

  
  (* Not handling these, and probably should never *)
  | Effect(_) -> failwith "Not handling Effect to TS"
  | Implementation(_) -> failwith "Not handling Implementation to TS"
  | File(_) -> failwith "Not handling File to TS"
  | Process(_, _) -> failwith "Not handling Process to TS - maybe convert to class"
  | Entity(_, _) -> failwith "Not handling Entity to TS"
  | Variant(_, _) -> failwith "Not handling Variant to TS"

and tstyped_attr_of_typed_attr ta =
  {tsname=ta.name; tstyp=tstype_of_sltype (Some(ta.typ)) |> Option.get}

and tsexpr_or_spread_of_expr env e =
  TSEOSExpr(tsexpr_of_expr env e)

and tsexpr_of_builtin_call n args env =
  match n with
  | "new" ->
    (* More hassle than necessary to get schema definitions.  *)
    let schema_name = expr_as_str (List.nth args 0) [] env in
    let schema_names = Env.SchemaEnv.find schema_name env.schemas |> List.map (fun ta -> ta.name) in
    let bound_pairs = List.combine schema_names (List.tl args) in
    
    TSObject(
      List.map 
        (fun (attr, e) -> {oname=attr; oval=tsexpr_of_expr env e })
        bound_pairs
    )
  | "append" -> 
    let arr = List.nth args 0 in
    let elem = List.nth args 1 in

    TSImmediateInvoke(TSClosure(
      [], 
      [
        TSLet("a", TSArray([TSEOSSpread(string_of_expr arr [] env)]));
        TSMethodCall("a", "push", [tsexpr_of_expr env elem]);
        TSReturn(TSIden({iname="a"; itype=None}))
      ],
      false
    ))

  | "delete" ->
    let arr = expr_as_str (List.nth args 0) [] env in
    let finder = expr_as_str (List.nth args 1) [] env in

    let finder_call = TSClosure(
      [TSPTypedAttr({tsname="a"; tstyp=TSTCustom("any")})],
      [TSReturn(TSFuncCall(finder, [
        TSIden({iname="a"; itype=None})
      ]))],
      false
    ) in

    let find_idx = TSLet("index", TSMethodCall(arr, "findIndex", [finder_call])) in
    let return_if_not_found = TSIf(
      TSEqual(TSIden({iname="index"; itype=None}), TSNum(-1)),
      TSReturn(TSIden({iname=arr; itype=None})),
      None
    ) in
    let ret_val = TSLet("ret", TSArray([TSEOSSpread(arr)])) in
    let filter_arr = TSMethodCall(
      "ret",
      "splice",
      [TSIden({iname="index"; itype=None}); TSNum(1)]
    ) in

    let return_ret = TSReturn(TSIden({iname="ret"; itype=None})) in

    TSImmediateInvoke(TSClosure(
      [],
      [
        find_idx;
        return_if_not_found;
        ret_val;
        filter_arr;
        return_ret;
      ],
      false
    ))
  | "map" ->
    let arr =  string_of_expr (List.nth args 0) [] env in
    let map_func = expr_as_str (List.nth args 1) [] env in
    let map_call = TSFuncCall(map_func, [
      TSIden({iname="a"; itype=None})
    ]) in

    TSMethodCall(
      arr,
      "map",
      [TSClosure(
        [TSPTypedAttr({tsname="a"; tstyp=TSTCustom("any")})],
        [TSReturn(map_call)],
        false
      )]
    )
  | "update" ->
    let arr = expr_as_str (List.nth args 0) [] env in
    let finder = expr_as_str (List.nth args 1) [] env in
    let updater = expr_as_str (List.nth args 2) [] env in

    let finder_call = TSClosure(
      [TSPTypedAttr({tsname="a"; tstyp=TSTCustom("any")})],
      [TSReturn(TSFuncCall(finder, [
        TSIden({iname="a"; itype=None})
      ]))],
      false
    ) in

    let find_idx = TSLet("index", TSMethodCall(arr, "findIndex", [finder_call])) in
    let return_if_not_found = TSIf(
      TSEqual(TSIden({iname="index"; itype=None}), TSNum(-1)),
      TSReturn(TSIden({iname=arr; itype=None})),
      None
    ) in
    let ret_val = TSLet("ret", TSArray([TSEOSSpread(arr)])) in
    let update_idx = TSAssignment(
      TSIndex(TSIden({iname="ret"; itype=None}), TSIden({iname="index"; itype=None})),
      TSFuncCall(updater, [TSIndex(TSIden({iname="ret"; itype=None}), TSIden({iname="index"; itype=None}))])
    ) in
    let return_ret = TSReturn(TSIden({iname="ret"; itype=None})) in

    TSImmediateInvoke(TSClosure(
      [],
      [
        find_idx;
        return_if_not_found;
        ret_val;
        update_idx;
        return_ret;
      ],
      false
    ))
  | "equalsStr" ->
    let larg = List.nth args 0 in
    let rarg = List.nth args 1 in

    TSEqual(tsexpr_of_expr env larg, tsexpr_of_expr env rarg)
  | "notEqualsStr" ->
    let larg = List.nth args 0 in
    let rarg = List.nth args 1 in

    TSNotEqual(tsexpr_of_expr env larg, tsexpr_of_expr env rarg)
  | "index" ->
    let larg = tsexpr_of_expr env (List.nth args 0) in
    let rarg = tsexpr_of_expr env (List.nth args 1) in

    TSIndex(larg, rarg)
  | "filter" ->
    let larg = expr_as_str (List.nth args 0) [] env in
    let rarg = tsexpr_of_expr env (List.nth args 1) in

    TSMethodCall(larg, "filter", [rarg])
(*    
  | "equals" ->
    let larg = string_of_expr (List.nth args 0) attrs env in
    let rarg = string_of_expr (List.nth args 1) attrs env in

    Printf.sprintf {|
    %s === %s
    |} larg rarg    *)
  | _ -> failwith (Printf.sprintf "Attempted to compile unknown builtin func: %s" n)    
