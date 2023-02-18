open Core

module Env = Map.Make(String)

let new_effect_env () = Env.empty

let effect_body_for_proc name effect args interp_env =
  let eff = List.find (fun eff -> eff.ecname = name) effect.procs in
  if name = "model" then
    (* The model process is a direct rewrite *)
    eff.ebody
  else
    (* Non-model processes get evaluated as macros *)
    let arg_sigs = eff.eargs in
    let arg_pairs: (typed_attr * expr) list = List.combine arg_sigs args in
    let interp_env = List.fold_left Interpreter.build_macro_env interp_env arg_pairs in
    let res = Interpreter.evaln eff.ebody interp_env in

    (* This can be handled by the type system *)
    match res with 
    | VSLExpr(e) -> [e]
    | VTS(tses) -> [TS(tses)]
    | _ -> failwith (Printf.sprintf "Effects can only return an SLExpr or TSExpr, got %s" (Interpreter.string_of_value res))

let analyze env stmt =
  match stmt with
  | Effect(e) -> Env.add e.ename e env
  | _ -> env

let print_env env =
  print_endline "Effects.Env";
  Env.iter (fun ename eff -> Printf.printf "%s -> %s\n" ename eff.ename) env;
  print_endline ""

(* Effect application should take place after implementation expansion *)
let rec apply proc_name effect_env interp_env expr =
  let apply_expr = apply proc_name effect_env interp_env in

  match expr with
  (* Replace a call with effect body for the given process *)
  | Call(effect_name, args) -> 
    if Env.mem effect_name effect_env  then
      let proc_effect = Env.find effect_name effect_env in
      let effect_procs = List.map (fun eff -> eff.ecname) proc_effect.procs in
      if List.mem proc_name effect_procs then
        Some(StmtList(effect_body_for_proc proc_name proc_effect args interp_env))
      else
        None
    else
      Some(expr)

  (* Recursively apply effects *)
  | Let(name, body) -> Option.map (fun e -> Let(name, e)) (apply_expr body)
  | StmtList(ss) -> 
    (* If an effect is applied its body is placed within a StmtList, so need to
       flatten that to prevent nested StmtLists *)
    let stmts = List.filter_map apply_expr ss |> List.concat_map (fun expr -> match expr with 
      | StmtList(es) -> es
      | _ -> [expr]) in

    Some(StmtList(stmts))
  | Process(n, defs) -> 
    (* Def need to use the effect_env here to take in definition information and use later,
       i.e. what was the type of a variable passed to a generic effect? *)
    Some(Process(n, List.map (fun def -> apply_proc_def proc_name effect_env interp_env def) defs))
  | FuncDef({fdname; fdargs; fdbody;}) -> Some(FuncDef({fdname; fdargs; fdbody=List.filter_map apply_expr fdbody}))
  | Access(e, _) -> apply_expr e
  | TS(tses) -> Some(TS(List.filter_map (fun tse -> apply_tsexpr proc_name effect_env interp_env tse) tses))
  | _ -> Some(expr)

and apply_proc_def proc_name effect_env interp_env def = match def with
  | ProcAttr(_) -> def
  | ProcAction({ aname; args; body; }) -> ProcAction({aname; args; body=List.filter_map (fun e -> apply proc_name effect_env interp_env e) body})

and apply_tsexpr proc_name effect_env interp_env tse =
  let apply_tsexpr_expr = apply_tsexpr proc_name effect_env interp_env in

  match tse with
  | TSLet(v, ie) -> Option.map (fun e -> TSLet(v, e)) (apply_tsexpr_expr ie)
  | TSStmtList(ss) -> Some(TSStmtList(List.filter_map apply_tsexpr_expr ss))
  | TSClass(n, ds) -> Some(TSClass(n, List.map (fun def -> apply_tsclassdef proc_name effect_env interp_env def) ds))
  | TSMethodCall(recv, m, args) -> Some(TSMethodCall(recv, m, List.filter_map apply_tsexpr_expr args))
  | TSFuncCall(f, args) -> Some(TSFuncCall(f, List.filter_map apply_tsexpr_expr args))
  | TSAccess(e1, e2) ->
    let e1' = apply_tsexpr_expr e1 in
    let e2' = apply_tsexpr_expr e2 in

    (match (e1', e2') with
    | (Some(e1'_some), Some(e2'_some)) -> Some(TSAccess(e1'_some, e2'_some))
    | _ -> None)
  | TSAssignment(e1, e2) -> 
    let e1' = apply_tsexpr_expr e1 in
    let e2' = apply_tsexpr_expr e2 in
    
    (match (e1', e2') with
    | (Some(e1'_some), Some(e2'_some)) -> Some(TSAssignment(e1'_some, e2'_some))
    | _ -> None)
  | TSClosure(args, body, ia) -> Some(TSClosure(args, List.filter_map apply_tsexpr_expr body, ia))
  | TSObject(props) -> 
    let new_props: obj_prop list = List.map (fun p -> {oname=p.oname; oval=apply_tsexpr_expr p.oval |> Option.get} ) props in

    Some(TSObject(new_props))
  | TSIf(e1, e2, e3) -> 
    Some(TSIf(apply_tsexpr_expr e1 |> Option.get, apply_tsexpr_expr e2 |> Option.get, Option.map (fun e -> apply_tsexpr_expr e |> Option.get) e3))
  | SLSpliceExpr(e) -> Option.map (fun e' -> SLSpliceExpr(e')) (apply proc_name effect_env interp_env e)
  | SLExpr(e) -> Option.map (fun e' -> SLExpr(e')) (apply proc_name effect_env interp_env e)
  | TSIden(_) -> Some(tse)
  | TSNum(_) -> Some(tse)
  | TSBool(_) -> Some(tse)
  | TSArray(_) -> Some(tse)
  | TSInterface(_, _) -> Some(tse)
  | TSString(_) -> Some(tse)
  | TSAwait(_) -> Some(tse)
  | TSExport(_) -> Some(tse)
  | TSAliasImport(_, _) -> Some(tse)
  | TSDefaultImport(_, _) -> Some(tse)
  | TSNew(_, _) -> Some(tse)
and apply_tsclassdef proc_name effect_env interp_env cd = match cd with
  | TSClassMethod(nm, args, body, ia) -> TSClassMethod(nm, args, List.filter_map (fun tse -> apply_tsexpr proc_name effect_env interp_env tse) body, ia)
  | _ -> cd

  