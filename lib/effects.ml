open Core

module Env = Map.Make(String)

let new_effect_env () = Env.empty

let effect_body_for_proc name effect =
  let eff = List.find (fun eff -> eff.ecname = name) effect.procs in
  eff.ebody

let analyze env stmt =
  match stmt with
  | Effect(e) -> Env.add e.ename e env
  | _ -> env

let print_env env =
  print_endline "Effects.Env";
  Env.iter (fun ename eff -> Printf.printf "%s -> %s\n" ename eff.ename) env;
  print_endline ""

let rec apply proc_name effect_env expr =
  Printf.printf "Calling apply for: %s\n" proc_name;
  let apply_expr = apply proc_name effect_env in

  match expr with
  (* Replace a call with effect body for the given process *)
  | Call(effect_name, _) -> 
    if Env.mem effect_name effect_env then
      let proc_effect = Env.find effect_name effect_env in
      Printf.printf "Found effect: %s\n" proc_effect.ename;
      
      StmtList(effect_body_for_proc proc_name proc_effect)
    else
      expr

  (* Recursively apply effects *)
  | Let(name, body) -> Let(name, apply_expr body)
  | StmtList(ss) -> StmtList(List.map apply_expr ss)
  | Process(n, defs) -> Process(n, List.map (fun def -> apply_proc_def proc_name effect_env def) defs)
  
  | FuncDef({fdname; fdargs; fdbody;}) -> FuncDef({fdname; fdargs; fdbody=List.map apply_expr fdbody})
  | Access(e, _) -> apply_expr e
  | TS(tses) -> TS(List.map (fun tse -> apply_tsexpr proc_name effect_env tse) tses)
  | _ -> expr

and apply_proc_def proc_name effect_env def = match def with
  | ProcAttr(_) -> def
  | ProcAction({ aname; args; body; }) -> ProcAction({aname; args; body= apply proc_name effect_env body})

and apply_tsexpr proc_name effect_env tse =
  Printf.printf "Applying ts_expr for name: %s\n" proc_name;
  let apply_tsexpr_expr = apply_tsexpr proc_name effect_env in

  match tse with
  | TSLet(v, ie) -> TSLet(v, apply_tsexpr_expr ie)
  | TSStmtList(ss) -> TSStmtList(List.map apply_tsexpr_expr ss)
  | TSClass(n, ds) -> TSClass(n, List.map (fun def -> apply_tsclassdef proc_name effect_env def) ds)
  | TSMethodCall(recv, m, args) -> TSMethodCall(recv, m, List.map apply_tsexpr_expr args)
  | TSAccess(e1, e2) -> TSAccess(apply_tsexpr_expr e1, apply_tsexpr_expr e2)
  | TSAssignment(e1, e2) -> TSAssignment(apply_tsexpr_expr e1, apply_tsexpr_expr e2)
  | SLSpliceExpr(e) -> SLSpliceExpr(apply proc_name effect_env e)
  | SLExpr(e) -> SLExpr(apply proc_name effect_env e)
  | _ -> tse
and apply_tsclassdef proc_name effect_env cd = match cd with
  | TSClassMethod(nm, args, body) -> TSClassMethod(nm, args, List.map (fun tse -> apply_tsexpr proc_name effect_env tse) body)
  | _ -> cd

  