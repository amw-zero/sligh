open Core

type schema = {
  name: string;
  attrs: typed_attr list;
}

type action = {
  action_ast: Core.proc_action;
  state_vars: Core.typed_attr list;
  assignments: (string * Core.expr) list
}

type variant = {
  vname: string;
  variants: variant_tag list
}

type process = {
  schemas: schema list;
  variants: variant list;
  variables: Core.typed_attr list;
  actions: action list;
}

let new_process () = {
  schemas=[];
  variants=[];
  variables=[];
  actions=[];
}

let filter_model stmts = List.filter_map (fun stmt -> match stmt with
| Core.Process(_) -> Some(stmt)
| Core.Entity(_) -> Some(stmt)
| _ -> None) stmts

let collect_actions actions def = match def with
  | ProcAction(act) -> act :: actions
  | _ -> actions

let filter_actions defs = List.fold_left collect_actions [] defs

let collect_attrs attrs def = match def with
  | ProcAttr(attr) -> attr :: attrs
  | _ -> attrs

let filter_attrs (defs: proc_def list): typed_attr list  = List.fold_left collect_attrs [] defs

type action_info = {
  vars: typed_attr list;
  assigns: (string * Core.expr) list
}

let empty_action_info =
  { vars=[]; assigns=[] }

let rec collect_action_info action_info e (proc_attrs: typed_attr list): action_info =
  match e with
  | Let(_, value) -> 
    let info = collect_action_info empty_action_info value proc_attrs in 
    let vars = info.vars @ action_info.vars in
    let assigns = info.assigns @ action_info.assigns in

    { vars; assigns }
  | Assignment(var, e) ->
    (* Failure to find attr here means assignment is on a non-state variable *)
    let proc_attr = List.find (fun attr -> Core.(attr.name = var)) proc_attrs in
    let info = collect_action_info empty_action_info e proc_attrs in
    let vars = {name=var; typ=proc_attr.typ} :: info.vars in
    let assigns = (var, e) :: info.assigns in 

    { vars; assigns }
  | Iden(i, _) ->
    let proc_attr = List.find_opt (fun attr -> Core.(attr.name = i)) proc_attrs in

    (match proc_attr with
    | Some(pa) -> 
      let vars = {name=i; typ=pa.typ} :: action_info.vars in

      { action_info with vars }
    | None -> action_info)
  | Plus(e1, e2) -> 
    let info_e1 = collect_action_info empty_action_info e1 proc_attrs in
    let info_e2 = collect_action_info empty_action_info e2 proc_attrs in 
    let vars = info_e1.vars @ info_e2.vars in
    let assigns = info_e1.assigns @ info_e2.assigns in

    { vars; assigns }
  | Array(es) ->
      List.fold_left
        (fun ai e -> 
          let info = collect_action_info empty_action_info e proc_attrs in 

          { vars=info.vars @ ai.vars; assigns=info.assigns @ ai.assigns }
        )
        empty_action_info
        es
  | If(cond, then_e, else_e) ->
    let else_info = match else_e with
      | Some(ee) -> collect_action_info empty_action_info ee proc_attrs
      | None -> empty_action_info in
    let cond_info = collect_action_info empty_action_info cond proc_attrs in
    let then_info = collect_action_info empty_action_info then_e proc_attrs in
    let vars = else_info.vars @ cond_info.vars @ then_info.vars in
    let assigns = else_info.assigns @ cond_info.assigns @ then_info.assigns in
    
    { vars; assigns }
  | StmtList(es) ->
    List.fold_left
        (fun ai e -> 
          let info = collect_action_info empty_action_info e proc_attrs in 

          { vars=info.vars @ ai.vars; assigns=info.assigns @ ai.assigns }
        )
        empty_action_info
        es
  | Call(_, args) ->
    List.fold_left
        (fun ai e -> 
          let info = collect_action_info empty_action_info e proc_attrs in 

          { vars=info.vars @ ai.vars; assigns=info.assigns @ ai.assigns }
        )
        empty_action_info
        args

  | Access(l, r) ->
    let proc_attr = List.find_opt (fun attr -> Core.(attr.name = r)) proc_attrs in

    (match proc_attr with
    | Some(pa) ->
      let l_info = collect_action_info empty_action_info l proc_attrs in
      let vars = {name=r; typ=pa.typ} :: l_info.vars in

      { action_info with vars }
    | None -> action_info)
  | Case(e, cases) ->
    let cases_info = List.fold_left
      (fun ai e -> 
        let info = collect_action_info empty_action_info e.value proc_attrs in 

        { vars=info.vars @ ai.vars; assigns=info.assigns @ ai.assigns }
      )
      empty_action_info
      cases in
    let e_info = collect_action_info empty_action_info e proc_attrs in

    { vars=cases_info.vars @ e_info.vars; assigns=cases_info.assigns @ e_info.assigns}
  | String(_) -> action_info
  | Num(_) -> action_info
  | Bool(_) -> action_info

   (* Should only be decl, possibly only be class decl *)
  | Implementation(_) -> action_info
  | FuncDef(_) -> action_info
  | File(_) -> action_info
  | Effect(_) -> action_info
  | Process(_, _) -> action_info
  | Entity(_, _) -> action_info
  | TS(_) -> action_info
  | Variant(_, _) -> action_info

let action_info_of_action (action: Core.proc_action) (proc_attrs: typed_attr list) =
  let info = List.fold_left
    (fun action_info e -> collect_action_info action_info e proc_attrs)
    empty_action_info
    action.body in

  let vars = List.sort_uniq (fun sv1 sv2 -> Core.(compare sv1.name sv2.name)) info.vars in
  let assigns = List.sort_uniq (fun a1 a2 -> compare (fst a1) (fst a2)) info.assigns in

  { vars; assigns }

let analyze_action actions action proc_attrs =
  let info = action_info_of_action action proc_attrs in
  {
    state_vars=info.vars;
    assignments=info.assigns;
    action_ast=action;
  } :: actions
  
let analyze_actions (actions: Core.proc_action list) (proc_attrs: typed_attr list) =
  List.fold_left
    (fun analyzed_actions action -> analyze_action analyzed_actions action proc_attrs)
    []
    actions

let analyze_model m stmt =
  match stmt with
  | Core.Process(_, defs) ->
    let actions = filter_actions defs in
    let attrs = filter_attrs defs in
    { m with
      schemas = m.schemas;
      variables = attrs @ m.variables;
      actions = m.actions @ (analyze_actions actions attrs);
    }
  | Core.Entity(e, attrs) -> 
    { m with
      schemas = {name=e; attrs;} :: m.schemas;
    }
  | Core.Variant(n, vs) ->
    { m with
      variants = {vname=n; variants=vs} :: m.variants}
  | _ -> m

let print_schema s =
  Printf.printf "Schema: %s,\n\t%s\n" s.name (Util.print_list "\n" (List.map Util.string_of_typed_attr s.attrs))

let print_variable v =
  Printf.printf "Var: %s\n" (Util.string_of_typed_attr v)

let print_action a =
  Printf.printf "Action: \n  ast: %s\n\n  state_vars: %s\n assignments: %s\n"
    (Util.string_of_proc_action a.action_ast)
    (String.concat "\n" (List.map Util.string_of_typed_attr a.state_vars))
    (String.concat "\n" (List.map Util.string_of_typed_attr a.state_vars))


let print_process m =
  print_endline "Process.schemas";
  List.iter print_schema m.schemas;
  print_endline "Process.variables";
  List.iter print_variable m.variables;
  print_endline "Process.actions";
  List.iter print_action m.actions
