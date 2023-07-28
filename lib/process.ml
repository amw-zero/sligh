open Core

type schema = {
  name: string;
  attrs: typed_attr list;
}

type action = {
  action_ast: Core.proc_action;
  state_vars: Core.typed_attr list;
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

let rec collect_state_vars state_vars e (proc_attrs: typed_attr list): typed_attr list =
  match e with
  | Let(_, value) -> state_vars @ collect_state_vars [] value proc_attrs
  | Assignment(var, e) ->
    (* Failure to find attr here means assignment is on a non-state variable *)
    let proc_attr = List.find (fun attr -> Core.(attr.name = var)) proc_attrs in

    {name=var; typ=proc_attr.typ} :: state_vars @ collect_state_vars [] e proc_attrs
  | Iden(i, _) ->
    let proc_attr = List.find_opt (fun attr -> Core.(attr.name = i)) proc_attrs in

    (match proc_attr with
    | Some(pa) -> {name=i; typ=pa.typ} :: state_vars
    | None -> state_vars)
  | Plus(e1, e2) -> 
    state_vars @ collect_state_vars [] e1 proc_attrs @ collect_state_vars [] e2 proc_attrs
  | Array(es) ->
      List.concat_map
        (fun e -> collect_state_vars [] e proc_attrs)
        es @ state_vars 
  | If(cond, then_e, else_e) ->
    let else_state_vars: typed_attr list = match else_e with
      | Some(ee) -> collect_state_vars [] ee proc_attrs
      | None -> [] in

    collect_state_vars [] cond proc_attrs @
    collect_state_vars [] then_e proc_attrs @ else_state_vars
  | StmtList(es) ->
    List.concat_map
      (fun e -> collect_state_vars [] e proc_attrs)
      es @ state_vars
  | Call(_, args) ->
    List.concat_map
      (fun e -> collect_state_vars [] e proc_attrs)
      args @ state_vars

  | Access(l, r) ->
    let proc_attr = List.find_opt (fun attr -> Core.(attr.name = r)) proc_attrs in

    (match proc_attr with
    | Some(pa) ->
      let l_state_vars = collect_state_vars [] l proc_attrs in

      {name=r; typ=pa.typ} :: state_vars @ l_state_vars
    | None -> state_vars)
  | Case(e, cases) ->
    collect_state_vars [] e proc_attrs @
    (List.concat_map (fun c -> collect_state_vars [] c.value proc_attrs) cases) @
    state_vars

  | String(_) -> state_vars
  | Num(_) -> state_vars
  | Bool(_) -> state_vars

   (* Should only be decl, possibly only be class decl *)
  | Implementation(_) -> state_vars
  | FuncDef(_) -> state_vars
  | File(_) -> state_vars
  | Effect(_) -> state_vars
  | Process(_, _) -> state_vars
  | Entity(_, _) -> state_vars
  | TS(_) -> state_vars
  | Variant(_, _) -> state_vars

let state_vars_of_action (action: Core.proc_action) (proc_attrs: typed_attr list) =
  List.fold_left
    (fun state_vars e -> collect_state_vars state_vars e proc_attrs)
    []
    action.body
    (* collect_state_vars is returning duplicates - should ultimately fix that instead
       of this unique sort *)
    |> List.sort_uniq (fun sv1 sv2 -> Core.(compare sv1.name sv2.name))

let analyze_action actions action proc_attrs =
  {
    state_vars=state_vars_of_action action proc_attrs;
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
  Printf.printf "Action: \n  ast: %s\n\n  state_vars: %s\n"
    (Util.string_of_proc_action a.action_ast)
    (String.concat "\n" (List.map Util.string_of_typed_attr a.state_vars))

let print_process m =
  print_endline "Process.schemas";
  List.iter print_schema m.schemas;
  print_endline "Process.variables";
  List.iter print_variable m.variables;
  print_endline "Process.actions";
  List.iter print_action m.actions
