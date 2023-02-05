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

let state_vars_of_action _action = [{name="state_var"; typ=Core.STInt}]

let analyze_action actions action = 
  {
    state_vars=state_vars_of_action action;
    action_ast=action;
  } :: actions
  
let analyze_actions actions = List.fold_left analyze_action [] actions

let analyze_model m stmt =
  match stmt with
  | Core.Process(name, defs) ->
    let actions = filter_actions defs in
    let attrs = filter_attrs defs in
    { m with
      schemas = {name; attrs;} :: m.schemas;
      variables = attrs @ m.variables;
      actions = m.actions @ (analyze_actions actions);
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
  Printf.printf "Action: %s\n" (Util.string_of_proc_action a.action_ast)

let print_process m =
  print_endline "Process.schemas";
  List.iter print_schema m.schemas;
  print_endline "Process.variables";
  List.iter print_variable m.variables;
  print_endline "Process.actions";
  List.iter print_action m.actions
