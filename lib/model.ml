open Core

type schema = {
  name: string;
  attrs: typed_attr list;
}

type model = {
  schemas: schema list;
  variables: Core.typed_attr list;
  actions: Core.domain_action list;
}

let new_model () = {
  schemas=[];
  variables=[];
  actions=[];
}

let collect_actions actions def = match def with
  | DomainAction(act) -> act :: actions
  | _ -> actions

let filter_actions defs = List.fold_left collect_actions [] defs

let collect_attrs attrs def = match def with
  | DomainAttr(attr) -> attr :: attrs
  | _ -> attrs

let filter_attrs defs = List.fold_left collect_attrs [] defs

let analyze m stmt =
  match stmt with
  | Core.Domain(name, defs) ->
    let actions = filter_actions defs in
    let attrs = filter_attrs defs in
    { 
      schemas = {name; attrs;} :: m.schemas;
      variables = attrs @ m.variables;
      actions = actions @ m.actions;
    }
  | _ -> m

let print_schema s =
  Printf.printf "Schema: %s,\n\t%s\n" s.name (Util.print_list (List.map Util.string_of_typed_attr s.attrs))

let print_variable v =
  Printf.printf "Var: %s\n" (Util.string_of_typed_attr v)

let print_action a =
  Printf.printf "Action: %s\n" (Util.string_of_domain_action a)

let print_model m =
  List.iter print_schema m.schemas;
  List.iter print_variable m.variables;
  List.iter print_action m.actions

(* let expr_of_model m =  *)