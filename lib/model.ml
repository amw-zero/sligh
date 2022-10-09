type sligh_type =
  | TypeInt
  | TypeArray

type variable = {
  name: string;
  typ: sligh_type;
}

type action = variable list -> variable list

type schema = {
  name: string;
  state: variable list;
  actions: action list;
}

type model = {
  schemas: schema list; 
}

let new_model () = {
  schemas = [];
}

let analyze m stmt =
  match stmt with
  | Core.Domain(name, _) -> { schemas = { name; state = []; actions = [] } :: m.schemas }
  | _ -> m

let print_schema s =
  print_endline "Hello";
  Printf.printf "Schema: %s\n" s.name

let print_model m =
  List.iter print_schema m.schemas