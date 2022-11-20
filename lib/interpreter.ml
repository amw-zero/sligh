(* 
  An interpreter for Sligh programs. This is initially intended for compile-time 
  execution in unquoting within quasi-quoted expressions.
*)

open Core

module Env = Map.Make(String)

let model_var_name = "Model"

type primitive_typ =
  | PTNum
  | PTString

type value =
  | VNum of int
  | VString of string
  | VArray of value list
  | VFunc of func_def
  | VType of type_val
  | VTS of tsexpr list
  | VTSClassDef of tsclassdef

and schema = {
  sname: string;
  attrs: schema_attr list;
}

and schema_attr = {
  aname: string;
  typ: type_val;
}

and type_val =  
| VSchema of schema
| VTArray of type_val list
| VPrimitive of primitive_typ

let rec string_of_value v = match v with
  | VNum(n) -> string_of_int n
  | VFunc(fd) -> Printf.sprintf "<func %s>" fd.fdname
  | VType(tv) -> string_of_type_val tv
  | VArray(vs) -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_value vs))
  | VTS(_) -> "VTS"
  | VTSClassDef(_) -> "VTSClassDef"
  | VString(s) -> s
and string_of_type_val tv = match tv with
  | VSchema s -> string_of_schema s
  | VTArray(_) -> "VTArray"
  | VPrimitive(_) -> "PrimitiveType"
and string_of_schema_attr a =
  Printf.sprintf "%s: %s" a.aname (string_of_type_val a.typ)
and string_of_schema s =
  Printf.sprintf "schema %s\n\  %s\nend" s.sname (Util.print_list (List.map (fun a -> string_of_schema_attr a) s.attrs))

let as_schema value =
  match value with
  | VType(tv) -> (match tv with
    | VSchema(s) -> VSchema(s)
    | _ -> failwith "not a schema")
  | _ -> failwith "not a schema"

type interp_env = value Env.t

type builtin = {
  bname: string;
  bdef: func_def;
}

let builtin_map_name = "map"
let builtin_map_def = {
  fdname=builtin_map_name;
  fdargs=[{name="list";typ=STString}; {name="func";typ=(STCustom("func"))}];
  fdbody=[];
}

let builtin_tsclass_name = "tsClass"
let builtin_tsclass_def = {
  fdname=builtin_tsclass_name;
  fdargs=[{name="name";typ=STString}; {name="defs";typ=(STCustom("syntax"))}];
  fdbody=[];
}

let builtin_tsclassprop_name = "tsClassProp"
let builtin_tsclassprop_def = {
  fdname=builtin_tsclassprop_name;
  fdargs=[{name="name";typ=STString}; {name="typ";typ=STString}];
  fdbody=[];
}

let all_builtins = [
  {bname=builtin_tsclassprop_name; bdef=builtin_tsclassprop_def};
  {bname=builtin_map_name; bdef=builtin_map_def};
  {bname=builtin_tsclass_name; bdef=builtin_tsclassprop_def}
]

let new_environment_with_builtins () =
  let env = Env.empty in
  List.fold_left (fun env b -> Env.add b.bname (VFunc(b.bdef)) env) env all_builtins

let builtin_funcs = List.map (fun b -> b.bname) all_builtins

let type_val_of_sligh_type st env = match st with
  | STCustom(name) -> Env.find name env |> as_schema
  | _ -> VPrimitive(PTNum)

let add_schema_to_env name (attrs: typed_attr list) (env: interp_env) =
  let schema_attrs = List.map (fun attr ->
    let _ = Printf.printf "Converting type for attr %s\n" (Util.string_of_typed_attr attr) in
    let attr_type = type_val_of_sligh_type Core.(attr.typ) env in

    { aname=attr.name; typ=attr_type }
  ) attrs in

  Env.add name (VType(VSchema({sname=name;attrs=schema_attrs}))) env

let build_env env stmt = 
  let _ = Printf.printf "Building env for statement %s\n" (Util.string_of_expr stmt) in
  match stmt with
  | FuncDef(fd) -> Env.add fd.fdname (VFunc(fd)) env
  | Process(d, defs) -> 
      let attrs: typed_attr list = Process.filter_attrs defs in
      add_schema_to_env d attrs env
  | Entity(e, attrs) -> 
      add_schema_to_env e attrs env
  | _ -> env

let add_model_to_env m env =
  (* Create a type named Schemas whose attributes are all of the existing schema definitions *)
  let schema_type_vals = List.filter_map 
    (fun schema -> match Env.find Process.(schema.name) env with 
      | VType(tv) -> (match tv with 
        | VSchema(s) -> Some(VSchema(s))
        | _ -> None)
      | _ -> None) 
    Process.(m.schemas) in

  (* let schemas_schema = VSchema({ sname="Schemas"; attrs=schema_attrs }) in
  let env = Env.add "Schemas" (VType(schemas_schema)) env in *)

  Env.add model_var_name (VType(VSchema({
    sname=model_var_name;
    attrs=[{ aname="schemas"; typ=VTArray(schema_type_vals) }]
  }))) env

let print_env env =
  print_endline "\nInterpreter.Env";
  Env.iter (fun k value -> Printf.printf "%s -> %s\n" k (string_of_value value)) env;
  print_endline ""

let build_call_env (env: interp_env) (pair: (typed_attr * value)) =
  let (arg_sig, arg) = pair in
  Env.add arg_sig.name arg env

let attribute_schema attr = { sname=attr.aname; attrs=[{ aname="type"; typ=attr.typ }] }

let schema_attr_values attrs = List.map (fun a -> VType(VSchema(attribute_schema a))) attrs

let find_attr attr_name schema =
  match attr_name with
  | "name" -> VString(schema.sname)
  | "attributes" -> VArray(schema_attr_values schema.attrs)
  | _ -> let found_attr = List.find (fun attr -> attr.aname = attr_name) schema.attrs in
    (match found_attr.typ with
    | VSchema(s) -> VType(VSchema(s))
    | VTArray(tvs) -> VArray(List.map (fun tv -> VType(tv)) tvs)
    | VPrimitive(pt) -> VType(VPrimitive(pt)))

  (* Evaluate a Sligh expression *)
let rec evaln es env = eval_and_return es env
and eval (e: expr) (env: interp_env): (value * interp_env) = 
  let _ = Printf.printf "Evaling expr %s\n" (Util.string_of_expr e) in
  match e with
  | Iden(i, _) -> (Env.find i env, env)
  | Num(n) -> (VNum(n), env)
  | Call(name, args) ->
    let (arg_sigs, body) = try (match Env.find name env with
      | VFunc({fdargs;fdbody;_}) -> (fdargs, fdbody)
      | _ -> failwith "Attempted to call non function definition")
      with Not_found -> failwith (Printf.sprintf "Couldn't find function def to call: %s" name) in
    if List.length args != List.length arg_sigs then failwith "Bad arity at func call" else ();
    
    let reduced_args = List.map 
      (fun a -> try eval a env |> fst with Not_found -> failwith (Printf.sprintf "Unable to eval expr: %s" (Util.string_of_expr e))) 
      args in    
    if List.exists (fun n -> n = name) builtin_funcs then
      eval_builtin_func name reduced_args env
    else
      let arg_pairs: (typed_attr * value) list = List.combine arg_sigs reduced_args in
      let call_env = List.fold_left build_call_env env arg_pairs in

      (eval_and_return body call_env, env)
  | Access(e, var) ->
      let (left, _) = eval e env in
      let _ = Printf.printf "Access - left evaled to %s\n" (string_of_value left) in
      (match left with 
      | VType(tv) -> (match tv with
        | VSchema(s) -> (find_attr var s, env)
        | _ -> failwith (Printf.sprintf "Unable to access variable - did not evaluate to a schema: %s" (Util.string_of_expr e)))
      | _ -> failwith (Printf.sprintf "Unable to access variable - did not evaluate to a type: %s" (Util.string_of_expr e)))
  | Let(var, e) ->
    let (ve, next_env) = eval e env in
    let new_env = Env.add var ve next_env in

    (ve, new_env)
  | TS(tses) ->
    let evaled_tses = List.map (fun tse -> eval_ts tse env |> fst) tses in
    (VTS(evaled_tses), env)
  | _ -> failwith (Printf.sprintf "Unable to eval expr %s" (Util.string_of_expr e))

and eval_and_return expr_list env =
  List.fold_left (fun (_, curr_env) e -> eval e curr_env) (eval (List.hd expr_list) env) expr_list
    |> fst

and eval_builtin_func name args env =
  match name with
  | "map" -> 
    let lst_arg = List.nth args 0 in
    let map_func_arg = List.nth args 1 in 

    let fd = match map_func_arg with
      | VFunc(fd) -> fd
      | _ -> failwith (Printf.sprintf "Tried calling map builtin with non-function 2nd arg %s" (string_of_value map_func_arg)) in
    
    let lst = match lst_arg with 
      | VArray(vs) -> vs
      | _ -> failwith (Printf.sprintf "Tried calling map builtin with non-array 1st arg %s" (string_of_value lst_arg)) in
    
    let result = List.map (fun e ->
      let arg_pairs: (typed_attr * value) list = List.combine fd.fdargs [e] in
      let call_env = List.fold_left build_call_env env arg_pairs in

      eval_and_return fd.fdbody call_env
    ) lst in

    (VArray(result), env)
  | "tsClass" ->
    let name_arg = List.nth args 0 in
    let name_arg = match name_arg with
      | VString(s) -> s
      | _ -> failwith (Printf.sprintf "Calling tsClass on non-string %s" (string_of_value name_arg)) in

    let defs_arg = List.nth args 1 in
    let defs_arg = match defs_arg with
      | VArray(vcds) -> List.filter_map (fun vcd -> match vcd with | VTSClassDef(cd) -> Some(cd) | _ -> None ) vcds
      | _ -> failwith (Printf.sprintf "Calling tsClass on non-Array of VTS Class defs %s" (string_of_value defs_arg)) in

    (VTS([tsClass name_arg defs_arg]), env)
  | "tsClassProp" ->
    let name_arg = List.nth args 0 in
    let name_arg = match name_arg with
    | VString(s) -> s
    | _ -> failwith (Printf.sprintf "Calling tsClassProp on non-string %s" (string_of_value name_arg)) in
    let typ_arg = List.nth args 1 in
    let typ_arg = match typ_arg with
      | VType(tv) -> tstype_of_type_val tv
      | _ -> failwith (Printf.sprintf "Calling tsClassProp on non-Type value %s" (string_of_value typ_arg)) in

    (VTSClassDef(tsClassProp name_arg typ_arg), env)
  | _ -> failwith (Printf.sprintf "Attempted to call unimplemented builtin func: %s" name)

and eval_ts ts_expr env = match ts_expr with
| SLExpr(e) -> (tsexpr_of_val ((eval e env) |> fst), env)
| TSLet(var, exp) -> (TSLet(var, eval_ts exp env |> fst), env)
| TSMethodCall(recv, call, args) ->
    let reduced_args = List.map (fun a -> eval_ts a env |> fst) args in
    (TSMethodCall(recv, call, reduced_args), env)
| _ -> (ts_expr, env)

and tsexpr_of_val v = match v with
| VNum(n) -> TSNum(n)
| VTS(tss) -> TSStmtList(tss)
| VArray(vs) -> TSArray(List.map tsexpr_of_val vs)
| VString(s) -> TSString(s)
| _ -> failwith (Printf.sprintf "Cannot tseval value %s" (string_of_value v))

and tstype_of_type_val tv = match tv with
| VSchema(s) -> TSTCustom(s.sname)
| VTArray(_) -> TSTCustom("RandomArray")
| VPrimitive(_)  -> TSTNumber

let type_of_string s = match s with
  | "Int" -> STInt
  | "String" -> STString
  | _ -> STCustom(s)

let tstype_of_string s = match s with
  | "number" -> TSTNumber
  | _ -> TSTCustom(s)
