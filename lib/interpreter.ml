(* 
  An interpreter for Sligh programs. This is initially intended for compile-time 
  execution in unquoting within quasi-quoted expressions.
*)

open Core

module Env = Map.Make(String)

let model_var_name = "Model"


type primitive_typ =
  | PTNum

type value =
  | VNum of int
  | VString of string
  | VArray of value list
  | VFunc of func_def
  | VType of type_val
  | VTS of tsexpr list

and schema = {
  sname: string;
  attrs: schema_attr list;
}

and schema_attr = {
  aname: string;
  value: value;
}

and type_val =  
| VSchema of schema
| VPrimitive of primitive_typ

let rec string_of_value v = match v with
  | VNum(n) -> string_of_int n
  | VFunc(fd) -> Printf.sprintf "<func %s>" fd.fdname
  | VType(t) -> (match t with
    | VSchema(s) -> Printf.sprintf "schema %s" s.sname
    | VPrimitive(_) -> "primitive type")
  | VArray(vs) -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_value vs))
  | VTS(_) -> "VTS"
  | VString(s) -> s

type interp_env = value Env.t

let builtin_map_name = "map"
let builtin_map_def = {
  fdname=builtin_map_name;
  fdargs=[{name="list";typ=(STCustom("list"))}; {name="func";typ=(STCustom("func"))}];
  fdbody=[];
}

let new_environment_with_builtins () = 
  let env = Env.empty in
  Env.add builtin_map_name (VFunc(builtin_map_def)) env

let builtin_funcs = [builtin_map_name]

let add_schema_to_env name attrs (env: interp_env) =
  let schema_attrs = List.filter_map (fun attr ->
    if Env.mem attr.name env then
      let value = Env.find attr.name env in
      match value with
      | VType(VSchema(s)) -> Some({aname=attr.name; value=VType(VSchema(s))})
      | _ -> failwith (Printf.sprintf "Value can't be added to environment as schema attribute %s" attr.name)
    else
      None
      (* failwith (Printf.sprintf "Referencing unknown identifier %s" attr.name) *)
  ) attrs in

  let attrs_with_name = {aname="name";value=VString(name)} :: schema_attrs in

  Env.add name (VType(VSchema({sname=name;attrs=attrs_with_name}))) env

let build_env env stmt = match stmt with
  | FuncDef(fd) -> Env.add fd.fdname (VFunc(fd)) env
  | Domain(d, defs) -> 
      let attrs: typed_attr list = Model.filter_attrs defs in
      add_schema_to_env d attrs env
  | Entity(e, attrs) -> 
      add_schema_to_env e attrs env
  | _ -> env

(* Somewhere, this needs to be made to recurse on nested schemas *)
let add_model_to_env m env =
  let schema_values = List.filter_map (fun schema -> match Env.find Model.(schema.name) env with | VType(tv) -> Some(VType(tv)) | _ -> None) Model.(m.schemas) in
  (* let schema_attrs = List.map (fun s -> {aname=s.sname; value=(VType(VSchema(s)))}) schema_values in *)

  Env.add model_var_name (VType(VSchema({
    sname=model_var_name;
    attrs=[{ aname="schemas"; value=VArray(schema_values) }];
  }))) env

let print_env env =
  print_endline "Interpreter.Env";
  Env.iter (fun k _ -> Printf.printf "%s\n" k) env;
  print_endline ""

let build_call_env (env: interp_env) (pair: (typed_attr * value)) =
  let (arg_sig, arg) = pair in
  Env.add arg_sig.name arg env

let find_attr attr_name schema =
  Printf.printf "Checking for attr %s in schema %s" attr_name (string_of_value (VType(VSchema(schema))));
  let attr = List.find (fun attr -> attr.aname = attr_name) schema.attrs in
  attr.value

  (* Evaluate a Sligh expression *)
let rec evaln es env = eval_and_return es env
and eval (e: expr) (env: interp_env): value = 
  print_endline "Evaluating expr";
  print_endline (Util.string_of_expr e);
  match e with
  | Iden(i, _) -> Env.find i env
  | Num(n) -> VNum(n)
  | Call(name, args) ->
    let (arg_sigs, body) = match Env.find name env with
      | VFunc({fdargs;fdbody;_}) -> (fdargs, fdbody)
      | _ -> failwith "Attempted to call non function definition" in
    if List.length args != List.length arg_sigs then failwith "Bad arity at func call" else ();
    
    let reduced_args = List.map (fun a -> eval a env) args in    
    if List.exists (fun n -> n = name) builtin_funcs then
      eval_builtin_func name reduced_args env
    else
      let arg_pairs: (typed_attr * value) list = List.combine arg_sigs reduced_args in
      let call_env = List.fold_left build_call_env env arg_pairs in

      eval_and_return body call_env
  | Access(e, var) ->
      let left = eval e env in
      (match left with 
      | VType(tv) -> (match tv with
        | VSchema(s) -> find_attr var s
        | _ -> failwith (Printf.sprintf "Unable to access variable - did not evaluate to a schema: %s" (Util.string_of_expr e)))
      | _ -> failwith (Printf.sprintf "Unable to access variable - did not evaluate to a type: %s" (Util.string_of_expr e)))
  | TS(tses) ->
    let evaled_tses = List.map (fun tse -> eval_ts tse env) tses in
    VTS(evaled_tses)
  | _ -> failwith (Printf.sprintf "Unable to eval expr %s" (Util.string_of_expr e))

and eval_and_return expr_list call_env =
  List.fold_left (fun _ e -> eval e call_env) (eval (List.hd expr_list) call_env) expr_list

and eval_builtin_func name args env =
  match name with
  | "map" -> 
    let lst_arg = List.nth args 0 in
    let map_func_arg = List.nth args 1 in 

    let fd = match map_func_arg with
      | VFunc(fd) -> fd
      | _ -> failwith (Printf.sprintf "Tried calling map builtin with non-function arg %s" (string_of_value map_func_arg)) in
    
    let lst = match lst_arg with 
      | VArray(vs) -> vs
      | _ -> failwith (Printf.sprintf "Tried calling map builtin with non-array arg %s" (string_of_value lst_arg)) in
    
    let result = List.map (fun e ->
      (* let arg_pairs: (typed_attr * value) list = [(List.hd builtin_map_def.fdargs, e)] in *)
      let arg_pairs: (typed_attr * value) list = List.combine fd.fdargs [e] in
      let call_env = List.fold_left build_call_env env arg_pairs in

      eval_and_return fd.fdbody call_env
    ) lst in

    VArray(result)
  | _ -> failwith (Printf.sprintf "Attempted to call unimplemented builtin func: %s" name)

and eval_ts ts_expr env = match ts_expr with
| SLExpr(e) -> tsexpr_of_val (eval e env)
| TSMethodCall(recv, call, args) ->
    let reduced_args = List.map (fun a -> eval_ts a env) args in
    TSMethodCall(recv, call, reduced_args)
| _ -> ts_expr

and tsexpr_of_val v = match v with
| VNum(n) -> TSNum(n)
| VTS(tss) -> TSStmtList(tss)
| VArray(vs) -> TSArray(List.map tsexpr_of_val vs)
| VString(s) -> TSString(s)
| _ -> failwith (Printf.sprintf "Cannot tseval value %s" (string_of_value v))

and tstype_of_type t = match t with
| STInt -> TSTNumber
| STCustom s -> TSTCustom s

let type_of_string s = match s with
  | "Int" -> STInt
  | _ -> STCustom(s)

let tstype_of_string s = match s with
  | "number" -> TSTNumber
  | _ -> TSTCustom(s)

  
(* let rec tsclassdef_of_expr e = match e with
  | Iden(i, Some(t)) -> TSClassProp(i, tstype_of_type t)
  | Call(_, _) -> eval e |> tsclassdef_of_expr
  | _ -> failwith "Cannot translate expr to tsclassdef" *)

