(* 
  An interpreter for Sligh programs. This is initially intended for compile-time 
  execution in unquoting within quasi-quoted expressions.
*)

open Core

module Env = Map.Make(String)

let new_environment () = Env.empty

let build_env ie stmt = match stmt with
  | FuncDef(name, _, _) -> Env.add name stmt ie
  | _ -> ie

let print_env ie =
  print_endline "Interpreter.Env";
  Env.iter (fun k _ -> Printf.printf "%s\n" k) ie;
  print_endline ""

let build_call_env (env: expr Env.t) (pair: (typed_attr * expr)) =
  let (arg_sig, arg) = pair in
  Env.add arg_sig.name arg env

(* Evaluate a Sligh expression *)
let rec evaln es env = eval_and_return es env
and eval (e: expr) (env: expr Env.t) = match e with
  | Iden(i, _) -> eval (Env.find i env) env
  | Num(_) -> e
  | Call(name, args) ->
    let (arg_sigs, body) = match Env.find name env with
      | FuncDef(_, arg_sigs, body) -> (arg_sigs, body)
      | _ -> failwith "Attempted to call non function definition" in
    if List.length args != List.length arg_sigs then failwith "Bad arity at func call" else ();
    let reduced_args = List.map (fun a -> eval a env) args in
    let arg_pairs: (typed_attr * expr) list = List.combine arg_sigs reduced_args in
    let call_env = List.fold_left build_call_env env arg_pairs in

    eval_and_return body call_env
  | TS(tses) ->
    let evaled_tses = List.map (fun tse -> eval_ts tse env) tses in
    TS(evaled_tses)
  | _ -> e

and eval_and_return expr_list call_env =
  List.fold_left (fun _ e -> eval e call_env) (eval (List.hd expr_list) call_env) expr_list

and eval_ts ts_expr env = match ts_expr with
| SLExpr(e) -> tsexpr_of_expr (eval e env)
| _ -> ts_expr

and tsexpr_of_expr e = match e with
| Let(v, b) -> TSLet(v, tsexpr_of_expr b)
| Iden (s, Some(t)) -> TSIden(s, Some(tstype_of_type t))
| Num(n) -> TSNum(n)
| TS(ts) -> TSStmtList(ts)
| _ -> failwith "Cannot translate expr to tsexpr"

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

