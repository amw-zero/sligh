open Typescript_syntax

type boolexp = BTrue | BFalse | BIf of boolexp * boolexp * boolexp

type sligh_type =
  | STInt
  | STCustom of string

type expr = 
  TS of tsexpr list
  | Let of string * expr
  | Iden of string * sligh_type option
  | Num of int
  | BoolExp of boolexp
  | StmtList of expr list
  | Domain of string * domain_def list
  | Call of string * expr list
  | Env of env_component list

and typed_attr =
  { name: string;
    (* Elevate the type to an actual type datatype *)
    typ: string;
  }

and domain_action =
  { aname: string;
    args: typed_attr list; 
    body: expr;
  }

and domain_def =
| DomainAttr of typed_attr
| DomainAction of domain_action

and env_component = {
  ename: string;
  ebody: expr list
}

let type_of_string s = match s with
  | "Int" -> STInt
  | _ -> STCustom(s)

let tstype_of_string s = match s with
  | "number" -> TSTNumber
  | _ -> TSTCustom(s)

let tstype_of_type t = match t with
  | STInt -> TSTNumber
  | STCustom s -> TSTCustom s

(* This effectively 'compiles' a Sligh expr into TS *)
let rec tsexpr_of_expr e = match e with
  | Let(v, b) -> TSLet(v, tsexpr_of_expr b)
  | Iden (s, Some(t)) -> TSIden(s, Some(tstype_of_type t))
  | Num(n) -> TSNum(n)
  | TS(ts) -> TSStmtList(ts)
  | _ -> TSNum(-1)

let tsclassdef_of_expr e = match e with
  | Iden(i, Some(t)) -> TSClassProp(i, tstype_of_type t)
  | _ -> TSClassProp("!error!", TSTNumber)
