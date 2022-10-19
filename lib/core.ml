type boolexp = BTrue | BFalse | BIf of boolexp * boolexp * boolexp

type sligh_type =
  | STInt
  | STString
  | STCustom of string

type expr = 
  TS of tsexpr list
  (* TargetLang of tlang_expr list - to make target languages extensible*)
  | Let of string * expr
  | Iden of string * sligh_type option
  | Num of int
  | BoolExp of boolexp
  | StmtList of expr list
  | Domain of string * domain_def list
  | Entity of string * typed_attr list
  | Call of string * expr list
  | Process of process
  | FuncDef of func_def
  | Access of expr * string

and func_def = {
  fdname: string;
  fdargs: typed_attr list;
  fdbody: expr list;
}

and typed_attr =
  { name: string;
    typ: sligh_type;
  }

and domain_action =
  { aname: string;
    args: typed_attr list; 
    body: expr;
  }

and domain_def =
| DomainAttr of typed_attr
| DomainAction of domain_action

and process = {
  ename: string;
  ebody: expr list
}

and tsexpr =
| TSIden of string * ts_type option
| TSNum of int
| TSLet of string * tsexpr
| TSStmtList of tsexpr list
| TSMethodCall of string * string * tsexpr list
| TSClass of string * tsclassdef list
| TSArray of tsexpr list
| TSString of string
| SLExpr of expr

and ts_type = 
  | TSTNumber
  | TSTString
  | TSTCustom of string

and tsclassdef =
  | CDSLExpr of expr
  | TSClassProp of string * ts_type
  (* | TSClassMethod of string * tstyped_attr list * tsexpr list *)

let tsClassProp name typ = TSClassProp(name, typ)

let tsClass name defs = TSClass(name, defs)
