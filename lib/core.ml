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
  | Process of process
  | FuncDef of string * typed_attr list * expr list

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
| SLExpr of expr

and ts_type = 
  | TSTNumber
  | TSTCustom of string  

and tsclassdef =
  | CDSLExpr of expr
  | TSClassProp of string * ts_type

let tsclassprop s t = TSClassProp(s, t)
