type boolexp = BTrue | BFalse | BIf of boolexp * boolexp * boolexp

type sligh_type =
  | STInt
  | STString
  | STDecimal
  | STCustom of string

type expr = 
  TS of tsexpr list
  (* TargetLang of tlang_expr list - to make target languages extensible*)
  | Let of string * expr
  | Iden of string * sligh_type option
  | Num of int
  | BoolExp of boolexp
  | StmtList of expr list

  | Process of string * proc_def list
  | Entity of string * typed_attr list
  | Call of string * expr list

  | File of file

  (* Should only be decl, possibly only be class decl *)
  | Implementation of expr
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

and proc_action =
  { aname: string;
    args: typed_attr list; 
    body: expr;
  }

and proc_def =
| ProcAttr of typed_attr
| ProcAction of proc_action

and file = {
  ename: string;
  ebody: expr list;
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

and tstyped_attr = {
  tsname: string;
  tstyp: ts_type;
}

and ts_type = 
  | TSTNumber
  | TSTString
  | TSTCustom of string

and tsclassdef =
  | CDSLExpr of expr
  | TSClassProp of string * ts_type
  | TSClassMethod of string * tstyped_attr list * tsexpr list

let tsClassProp name typ = TSClassProp(name, typ)

let tsClass name defs = TSClass(name, defs)

let tsClassMethod name args body = TSClassMethod(name, args, body)
