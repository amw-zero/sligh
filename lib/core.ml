type boolexp = BTrue | BFalse | BIf of boolexp * boolexp * boolexp

type sligh_type =
  | STInt
  | STString
  | STDecimal
  | STCustom of string
  | STGeneric of string * sligh_type list
  | STVariant of string * variant_tag list

and typed_attr = {
  name: string;
  typ: sligh_type;
}

and variant_tag = {
  tname: string;
  tattrs: typed_attr list;
}

type pattern_binding =
  | PBVar of string
  | PBAny

type variant_pattern = {
  vname: string;
  var_bindings: pattern_binding list
}

type value_pattern =
  | StringPattern of string
  | VariantPattern of variant_pattern

type expr =
  TS of tsexpr list
  (* TargetLang of tlang_expr list - to make target languages extensible*)

  (* Statements, move to separate type *)
  | Let of string * expr
  | Assignment of string * expr

  | Iden of string * sligh_type option
  | Num of int
  | Array of expr list
  | If of expr * expr * expr option
  | StmtList of expr list

  | Process of string * proc_def list
  | Entity of string * typed_attr list
  | Variant of string * variant_tag list
  | Call of string * expr list

  | File of file

  | String of string

  (* Should only be decl, possibly only be class decl *)
  | Implementation of expr
  | FuncDef of func_def
  | Access of expr * string

  | Case of expr * case_branch list
  | Effect of effect

and effect = {
  ename: string;  
  procs: proc_effect list
}

and proc_effect = {
  ecname: string;
  eargs: typed_attr list;
  ebody: expr list;
}

and func_def = {
  fdname: string;
  fdargs: typed_attr list;
  fdbody: expr list;
}

and proc_action =
  { aname: string;
    args: typed_attr list; 
    body: expr list;
  }

and proc_def =
| ProcAttr of typed_attr
| ProcAction of proc_action

and file = {
  fname: string;
  fbody: expr list;
}

and case_branch = {
  pattern: value_pattern;
  value: expr;
}

and tsexpr =
| TSIden of tsiden
| TSNum of int
| TSLet of string * tsexpr
| TSStmtList of tsexpr list
| TSMethodCall of string * string * tsexpr list
| TSFuncCall of string * tsexpr list
| TSClass of string * tsclassdef list
| TSIf of tsexpr * tsexpr * tsexpr option
| TSArray of tsexpr list
| TSString of string
| TSAccess of tsexpr * tsexpr
| TSAssignment of tsexpr * tsexpr
| TSInterface of string * tstyped_attr list
| TSClosure of tsiden list * tsexpr list
| TSObject of obj_prop list
| TSNew of string * tsexpr list
| TSAwait of tsexpr
| TSAsync of tsexpr

(* | TSFunc of string * tstyped_attr * tsexpr list *)
| SLExpr of expr
| SLSpliceExpr of expr

and obj_prop = {
  oname: string;
  oval: tsexpr;
}

and tsiden = {
  iname: string;
  itype: ts_type option;
}

and tstyped_attr = {
  tsname: string;
  tstyp: ts_type;
}

and ts_type = 
  | TSTNumber
  | TSTString
  | TSTCustom of string
  | TSTGeneric of string * ts_type list

and tsclassdef =
  | CDSLExpr of expr
  | TSClassProp of string * ts_type
  | TSClassMethod of string * tstyped_attr list * tsexpr list

let tsClassProp name typ = TSClassProp(name, typ)

let tsClass name defs = TSClass(name, defs)

let tsClassMethod name args body = TSClassMethod(name, args, body)

let tsTypedAttr name typ = {tsname=name; tstyp=typ}

let tsAccess left right = TSAccess(left, right)

let tsIden n = TSIden({iname=n; itype=None})

let tsAssignment left right = TSAssignment(left, right)

let tsStatementList stmts = TSStmtList(stmts)

let tsInterface name attrs = TSInterface(name, attrs)
