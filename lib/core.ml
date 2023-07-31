type sligh_type =
  | STInt
  | STString
  | STDecimal
  | STBool
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

type tssymbol_import = {
  symbol: string;
  alias: string option;
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
  | Bool of bool
  | Iden of string * sligh_type option
  | Num of int
  | Plus of expr * expr
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
| TSBool of bool
| TSLet of string * tsexpr
| TSStmtList of tsexpr list
| TSMethodCall of string * string * tsexpr list
| TSFuncCall of string * tsexpr list
| TSClass of string * tsclassdef list
| TSIf of tsexpr * tsexpr * tsexpr option
| TSArray of tsexpr list
| TSString of string
| TSReturn of tsexpr
| TSAccess of tsexpr * tsexpr
| TSAssignment of tsexpr * tsexpr
| TSInterface of string * tstyped_attr list
| TSClosure of tsparam list * tsexpr list * bool
| TSObject of obj_prop list
| TSNew of string * tsexpr list
| TSAwait of tsexpr
| TSExport of tsexpr
| TSCast of tsexpr * string
| TSAliasImport of tssymbol_import list * string
| TSDefaultImport of string * string
| SLExpr of expr
| SLSpliceExpr of expr

and obj_prop = { oname: string;
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

and tsobject_pat_prop = {
  oppname: string;
  oppvalue: string;
}

and tsobject_pat = {
  opprops: tsobject_pat_prop list;
  optyp: ts_type;
}

and tsparam =
  | TSPTypedAttr of tstyped_attr
  | TSPObjectPat of tsobject_pat

and ts_type = 
  | TSTNumber
  | TSTString
  | TSTBool
  | TSTCustom of string
  | TSTGeneric of string * ts_type list

and tsclassdef =
  | CDSLExpr of expr
  | TSClassProp of string * ts_type
  | TSClassMethod of string * tstyped_attr list * tsexpr list * bool

let tsClassProp name typ = TSClassProp(name, typ)

let tsClass name defs = TSClass(name, defs)

let tsTypedAttr name typ = {tsname=name; tstyp=typ}

let tsAccess left right = TSAccess(left, right)

let tsIden n = TSIden({iname=n; itype=None})

let tsAssignment left right = TSAssignment(left, right)

let tsStatementList stmts = TSStmtList(stmts)

let tsInterface name attrs = TSInterface(name, attrs)
