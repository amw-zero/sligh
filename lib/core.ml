type boolexp = BTrue | BFalse | BIf of boolexp * boolexp * boolexp

type tsexpr =
  | TSIden of string
  | TSNum of int
  | TSLet of string * tsexpr
  | TSStmtList of tsexpr list

type expr = 
  TS of tsexpr list
  | Let of string * expr
  | Iden of string
  | Num of int
  | BoolExp of boolexp
  | StmtList of expr list
  | Domain of string * domain_def list

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

let rec tsexpr_of_expr e = match e with
  | Let(v, b) -> TSLet(v, tsexpr_of_expr b)
  | Iden (s) -> TSIden(s)
  | Num(n) -> TSNum(n)
  | TS(ts) -> TSStmtList(ts)
  | _ -> TSNum(-1)
