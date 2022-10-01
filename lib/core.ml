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

let rec tsexpr_of_expr e = match e with
  | Let(v, b) -> TSLet(v, tsexpr_of_expr b)
  | Iden (s) -> TSIden(s)
  | Num(n) -> TSNum(n)
  | TS(ts) -> TSStmtList(ts)
  | _ -> TSNum(-1)
