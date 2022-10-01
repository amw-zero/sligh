type boolexp = BTrue | BFalse | BIf of boolexp * boolexp * boolexp

type tsexpr =
  | TSIden of string
  | TSNum of int
  | TSLet of string * tsexpr

type expr = 
  TS of tsexpr
  | Let of string * expr
  | Iden of string
  | Num of int
  | BoolExp of boolexp

let rec tsexpr_of_expr e = match e with
  | Let(v, b) -> TSLet(v, tsexpr_of_expr b)
  | Iden (s) -> TSIden(s)
  | Num(n) -> TSNum(n)
  | TS(t) -> t
  | _ -> TSNum(-1)

type decl = Idk

type syntax = 
  Expr of expr | Decl of decl
