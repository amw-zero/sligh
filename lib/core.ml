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
  (* Don't know what to do with nested TS blocks, and boolexp isn't even real *)
  | _ -> TSNum(-1)

type decl = Idk

type syntax = 
  Expr of expr | Decl of decl

(* 

let prog = typescript:
  let x = 5;
  let y = 17;
end

let x = 5
typescript:
  let tx = `x`;
end

*)