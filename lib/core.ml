type boolexp = BTrue | BFalse | BIf of boolexp * boolexp * boolexp

type tsexpr =
  | TSIDen of string
  | TSNum of int
  | TSLet of string * tsexpr

type expr = 
  TS of tsexpr
  | Let of string * expr
  | Iden of string
  | Num of int
  | BoolExp of boolexp

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