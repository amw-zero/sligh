type boolexp = BTrue | BFalse | BIf of boolexp * boolexp * boolexp

type expr = 
  TS of string
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