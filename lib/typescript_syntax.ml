type ts_type = 
  | TSTNumber
  | TSTCustom of string  

type tsclassdef =
  | TSClassProp of string * ts_type

type tsexpr =
  | TSIden of string * ts_type option
  | TSNum of int
  | TSLet of string * tsexpr
  | TSStmtList of tsexpr list
  | TSMethodCall of string * string * tsexpr list
  | TSClass of string * tsclassdef list