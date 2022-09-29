{
  open Parser

  exception SyntaxError of string
}

let whitespace = [' ' '\t' '\r' '\n']

rule read = parse
  | whitespace+       { read lexbuf }
  | "true"            { TRUE }
  | "false"           { FALSE }
  | "if"              { IF }
  | "then"            { THEN }
  | "else"            { ELSE }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | _                 { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof               { EOF }