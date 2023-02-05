{
open Parser

class lexer_context =
object
  val lexers = Stack.create ()

  method push_lexer (lexer : Lexing.lexbuf -> Parser.token) =
    Stack.push lexer lexers

  method pop_lexer =
    (* We've a stack of functions, so we don't want to apply the result. *)
    ignore (Stack.pop lexers) [@warning "-5"]

  method next_lexer =
    Stack.top lexers
end

let fresh_context () = new lexer_context

exception SyntaxError of string
}

let whitespace = [' ' '\t' '\r' '\n']
let iden = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9' '!']*
let num = ['0'-'9']

rule read ctx = parse
  | eof               { EOF }
  | whitespace+       { read ctx lexbuf }
  | "}}"              { ctx#pop_lexer; UNQUOTEEND }
  | '"'               { read_string (Buffer.create 16) lexbuf}
  | "true"            { TRUE }
  | "false"           { FALSE }
  | "if"              { IF }
  | "then"            { THEN }
  | "else"            { ELSE }
  | "case"            { CASE }
  | "data"            { DATA }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '['               { LSQBRACKET }
  | ']'               { RSQBRACKET }
  | ':'               { COLON }
  | ','               { COMMA }
  | '.'               { DOT }
  | "="               { EQUALS }
  | "|"               { BAR }
  | "_"               { UNDERSCORE }
  | "let"             { LET }
  | "end"             { END }
  | "def"             { DEF }
  | "process"         { PROCESS }
  | "entity"          { ENTITY }
  | "file"            { FILE }
  | "implementation"  { IMPLEMENTATION }
  | "rewrite"          { EFFECT }
  | "typescript"      { ctx#push_lexer (read_ts ctx); TYPESCRIPT }
  | num               { NUMBER (Lexing.lexeme lexbuf |> int_of_string) }
  | iden              { IDEN (Lexing.lexeme lexbuf) }
  | _                 { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and read_string buf = parse
  | '"'               { STRING (Buffer.contents buf) }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
and read_ts ctx = parse
  | eof               { EOF }
  | whitespace+       { read_ts ctx lexbuf }
  | "{{"              { ctx#push_lexer (read ctx); UNQUOTE }
  | "{{*"             { ctx#push_lexer (read ctx); UNQUOTE_SPLICE }
  | '"'               { read_string (Buffer.create 16) lexbuf}
  | ':'               { COLON }
  | '.'               { DOT }
  | "="               { EQUALS }
  | ','               { COMMA }
  | "let"             { LET }
  | "class"           { CLASS }
  | "await"           { AWAIT }
  | "async"           { ASYNC }
  | '{'               { LBRACE }
  | '}'               { RBRACE }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | "end"             { ctx#pop_lexer; END }
  | num               { NUMBER (Lexing.lexeme lexbuf |> int_of_string) }
  | iden              { IDEN (Lexing.lexeme lexbuf) }
  | _                 { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

{
 let lexer : lexer_context
         -> (Lexing.lexbuf -> Parser.token) =
  fun ctxt ->
    ctxt#push_lexer (read ctxt);
   fun lexbuf -> ctxt#next_lexer lexbuf
}
