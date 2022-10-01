%{
open Core
%}

// Values
%token TRUE
%token FALSE

%token IF
%token THEN
%token ELSE

%token LPAREN
%token RPAREN
%token EOF

// Edsl
%token <int> NUMBER
%token LET
%token <string> IDEN
%token TYPESCRIPT
%token COLON
%token END
%token EQUALS

%start prog
%type <expr option> prog 
%type <boolexp> boolexp
%type <tsexpr> tsexp

%%

prog: 
  | e = expression EOF { Some e }
  | EOF                { None }

boolexp:
  | TRUE                            { BTrue }
  | FALSE                           { BFalse }
  | IF e1 = boolexp THEN e2 = boolexp ELSE e3 = boolexp 
                                    { BIf(e1, e2, e3) }

tsexp:
  | n = NUMBER                          { print_endline "TSExp Num hit"; TSNum(n) }
  | LET i = IDEN EQUALS tse = tsexp     { print_endline "TSExp Let hit"; TSLet(i, tse) }

expression: 
  | boolexp                               { BoolExp($1) }
  | n = NUMBER                            { Num(n) }
  | LET i = IDEN EQUALS e = expression    { Let(i, e) }
  | TYPESCRIPT COLON tse = tsexp END    { TS(tse) }
  | LPAREN e = expression RPAREN          { e }

