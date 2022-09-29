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

%start prog
%type <boolexp option> prog 

%%

prog: 
  | e = expression EOF { Some e }
  | EOF                { None };

expression:
  | TRUE                          { BTrue }
  | FALSE                         { BFalse}
  | IF e1 = expression THEN e2 = expression ELSE e3 = expression 
                                  { BIf(e1, e2, e3) }
  | LPAREN e = expression RPAREN  { e }
