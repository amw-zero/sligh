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

// TS EDSL
%token <int> NUMBER
%token LET
%token <string> IDEN
%token TYPESCRIPT
%token COLON
%token END
%token EQUALS
%token UNQUOTE
%token UNQUOTEEND
%token COMMA

// Sligh
%token DOMAIN
%token DEF
%token DOT
%token ENVIRONMENT
%token ENTITY

%start prog
%type <expr list> prog 
%type <boolexp> boolexp
%type <tsexpr> tsexp

%%

prog: 
  | ss = statements EOF     { ss }
  | EOF                     { [] }

statements:
  | ss = statements s = statement   { ss @ [s] }
  | s = statement                   { [s] }

statement:
  | LET i = IDEN EQUALS e = expression              { Let(i, e) }
  | DOMAIN i = IDEN COLON d = domain_def* END       { Domain(i, d) }
  | ENTITY i = IDEN COLON d = domain_def* END       { Domain(i, d) }
  | ENVIRONMENT COLON ss = statements END           { Env(ss) }
  | e = expression                                  { e }

boolexp:
  | TRUE                            { BTrue }
  | FALSE                           { BFalse }
  | IF e1 = boolexp THEN e2 = boolexp ELSE e3 = boolexp 
                                    { BIf(e1, e2, e3) }
expression:
  | boolexp                                       { BoolExp($1) }
  | n = NUMBER                                    { Num(n) }
  | i = IDEN                                      { Iden(i) }
  | recv = expression DOT meth = IDEN LPAREN args = separated_list(COMMA, expression) RPAREN 
                                                  { Call(meth, [recv] @ args) }

  (* This causes a shift/reduce warning currently *)
  | meth = IDEN LPAREN args = separated_list(COMMA, expression) RPAREN
                                                  { Call(meth, args) }
  | TYPESCRIPT COLON tse = tsstatements END       { TS(tse) }
  | LPAREN e = expression RPAREN                  { e }

domain_def:
  | ta = typed_attr                               { DomainAttr(ta) }
  | DEF act = IDEN LPAREN args = separated_list(COMMA, typed_attr) RPAREN COLON e = expression END
                                                  { DomainAction({
                                                      aname=act;
                                                      args;
                                                      body=e
                                                    }) }
typed_attr:
  | attr = IDEN COLON typ = IDEN                  { {name=attr; typ=typ} }

(* TypeScript Lang *)
tsstatements:
  | ss = tsstatements s = tsstatement   { ss @ [s] }
  | s = tsstatement                     { [s] }

tsstatement:
  | LET i = IDEN EQUALS tse = tsexp     { TSLet(i, tse) }
  | e = tsexp                           { e }

tsexp:
  | n = NUMBER                          { TSNum(n) }
  | UNQUOTE e = expression UNQUOTEEND   { tsexpr_of_expr e }
