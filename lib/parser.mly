%{
open Core
open Interpreter
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
%token CLASS
%token LBRACE
%token RBRACE

// Sligh
%token DOMAIN
%token DEF
%token DOT
%token PROCESS
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

//  | ENTITY i = IDEN COLON ta = typed_attr* END       { Entity(i, ta) }
  | PROCESS n = IDEN COLON es = statement* END      { Process({ename=n;ebody=es}) }
  | DEF i = IDEN LPAREN args = separated_list(COMMA, typed_attr) RPAREN COLON body = statements END
                                                    { FuncDef(i, args, body) }
  | e = expression                                  { e }

boolexp:
  | TRUE                            { BTrue }
  | FALSE                           { BFalse }
  | IF e1 = boolexp THEN e2 = boolexp ELSE e3 = boolexp 
                                    { BIf(e1, e2, e3) }
expression:
  | n = NUMBER                                    { Num(n) }
  | i = IDEN                                      { Iden(i, None) }
  | TYPESCRIPT COLON tse = tsstatements END       { TS(tse) }
  | boolexp                                       { BoolExp($1) }
  | LPAREN e = expression RPAREN                  { e }
  | s = expression DOT t = IDEN                   { Access(s, t) }
  | recv = expression DOT meth = IDEN LPAREN args = separated_list(COMMA, expression) RPAREN
                                                  { Call(meth, [recv] @ args) }
  (* This causes a shift/reduce warning currently *)
  | func = IDEN LPAREN args = separated_list(COMMA, expression) RPAREN
                                                  { Call(func, args) }

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
  | CLASS i = IDEN LBRACE ds = tsclassdef* RBRACE
                                        { TSClass(i, ds) }
  | e = tsexp                           { e }

tsclassdef_unquote:
  | i = IDEN COLON typ = IDEN   { CDSLExpr (Iden(i, Some(type_of_string typ))) }
  | e = expression              { CDSLExpr e }

tsclassdef:
  | UNQUOTE e = tsclassdef_unquote UNQUOTEEND   { e }
  | i = IDEN COLON typ = IDEN                   { TSClassProp(i, tstype_of_string typ) }

tsexp:
  | n = NUMBER                          { TSNum(n) }
  | recv = IDEN DOT meth = IDEN LPAREN args = separated_list(COMMA, tsexp) RPAREN
                                        { TSMethodCall(recv, meth, args) }
  | UNQUOTE e = expression UNQUOTEEND   { SLExpr(e) }
