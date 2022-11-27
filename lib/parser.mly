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
%token UNQUOTE_SPLICE
%token UNQUOTEEND
%token COMMA
%token CLASS
%token LBRACE
%token RBRACE

// Sligh
%token DEF
%token DOT
%token PROCESS
%token FILE
%token ENTITY
%token IMPLEMENTATION
%token <string> STRING
%token CASE
%token BAR
%token UNDERSCORE

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
  | PROCESS i = IDEN COLON p = proc_def* END        { Process(i, p) }
  | ENTITY i = IDEN COLON ta = typed_attr* END      { Entity(i, ta) }
  | IMPLEMENTATION COLON e = expression END         { Implementation(e) }
  | FILE n = IDEN COLON es = statement* END         { File({ename=n;ebody=es;}) }  
  | DEF i = IDEN LPAREN args = separated_list(COMMA, typed_attr) RPAREN COLON body = statements END
                                                    { FuncDef({fdname=i; fdargs=args; fdbody=body}) }
  | e = expression                                  { e }

boolexp:
  | TRUE                            { BTrue }
  | FALSE                           { BFalse }
  | IF e1 = boolexp THEN e2 = boolexp ELSE e3 = boolexp 
                                    { BIf(e1, e2, e3) }
expression:
  | non_app   { $1 }
  | app       { $1 }

pattern_binding:
  | i = IDEN      { PBVar(i) }
  | UNDERSCORE    { PBAny }

case_branch:
  | BAR i = IDEN LPAREN bindings = separated_list(COMMA, pattern_binding) RPAREN COLON body = expression
                                                  { {pattern={vname=i; var_bindings=bindings}; value=body} }

app:
  | recv = non_app DOT meth = IDEN LPAREN args = separated_list(COMMA, non_app) RPAREN
                                                  { Call(meth, [recv] @ args) }
  (* This causes a shift/reduce warning currently *)
  | func = IDEN LPAREN args = separated_list(COMMA, non_app) RPAREN
                                                  { Call(func, args) }

non_app:
  | n = NUMBER                                    { Num(n) }
  | i = IDEN                                      { Iden(i, None) }
  | s = STRING                                    { String(s) }
  | LPAREN e = expression RPAREN                  { e }
  | CASE e = expression COLON branches = list(case_branch) END
                                                  { Case(e, branches)}
  | TYPESCRIPT COLON tse = tsstatements END       { TS(tse) }
  | boolexp                                       { BoolExp($1) }

  (* Shift / reduce warning *)
  | e = non_app DOT i = IDEN                      { Access(e, i) }

proc_def:
  | ta = typed_attr                               { ProcAttr(ta) }
  | DEF act = IDEN LPAREN args = separated_list(COMMA, typed_attr) RPAREN COLON e = expression END
                                                  { ProcAction({
                                                      aname=act;
                                                      args;
                                                      body=e
                                                    }) }
typed_attr:
  | attr = IDEN COLON typ = IDEN                  { {name=attr; typ=type_of_string typ} }

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
  | n = NUMBER                                  { TSNum(n) }
  | recv = IDEN DOT meth = IDEN LPAREN args = separated_list(COMMA, tsexp) RPAREN
                                                { TSMethodCall(recv, meth, args) }
  | UNQUOTE_SPLICE e = expression UNQUOTEEND    { SLSpliceExpr(e) }
  | UNQUOTE e = expression UNQUOTEEND           { SLExpr(e) }
