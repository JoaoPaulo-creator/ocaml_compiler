%{
    open Ast
%}


%token <int> INT
%token <string> IDENFITIFER
%token PLUS MINUS DIVIDE MULTIPLY 
%token LPAREN RPAREN
%token LET EQUALS
%token PRINT
%token SEMICOLON
%token EOF

%left PLUS MINUS
%left MULTIPLY DIVIDE

%start <program> program

%%

program:
    | stmts = list(stmt) EOF { stmt }
    ;

stmt:
    | LET id = IDENFITIFER EQUALS e = expr SEMICOLON { Let (id, e)}
    | PRINT LPAREN e = expr RPAREN SEMICOLON { Print e }
    | e = expr SEMICOLON { e }
    ;

expr:
  | i = INT                                      { Int i }
  | id = IDENTIFIER                              { Var id }
  | LPAREN e = expr RPAREN                       { e }
  | e1 = expr PLUS e2 = expr                     { Binary (e1, Plus, e2) }
  | e1 = expr MINUS e2 = expr                    { Binary (e1, Minus, e2) }
  | e1 = expr MULTIPLY e2 = expr                 { Binary (e1, Multiply, e2) }
  | e1 = expr DIVIDE e2 = expr                   { Binary (e1, Divide, e2) }
  ;