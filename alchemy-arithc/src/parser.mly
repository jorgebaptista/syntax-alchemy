
/* Analisador sintático para Arith */

%{
  open Ast
%}

%token <int> CST
%token <string> IDENT
%token SET, LET, IN, PRINT
%token TRUE, FALSE
%token IF, THEN, ELSE
%token WHILE, DO, DONE
%token AND, OR, NOT
%token EOF
%token LP RP
%token PLUS MINUS TIMES DIV
%token EQ EQEQ NEQ LT LE GT GE

/* Definição das prioridades e das associatividades dos tokens */

%nonassoc IN
%right ELSE              /* resolve dangling else */
%left OR
%left AND
%left EQEQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT
%nonassoc uminus

/* Entry point para a gramática */
%start prog

/* Tipo dos valores devolvidos pelo analisador sintático */
%type <Ast.program> prog

%%

prog:
| p = stmts EOF { List.rev p }
;

stmts:
| i = stmt               { [i] }
| l = stmts i = stmt     { i :: l }
;

stmt:
| SET id = IDENT EQ e = expr 
    { Set (id, e) }
| PRINT e = expr             
    { Print e }
| IF e = expr THEN b1 = block ELSE b2 = block
    { If (e, b1, b2) }
| IF e = expr THEN b1 = block
    { If (e, b1, []) }
| WHILE e = expr DO b = while_block DONE
    { While (e, b) }
;

while_block:
| /* empty */        { [] }
| l = while_block s = stmt { l @ [s] }
;

block:
| s = stmt           { [s] }
| LP l = stmts RP    { List.rev l }
;

expr:
| c = CST                            { Cst c }
| TRUE                               { Bool true }
| FALSE                              { Bool false }
| id = IDENT                         { Var id }
| e1 = expr o = arith_op e2 = expr   { Binop (o, e1, e2) }
| e1 = expr o = cmp_op e2 = expr     { Binop (o, e1, e2) }
| e1 = expr AND e2 = expr            { Binop (And, e1, e2) }
| e1 = expr OR e2 = expr             { Binop (Or, e1, e2) }
| NOT e = expr                       { Unop (Not, e) }
| MINUS e = expr %prec uminus        { Unop (Neg, e) }
| LET id = IDENT EQ e1 = expr IN e2 = expr
                                     { Letin (id, e1, e2) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
                                     { IfExpr (e1, e2, e3) }
| LP e = expr RP                     { e }
;

%inline arith_op:
| PLUS  { Add }
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
;

%inline cmp_op:
| EQEQ { Eq }
| NEQ  { Neq }
| LT   { Lt }
| LE   { Le }
| GT   { Gt }
| GE   { Ge }
;



