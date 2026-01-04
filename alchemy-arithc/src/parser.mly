
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
%token DEF, RETURN
%token LEN
%token FOR
%token AND, OR, NOT
%token EOF
%token LP RP
%token LSQ RSQ
%token COMMA
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
%nonassoc LSQ

/* Entry point para a gramática */
%start prog

/* Tipo dos valores devolvidos pelo analisador sintático */
%type <Ast.program> prog

%%

prog:
| dl = list(def) s = opt_stmts EOF { (dl, s) }
;

stmts:
| i = stmt               { [i] }
| l = stmts i = stmt     { i :: l }
;

opt_stmts:
| /* empty */            { [] }
| l = stmts              { List.rev l }
;

def:
| DEF f = IDENT LP params = separated_list(COMMA, IDENT) RP b = block
    { (f, params, b) }
;

stmt:
| SET id = IDENT EQ e = expr 
    { Set (id, e) }
| SET id = IDENT LSQ i = expr RSQ EQ e = expr
    { SetIndex (Var id, i, e) }
| PRINT e = expr             
    { Print e }
| IF e = expr THEN b1 = block ELSE b2 = block
    { If (e, b1, b2) }
| IF e = expr THEN b1 = block
    { If (e, b1, []) }
| WHILE e = expr DO b = while_block DONE
    { While (e, b) }
| FOR id = IDENT IN e = expr DO b = while_block DONE
    { For (id, e, b) }
| RETURN e = expr
    { Return e }
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
| f = IDENT LP args = separated_list(COMMA, expr) RP
                                     { Call (f, args) }
| LEN LP e = expr RP                 { Len e }
| LSQ l = separated_list(COMMA, expr) RSQ
                                     { ListLit l }
| e1 = expr LSQ e2 = expr RSQ %prec LSQ
                                     { Get (e1, e2) }
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
