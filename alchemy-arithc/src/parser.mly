/* Analisador sintático para Arith */

/* Import do módulo AST para dentro do código do parser gerado */
%{
  open Ast
%}

/* Lista de todos os tokens léxicos que o parser reconhece, produzidos pelo lexer e consumidos pelo parser */ 
%token <int> CST
%token <string> IDENT
%token <string> STRING
%token SET, LET, IN, PRINT
%token TRUE, FALSE, NONE
%token IF, THEN, ELSE
%token WHILE, DO, DONE
%token DEF, RETURN
%token LEN, LIST, RANGE
%token FOR
%token AND, OR, NOT
%token EOF
%token LP RP
%token LSQ RSQ
%token COMMA
%token PLUS MINUS TIMES DIV MOD
%token EQ EQEQ NEQ LT LE GT GE

/* Definição das prioridades e das associatividades dos tokens 
    - nonassoc -> Não permite encadeamento
    - right -> Agrupa da direita para a esquerda
    - left -> Agrupa da esquerda para a direita */
%nonassoc IN
%right ELSE              /* resolve dangling else */
%left OR
%left AND
%left EQEQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc NOT
%nonassoc uminus
%nonassoc LSQ

/* Entry point para a gramática */
%start prog

/* Tipo dos valores devolvidos pelo analisador sintático */
%type <Ast.program> prog

/* Regras de gramática */
%%

/* Define o formato de um programa completo:
    - dl = list(def) -> o programa começa com zero ou mais definições de função (def)
    - s = opt_stmts -> depois tem zero ou mais statements, que corresponde ao corpo principal do programa
    - EOF -> End of File para garantir que o parser consumiu tudo */
prog:
| dl = list(def) s = opt_stmts EOF { (dl, s) }
;

/* Regra de construção de listas de statements:
    - 1 statement -> retorna [i] 
    - muitos statements -> vai empilhando com i::l */
stmts:
| i = stmt               { [i] }
| l = stmts i = stmt     { i :: l }
;

/* Regras opcionais de statements:
    - Pode ser uma lista vazia -> []
    - Pode também ser vários statements. Neste caso, faz List.rev l para retornar à ordem correta */
opt_stmts:
| /* empty */            { [] }
| l = stmts              { List.rev l }
;

/* Descrição da definição de função:
    - DEF -> keyword def
    - f = IDENT -> nome da função
    - LP ... RP -> parênteses (Left Parentheses e Right Parentheses)
    - params = separated_list(COMMA, IDENT) -> lista de parâmetros separados por vírgula
    - b = block -> corpo da função 
    Devolve o tipo definido em AST -> def = string * string list * stmt list */
def:
| DEF f = IDENT LP params = separated_list(COMMA, IDENT) RP b = block
    { (f, params, b) }
;

/* Regras para construção de statements.
   Cada alternativa corresponde a um tipo de instrução da linguagem,
   sendo diretamente mapeada para o construtor equivalente no AST. */
/* Atribuições */
stmt:
| SET id = IDENT EQ e = expr 
    { Set (id, e) }
| SET id = IDENT LSQ i = expr RSQ EQ e = expr
    { SetIndex (Var id, i, e) }
    
/* Output e avaliação de expressões */
| PRINT e = expr             
    { Print e }
| e = expr
    { Expr e }

/* Condicionais */
| IF e = expr THEN b1 = block ELSE b2 = block
    { If (e, b1, b2) }
| IF e = expr THEN b1 = block
    { If (e, b1, []) }

/* Ciclos */
| WHILE e = expr DO b = while_block DONE
    { While (e, b) }
| FOR id = IDENT IN e = expr DO b = while_block DONE
    { For (id, e, b) }

/* Return */
| RETURN e = expr
    { Return e }
;

/* Corpo de ciclos (while / for), representado como uma lista de statements */
while_block:
| /* empty */        { [] }
| l = while_block s = stmt { l @ [s] }
;

/* Bloco de instruções, podendo conter um ou vários statements */
block:
| s = stmt           { [s] }
| LP l = stmts RP    { List.rev l }
;

/* Regras das expressões da linguagem.
   Estas expressões representam computações que produzem um valor, podendo ser inteiro, booleano, string, etc
   Podem ser combinadas recursivamente para foramr expressões mais completas */
expr:
/* Literais e variáveis */
| c = CST                            { Cst c }
| TRUE                               { Bool true }
| FALSE                              { Bool false }
| STRING                             { Str $1 }
| NONE                               { NoneLit }
| id = IDENT                         { Var id }
/* Chamadas de função e funções built-in */
| f = IDENT LP args = separated_list(COMMA, expr) RP
                                     { Call (f, args) }
| LIST LP RANGE LP e = expr RP RP    { ListRange e }
| LEN LP e = expr RP                 { Len e }
/* Construção e acesso a listas */
| LSQ l = separated_list(COMMA, expr) RSQ
                                     { ListLit l }
| e1 = expr LSQ e2 = expr RSQ %prec LSQ
/* Operações aritméticas, de comparação e lógicas */
                                     { Get (e1, e2) }
| e1 = expr o = arith_op e2 = expr   { Binop (o, e1, e2) }
| e1 = expr o = cmp_op e2 = expr     { Binop (o, e1, e2) }
| e1 = expr AND e2 = expr            { Binop (And, e1, e2) }
| e1 = expr OR e2 = expr             { Binop (Or, e1, e2) }
| NOT e = expr                       { Unop (Not, e) }
| MINUS e = expr %prec uminus        { Unop (Neg, e) }
/* Expressões compostas */
| LET id = IDENT EQ e1 = expr IN e2 = expr
                                     { Letin (id, e1, e2) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
                                     { IfExpr (e1, e2, e3) }
/* Agrupamento de expressões */                                     
| LP e = expr RP                     { e }
;

/* Operadores aritméticos.
   Traduz os tokens aritméticos da linguagem (+, -, *, /, %)
   para os construtores correspondentes do AST */
%inline arith_op:
| PLUS  { Add }
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
| MOD   { Mod }
;

/* Operadores de comparação.
   Traduz os tokens de comparação (==, !=, <, <=, >, >=)
   para os construtores correspondentes do AST */
%inline cmp_op:
| EQEQ { Eq }
| NEQ  { Neq }
| LT   { Lt }
| LE   { Le }
| GT   { Gt }
| GE   { Ge }
;
