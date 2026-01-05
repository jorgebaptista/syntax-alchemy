(* Sintaxe abstracta para a linguagem Arith *)

(* Um programa é representado por:
    - Uma lista de definições de funções no ambiente global (def list)
    - Uma lista de instruções executadas sequencialmente com o corpo principal do programa (stmt list) 
    Tem como objetivo facilitar a analise semântica e a geração de código nas diferentes fases do compilador*)  
type program = def list * stmt list

(* Uma definição de função é representada por:
    - O nome da função (string)
    - A lista de parâmetros formais (string list)
    - O corpo da função, representado como uma lista de instruções (stmt list)*)
and def = string * string list * stmt list

(* Instruções (statements) da linguagem. Representam ações executáveis que afetam o estado do programa
  e definem o controlo de fluxo durante a execução *)
and stmt =
  | Set of string * expr (* set x = e *)
  | SetIndex of expr * expr * expr (* set a[i] = e *)
  | Print of expr (* print e *)
  | Expr of expr (* avaliar expressão e descartar o resultado *)
  | If of expr * stmt list * stmt list (* if e then stmts else stmts *)
  | While of expr * stmt list (* while e do stmts done *)
  | For of string * expr * stmt list (* for x in e do stmts done *)
  | Return of expr (* return e *)

(* Uma expressão (expr) representa uma construção que, quando avaliada,
  produz um valor (inteiro, string, etc) *)
and expr =
  | Cst of int (* integer constant *)
  | Bool of bool (* boolean constant: true, false *)
  | Str of string (* string constant *)
  | NoneLit (* none constant *)
  | Var of string (* variable *)
  | Binop of binop * expr * expr (* binary operation *)
  | Unop of unop * expr (* unary operation *)
  | Letin of string * expr * expr (* let x = e1 in e2 *)
  | IfExpr of expr * expr * expr (* if e1 then e2 else e3 (expression) *)
  | Call of string * expr list (* function call f(e1, e2, ...) *)
  | ListLit of expr list (* [e1, e2, ...] *)
  | ListRange of expr (* list(range(e)) *)
  | Get of expr * expr (* e1[e2] *)
  | Len of expr (* len(e) *)

(* Operadores binários *)  
and binop =
  (* Aritméticos *)
  | Add
  | Sub
  | Mul
  | Div 
  | Mod
  (* Comparação *)
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  (* Lógicos *)
  | And
  | Or

(* Operadores unários *)  
and unop =
  (* Negação aritmética *)
  | Neg (* -e *)
  (* Negação lógica *)
  | Not (* not e *)
