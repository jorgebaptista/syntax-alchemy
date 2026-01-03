(* Sintaxe abstracta para a linguagem Arith *)

type program = stmt list

and stmt =
  | Set of string * expr (* set x = e *)
  | Print of expr (* print e *)
  | If of expr * stmt list * stmt list (* if e then stmts else stmts *)
  | While of expr * stmt list (* while e do stmts done *)

and expr =
  | Cst of int (* integer constant *)
  | Bool of bool (* boolean constant: true, false *)
  | Var of string (* variable *)
  | Binop of binop * expr * expr (* binary operation *)
  | Unop of unop * expr (* unary operation *)
  | Letin of string * expr * expr (* let x = e1 in e2 *)
  | IfExpr of expr * expr * expr (* if e1 then e2 else e3 (expression) *)

and binop =
  | Add
  | Sub
  | Mul
  | Div (* arithmetic *)
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge (* comparison *)
  | And
  | Or (* logical *)

and unop =
  | Neg
  (* -e *)
  | Not (* not e *)
