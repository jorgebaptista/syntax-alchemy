open Ast

exception VarUndef of string
exception FuncUndef of string
exception FuncRedef of string
exception ReturnOutside
exception ArityMismatch of string * int * int

module StrMap = Map.Make (String)

type env = { functions : int StrMap.t }

let empty_env = { functions = StrMap.empty }

let lookup_fun env name =
  match StrMap.find_opt name env.functions with
  | Some arity -> arity
  | None -> raise (FuncUndef name)

let register_function env (name, params, _body) =
  if StrMap.mem name env.functions then raise (FuncRedef name);
  { env with functions = StrMap.add name (List.length params) env.functions }

let rec check_expr env = function
  | Cst _ | Bool _ | Str _ | NoneLit | Var _ -> ()
  | ListLit elems -> List.iter (check_expr env) elems
  | ListRange e -> check_expr env e
  | Get (e1, e2) ->
      check_expr env e1;
      check_expr env e2
  | Len e -> check_expr env e
  | Unop (_, e) -> check_expr env e
  | Binop (_, e1, e2) ->
      check_expr env e1;
      check_expr env e2
  | Letin (_, e1, e2) ->
      check_expr env e1;
      check_expr env e2
  | IfExpr (c, e1, e2) ->
      check_expr env c;
      check_expr env e1;
      check_expr env e2
  | Call (name, args) ->
      let expected = lookup_fun env name in
      let actual = List.length args in
      if expected <> actual then raise (ArityMismatch (name, expected, actual));
      List.iter (check_expr env) args

let rec check_stmt env ~in_function = function
  | Set (_, e) -> check_expr env e
  | SetIndex (e1, e2, e3) ->
      check_expr env e1;
      check_expr env e2;
      check_expr env e3
  | Print e -> check_expr env e
  | Expr e -> check_expr env e
  | If (c, t, e) ->
      check_expr env c;
      List.iter (check_stmt env ~in_function) t;
      List.iter (check_stmt env ~in_function) e
  | While (c, body) ->
      check_expr env c;
      List.iter (check_stmt env ~in_function) body
  | For (_, e, body) ->
      check_expr env e;
      List.iter (check_stmt env ~in_function) body
  | Return e ->
      if not in_function then raise ReturnOutside;
      check_expr env e

let check_def env (_name, _params, body) =
  List.iter (check_stmt env ~in_function:true) body

let check_program (defs, stmts) =
  let env = List.fold_left register_function empty_env defs in
  List.iter (check_def env) defs;
  List.iter (check_stmt env ~in_function:false) stmts;
  env
