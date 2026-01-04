open Format
open Ast

type typ = TInt | TBool | TVar of tvar
and tvar = { id : int; mutable def : typ option }

let rec pp_typ fmt = function
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"
  | TVar v -> pp_tvar fmt v

and pp_tvar fmt = function
  | { def = None; id } -> fprintf fmt "'%d" id
  | { def = Some t; id } -> fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t

module V = struct
  type t = tvar

  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id

  let create =
    let r = ref 0 in
    fun () ->
      incr r;
      { id = !r; def = None }
end

let rec head = function
  | TVar v -> (
      match v.def with
      | None -> TVar v
      | Some t ->
          let h = head t in
          v.def <- Some h;
          h)
  | t -> t

let rec canon t =
  match head t with TInt -> TInt | TBool -> TBool | TVar v -> TVar v

exception TypeError of typ * typ
exception VarUndef of string

let type_error t1 t2 = raise (TypeError (canon t1, canon t2))

let rec occur v t =
  match head t with TVar v' -> V.equal v v' | TInt | TBool -> false

let rec unify t1 t2 =
  match (head t1, head t2) with
  | TInt, TInt -> ()
  | TBool, TBool -> ()
  | TVar v1, TVar v2 when V.equal v1 v2 -> ()
  | TVar v, t | t, TVar v ->
      if occur v t then type_error (TVar v) t else v.def <- Some t
  | t1', t2' -> type_error t1' t2'

module StrMap = Map.Make (String)

type env = { globals : typ StrMap.t }

let empty_env = { globals = StrMap.empty }

let lookup env locals name =
  match StrMap.find_opt name locals with
  | Some t -> t
  | None -> (
      match StrMap.find_opt name env.globals with
      | Some t -> t
      | None -> raise (VarUndef name))

let expect actual expected =
  unify actual expected;
  expected

let rec infer_expr env locals = function
  | Cst _ -> TInt
  | Bool _ -> TBool
  | Var name -> lookup env locals name
  | Unop (Neg, e) ->
      let t = infer_expr env locals e in
      ignore (expect t TInt);
      TInt
  | Unop (Not, e) ->
      let t = infer_expr env locals e in
      ignore (expect t TBool);
      TBool
  | Binop (Add, e1, e2)
  | Binop (Sub, e1, e2)
  | Binop (Mul, e1, e2)
  | Binop (Div, e1, e2) ->
      let t1 = infer_expr env locals e1 in
      let t2 = infer_expr env locals e2 in
      ignore (expect t1 TInt);
      ignore (expect t2 TInt);
      TInt
  | Binop (And, e1, e2) | Binop (Or, e1, e2) ->
      let t1 = infer_expr env locals e1 in
      let t2 = infer_expr env locals e2 in
      ignore (expect t1 TBool);
      ignore (expect t2 TBool);
      TBool
  | Binop (Eq, e1, e2) | Binop (Neq, e1, e2) ->
      let t1 = infer_expr env locals e1 in
      let t2 = infer_expr env locals e2 in
      unify t1 t2;
      TBool
  | Binop (Lt, e1, e2)
  | Binop (Le, e1, e2)
  | Binop (Gt, e1, e2)
  | Binop (Ge, e1, e2) ->
      let t1 = infer_expr env locals e1 in
      let t2 = infer_expr env locals e2 in
      ignore (expect t1 TInt);
      ignore (expect t2 TInt);
      TBool
  | Letin (name, e1, e2) ->
      let t1 = infer_expr env locals e1 in
      infer_expr env (StrMap.add name t1 locals) e2
  | IfExpr (cond, e1, e2) ->
      let tcond = infer_expr env locals cond in
      ignore (expect tcond TBool);
      let t1 = infer_expr env locals e1 in
      let t2 = infer_expr env locals e2 in
      unify t1 t2;
      t1

let rec check_stmt env = function
  | Set (name, e) ->
      let t = infer_expr env StrMap.empty e in
      let globals =
        match StrMap.find_opt name env.globals with
        | None -> StrMap.add name t env.globals
        | Some prev ->
            unify prev t;
            env.globals
      in
      { globals }
  | Print e ->
      ignore (infer_expr env StrMap.empty e);
      env
  | If (cond, then_stmts, else_stmts) ->
      let tcond = infer_expr env StrMap.empty cond in
      ignore (expect tcond TBool);
      let env = check_block env then_stmts in
      check_block env else_stmts
  | While (cond, body) ->
      let tcond = infer_expr env StrMap.empty cond in
      ignore (expect tcond TBool);
      check_block env body

and check_block env stmts = List.fold_left check_stmt env stmts

let check_program prog = ignore (check_block empty_env prog)
