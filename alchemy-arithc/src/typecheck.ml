open Format
open Ast

type typ =
  | TInt
  | TBool
  | TString
  | TNone
  | TList of typ
  | TFun of typ list * typ
  | TVar of tvar

and tvar = {
  id : int;
  mutable def : typ option;
  mutable addable : bool;
  mutable comparable : bool;
}

let rec pp_typ fmt = function
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"
  | TString -> fprintf fmt "string"
  | TNone -> fprintf fmt "none"
  | TList t -> fprintf fmt "list[%a]" pp_typ t
  | TFun (args, ret) ->
      fprintf fmt "@[<2>(%a) -> %a@]" pp_typ_list args pp_typ ret
  | TVar v -> pp_tvar fmt v

and pp_typ_list fmt = function
  | [] -> ()
  | [ t ] -> pp_typ fmt t
  | t :: ts -> fprintf fmt "%a, %a" pp_typ t pp_typ_list ts

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
      { id = !r; def = None; addable = false; comparable = false }
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
  match head t with
  | TInt -> TInt
  | TBool -> TBool
  | TString -> TString
  | TNone -> TNone
  | TList t -> TList (canon t)
  | TFun (args, ret) -> TFun (List.map canon args, canon ret)
  | TVar v -> TVar v

exception TypeError of typ * typ
exception VarUndef of string
exception FuncUndef of string
exception FuncRedef of string
exception ReturnOutside
exception ArityMismatch of string * int * int

let type_error t1 t2 = raise (TypeError (canon t1, canon t2))

let rec ensure_addable t =
  match head t with
  | TInt | TString -> ()
  | TList _ -> ()
  | TVar v -> v.addable <- true
  | t' -> type_error t' TInt

let rec ensure_comparable t =
  match head t with
  | TInt | TBool | TString -> ()
  | TList t' -> ensure_comparable t'
  | TVar v -> v.comparable <- true
  | t' -> type_error t' TInt

let constrain_addable t = ensure_addable t
let constrain_comparable t = ensure_comparable t

let merge_constraints v1 v2 =
  v2.addable <- v1.addable || v2.addable;
  v2.comparable <- v1.comparable || v2.comparable

let check_constraints v t =
  if v.addable then ensure_addable t;
  if v.comparable then ensure_comparable t

let rec occur v t =
  match head t with
  | TVar v' -> V.equal v v'
  | TInt | TBool | TString | TNone -> false
  | TList t -> occur v t
  | TFun (args, ret) -> List.exists (occur v) args || occur v ret

let rec unify t1 t2 =
  match (head t1, head t2) with
  | TInt, TInt -> ()
  | TBool, TBool -> ()
  | TString, TString -> ()
  | TNone, TNone -> ()
  | TList t1', TList t2' -> unify t1' t2'
  | TFun (args1, ret1), TFun (args2, ret2) ->
      if List.length args1 <> List.length args2 then type_error t1 t2
      else (
        List.iter2 unify args1 args2;
        unify ret1 ret2)
  | TVar v1, TVar v2 when V.equal v1 v2 -> ()
  | TVar v1, TVar v2 ->
      merge_constraints v1 v2;
      v1.def <- Some (TVar v2)
  | TVar v, t | t, TVar v ->
      if occur v t then type_error (TVar v) t
      else (
        check_constraints v t;
        v.def <- Some t)
  | t1', t2' -> type_error t1' t2'

module StrMap = Map.Make (String)

type env = { globals : typ StrMap.t; functions : typ StrMap.t }

let empty_env = { globals = StrMap.empty; functions = StrMap.empty }

let lookup_var env locals ~allow_globals name =
  match StrMap.find_opt name locals with
  | Some t -> t
  | None ->
      if allow_globals then
        match StrMap.find_opt name env.globals with
        | Some t -> t
        | None -> raise (VarUndef name)
      else raise (VarUndef name)

let lookup_fun env name =
  match StrMap.find_opt name env.functions with
  | Some t -> t
  | None -> raise (FuncUndef name)

let expect actual expected =
  unify actual expected;
  expected

let rec infer_expr env locals ~allow_globals = function
  | Cst _ -> TInt
  | Bool _ -> TBool
  | Str _ -> TString
  | NoneLit -> TNone
  | Var name -> lookup_var env locals ~allow_globals name
  | Call (name, args) ->
      let fn_ty = lookup_fun env name in
      let arg_tys =
        List.map (fun e -> infer_expr env locals ~allow_globals e) args
      in
      let expected_args =
        match head fn_ty with
        | TFun (params, _ret) -> List.length params
        | _ -> assert false
      in
      let actual_args = List.length arg_tys in
      if expected_args <> actual_args then
        raise (ArityMismatch (name, expected_args, actual_args));
      let ret_ty = TVar (V.create ()) in
      unify fn_ty (TFun (arg_tys, ret_ty));
      ret_ty
  | ListLit elems ->
      let elem_ty =
        match elems with
        | [] -> TVar (V.create ())
        | e1 :: rest ->
            let t1 = infer_expr env locals ~allow_globals e1 in
            List.iter
              (fun e ->
                let t = infer_expr env locals ~allow_globals e in
                unify t1 t)
              rest;
            t1
      in
      TList elem_ty
  | ListRange e ->
      let t = infer_expr env locals ~allow_globals e in
      ignore (expect t TInt);
      TList TInt
  | Get (e_list, e_index) ->
      let t_list = infer_expr env locals ~allow_globals e_list in
      let t_index = infer_expr env locals ~allow_globals e_index in
      ignore (expect t_index TInt);
      let elem_ty = TVar (V.create ()) in
      unify t_list (TList elem_ty);
      elem_ty
  | Len e ->
      let t = infer_expr env locals ~allow_globals e in
      let elem_ty = TVar (V.create ()) in
      unify t (TList elem_ty);
      TInt
  | Unop (Neg, e) ->
      let t = infer_expr env locals ~allow_globals e in
      ignore (expect t TInt);
      TInt
  | Unop (Not, e) ->
      let t = infer_expr env locals ~allow_globals e in
      ignore (expect t TBool);
      TBool
  | Binop (Add, e1, e2) ->
      let t1 = infer_expr env locals ~allow_globals e1 in
      let t2 = infer_expr env locals ~allow_globals e2 in
      unify t1 t2;
      constrain_addable t1;
      t1
  | Binop (Sub, e1, e2)
  | Binop (Mul, e1, e2)
  | Binop (Div, e1, e2)
  | Binop (Mod, e1, e2) ->
      let t1 = infer_expr env locals ~allow_globals e1 in
      let t2 = infer_expr env locals ~allow_globals e2 in
      ignore (expect t1 TInt);
      ignore (expect t2 TInt);
      TInt
  | Binop (And, e1, e2) | Binop (Or, e1, e2) ->
      let t1 = infer_expr env locals ~allow_globals e1 in
      let t2 = infer_expr env locals ~allow_globals e2 in
      ignore (expect t1 TBool);
      ignore (expect t2 TBool);
      TBool
  | Binop (Eq, e1, e2) | Binop (Neq, e1, e2) ->
      let t1 = infer_expr env locals ~allow_globals e1 in
      let t2 = infer_expr env locals ~allow_globals e2 in
      unify t1 t2;
      TBool
  | Binop (Lt, e1, e2)
  | Binop (Le, e1, e2)
  | Binop (Gt, e1, e2)
  | Binop (Ge, e1, e2) ->
      let t1 = infer_expr env locals ~allow_globals e1 in
      let t2 = infer_expr env locals ~allow_globals e2 in
      unify t1 t2;
      constrain_comparable t1;
      TBool
  | Letin (name, e1, e2) ->
      let t1 = infer_expr env locals ~allow_globals e1 in
      infer_expr env (StrMap.add name t1 locals) ~allow_globals e2
  | IfExpr (cond, e1, e2) ->
      let tcond = infer_expr env locals ~allow_globals cond in
      ignore (expect tcond TBool);
      let t1 = infer_expr env locals ~allow_globals e1 in
      let t2 = infer_expr env locals ~allow_globals e2 in
      unify t1 t2;
      t1

let rec check_stmt env locals ~allow_globals ~ret_ty = function
  | Set (name, e) ->
      let t = infer_expr env locals ~allow_globals e in
      if allow_globals then
        let globals =
          match StrMap.find_opt name env.globals with
          | None -> StrMap.add name t env.globals
          | Some prev ->
              unify prev t;
              env.globals
        in
        ({ env with globals }, locals)
      else
        let locals =
          match StrMap.find_opt name locals with
          | None -> StrMap.add name t locals
          | Some prev ->
              unify prev t;
              locals
        in
        (env, locals)
  | SetIndex (e_list, e_index, e_value) ->
      let t_list = infer_expr env locals ~allow_globals e_list in
      let t_index = infer_expr env locals ~allow_globals e_index in
      let t_value = infer_expr env locals ~allow_globals e_value in
      ignore (expect t_index TInt);
      let elem_ty = TVar (V.create ()) in
      unify t_list (TList elem_ty);
      unify t_value elem_ty;
      (env, locals)
  | For (name, e_list, body) ->
      let t_list = infer_expr env locals ~allow_globals e_list in
      let elem_ty = TVar (V.create ()) in
      unify t_list (TList elem_ty);
      let env, locals =
        if allow_globals then
          let globals =
            match StrMap.find_opt name env.globals with
            | None -> StrMap.add name elem_ty env.globals
            | Some prev ->
                unify prev elem_ty;
                env.globals
          in
          ({ env with globals }, locals)
        else
          let locals =
            match StrMap.find_opt name locals with
            | None -> StrMap.add name elem_ty locals
            | Some prev ->
                unify prev elem_ty;
                locals
          in
          (env, locals)
      in
      check_block env locals ~allow_globals ~ret_ty body
  | Print e ->
      ignore (infer_expr env locals ~allow_globals e);
      (env, locals)
  | Expr e ->
      ignore (infer_expr env locals ~allow_globals e);
      (env, locals)
  | Return e -> (
      match ret_ty with
      | None -> raise ReturnOutside
      | Some ret ->
          let t = infer_expr env locals ~allow_globals e in
          unify t ret;
          (env, locals))
  | If (cond, then_stmts, else_stmts) ->
      let tcond = infer_expr env locals ~allow_globals cond in
      ignore (expect tcond TBool);
      let env, locals =
        check_block env locals ~allow_globals ~ret_ty then_stmts
      in
      check_block env locals ~allow_globals ~ret_ty else_stmts
  | While (cond, body) ->
      let tcond = infer_expr env locals ~allow_globals cond in
      ignore (expect tcond TBool);
      check_block env locals ~allow_globals ~ret_ty body

and check_block env locals ~allow_globals ~ret_ty stmts =
  List.fold_left
    (fun (env, locals) stmt ->
      check_stmt env locals ~allow_globals ~ret_ty stmt)
    (env, locals) stmts

let register_function env (name, params, _body) =
  if StrMap.mem name env.functions then raise (FuncRedef name);
  let param_tys = List.map (fun _ -> TVar (V.create ())) params in
  let ret_ty = TVar (V.create ()) in
  let fn_ty = TFun (param_tys, ret_ty) in
  { env with functions = StrMap.add name fn_ty env.functions }

let rec stmt_has_return = function
  | Return _ -> true
  | If (_, then_stmts, else_stmts) ->
      List.exists stmt_has_return then_stmts
      || List.exists stmt_has_return else_stmts
  | While (_, body) -> List.exists stmt_has_return body
  | For (_, _, body) -> List.exists stmt_has_return body
  | Set _ | SetIndex _ | Print _ | Expr _ -> false

let check_def env (name, params, body) =
  let fn_ty = lookup_fun env name in
  let param_tys, ret_ty =
    match fn_ty with TFun (args, ret) -> (args, ret) | _ -> assert false
  in
  let locals =
    List.fold_left2
      (fun acc param ty -> StrMap.add param ty acc)
      StrMap.empty params param_tys
  in
  ignore
    (check_block env locals ~allow_globals:false ~ret_ty:(Some ret_ty) body);
  if not (List.exists stmt_has_return body) then unify ret_ty TNone

let check_program (defs, stmts) =
  let env = List.fold_left register_function empty_env defs in
  List.iter (check_def env) defs;
  let env, _locals =
    check_block env StrMap.empty ~allow_globals:true ~ret_ty:None stmts
  in
  env
