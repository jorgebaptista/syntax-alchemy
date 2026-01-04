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

and tvar = { id : int; mutable def : typ option }

exception TypeError of typ * typ
exception VarUndef of string
exception FuncUndef of string
exception FuncRedef of string
exception ReturnOutside
exception ArityMismatch of string * int * int

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

module StrMap = Map.Make (String)
module Vset = Set.Make (V)
module Vmap = Map.Make (V)

type schema = { vars : Vset.t; typ : typ }

let mono_schema typ = { vars = Vset.empty; typ }

type env = { globals : schema StrMap.t; functions : typ StrMap.t }

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
  | TList t' -> TList (canon t')
  | TFun (args, ret) -> TFun (List.map canon args, canon ret)
  | TVar v -> TVar v

let rec pp_typ fmt = function
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"
  | TString -> fprintf fmt "string"
  | TNone -> fprintf fmt "none"
  | TList t -> fprintf fmt "list[%a]" pp_typ t
  | TFun (args, ret) ->
      let pp_args fmt =
        match args with
        | [ t ] -> fprintf fmt "%a" pp_typ t
        | _ ->
            fprintf fmt "(";
            List.iteri
              (fun i t ->
                if i > 0 then fprintf fmt ", ";
                fprintf fmt "%a" pp_typ t)
              args;
            fprintf fmt ")"
      in
      fprintf fmt "%t -> %a" pp_args pp_typ ret
  | TVar v -> pp_tvar fmt v

and pp_tvar fmt = function
  | { def = None; id } -> fprintf fmt "'%d" id
  | { def = Some t; id } -> fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t

let type_error t1 t2 = raise (TypeError (canon t1, canon t2))

let rec occur v t =
  match head t with
  | TVar v' -> V.equal v v'
  | TInt | TBool | TString | TNone -> false
  | TList t' -> occur v t'
  | TFun (args, ret) -> List.exists (occur v) args || occur v ret

let rec unify t1 t2 =
  match (head t1, head t2) with
  | TInt, TInt -> ()
  | TBool, TBool -> ()
  | TString, TString -> ()
  | TNone, TNone -> ()
  | TList t1', TList t2' -> unify t1' t2'
  | TFun (args1, ret1), TFun (args2, ret2) ->
      if List.length args1 <> List.length args2 then type_error t1 t2;
      List.iter2 unify args1 args2;
      unify ret1 ret2
  | TVar v1, TVar v2 when V.equal v1 v2 -> ()
  | TVar v, t | t, TVar v ->
      if occur v t then type_error (TVar v) t else v.def <- Some t
  | t1', t2' -> type_error t1' t2'

let fresh_var () = TVar (V.create ())

let expect actual expected =
  unify actual expected;
  expected

let rec fvars t =
  match head t with
  | TInt | TBool | TString | TNone -> Vset.empty
  | TList t' -> fvars t'
  | TFun (args, ret) ->
      List.fold_left (fun acc t -> Vset.union acc (fvars t)) (fvars ret) args
  | TVar v -> Vset.singleton v

let fvars_schema { vars; typ } = Vset.diff (fvars typ) vars

let fvars_bindings bindings =
  StrMap.fold
    (fun _ schema acc -> Vset.union acc (fvars_schema schema))
    bindings Vset.empty

let env_fvars ~allow_globals env locals =
  let locals_fvars = fvars_bindings locals in
  if allow_globals then Vset.union locals_fvars (fvars_bindings env.globals)
  else locals_fvars

let generalize ~allow_globals env locals typ =
  let vars = Vset.diff (fvars typ) (env_fvars ~allow_globals env locals) in
  { vars; typ }

let instantiate schema =
  let subs = ref Vmap.empty in
  let rec inst t =
    match head t with
    | TInt -> TInt
    | TBool -> TBool
    | TString -> TString
    | TNone -> TNone
    | TList t' -> TList (inst t')
    | TFun (args, ret) -> TFun (List.map inst args, inst ret)
    | TVar v ->
        if Vset.mem v schema.vars then (
          match Vmap.find_opt v !subs with
          | Some v' -> TVar v'
          | None ->
              let v' = V.create () in
              subs := Vmap.add v v' !subs;
              TVar v')
        else TVar v
  in
  inst schema.typ

let empty_env = { globals = StrMap.empty; functions = StrMap.empty }

let lookup_fun env name =
  match StrMap.find_opt name env.functions with
  | Some ty -> ty
  | None -> raise (FuncUndef name)

let lookup_var ~allow_globals env locals name =
  match StrMap.find_opt name locals with
  | Some schema -> instantiate schema
  | None ->
      if allow_globals then
        match StrMap.find_opt name env.globals with
        | Some schema -> instantiate schema
        | None -> raise (VarUndef name)
      else raise (VarUndef name)

let default_int t =
  match head t with
  | TVar v ->
      v.def <- Some TInt;
      TInt
  | t' -> t'

let ensure_addable t =
  match head (default_int t) with
  | TInt -> TInt
  | TString -> TString
  | TList t' -> TList t'
  | t' -> type_error t' TInt

let rec ensure_orderable t =
  match head (default_int t) with
  | TInt | TString -> TBool
  | TList t' ->
      ignore (ensure_orderable t');
      TBool
  | t' -> type_error t' TInt

let rec infer_expr env locals ~allow_globals = function
  | Cst _ -> TInt
  | Bool _ -> TBool
  | Str _ -> TString
  | NoneLit -> TNone
  | Var name -> lookup_var ~allow_globals env locals name
  | Call (name, args) -> (
      match lookup_fun env name with
      | TFun (params, ret) ->
          let expected = List.length params in
          let actual = List.length args in
          if expected <> actual then
            raise (ArityMismatch (name, expected, actual));
          List.iter2
            (fun param arg ->
              let targ = infer_expr env locals ~allow_globals arg in
              ignore (expect targ param))
            params args;
          ret
      | _ -> assert false)
  | ListLit elems -> (
      match elems with
      | [] -> TList (fresh_var ())
      | e :: rest ->
          let t = infer_expr env locals ~allow_globals e in
          List.iter
            (fun e' ->
              let t' = infer_expr env locals ~allow_globals e' in
              unify t t')
            rest;
          TList t)
  | ListRange e ->
      let t = infer_expr env locals ~allow_globals e in
      ignore (expect t TInt);
      TList TInt
  | Get (e_list, e_index) ->
      let t_list = infer_expr env locals ~allow_globals e_list in
      let t_index = infer_expr env locals ~allow_globals e_index in
      ignore (expect t_index TInt);
      let t_elem = fresh_var () in
      unify t_list (TList t_elem);
      t_elem
  | Len e ->
      let t = infer_expr env locals ~allow_globals e in
      let t_elem = fresh_var () in
      unify t (TList t_elem);
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
      ensure_addable t1
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
      ensure_orderable t1
  | Letin (name, e1, e2) ->
      let t1 = infer_expr env locals ~allow_globals e1 in
      let schema = generalize ~allow_globals env locals t1 in
      let locals = StrMap.add name schema locals in
      infer_expr env locals ~allow_globals e2
  | IfExpr (cond, e1, e2) ->
      let tcond = infer_expr env locals ~allow_globals cond in
      ignore (expect tcond TBool);
      let t1 = infer_expr env locals ~allow_globals e1 in
      let t2 = infer_expr env locals ~allow_globals e2 in
      unify t1 t2;
      t1

let merge_branch_maps before then_map else_map =
  let module StrSet = Set.Make (String) in
  let keys =
    StrMap.fold (fun name _ acc -> StrSet.add name acc) then_map StrSet.empty
    |> StrMap.fold (fun name _ acc -> StrSet.add name acc) else_map
  in
  let out = ref before in
  StrSet.iter
    (fun name ->
      let s_before = StrMap.find_opt name before in
      let s_then = StrMap.find_opt name then_map in
      let s_else = StrMap.find_opt name else_map in
      match (s_before, s_then, s_else) with
      | Some sb, Some st, Some se ->
          unify sb.typ st.typ;
          unify sb.typ se.typ
      | Some sb, Some st, None -> unify sb.typ st.typ
      | Some sb, None, Some se -> unify sb.typ se.typ
      | None, Some st, Some se ->
          unify st.typ se.typ;
          out := StrMap.add name (mono_schema st.typ) !out
      | _ -> ())
    keys;
  !out

let merge_loop_maps before body_map =
  StrMap.iter
    (fun name s_body ->
      match StrMap.find_opt name before with
      | Some s_before -> unify s_before.typ s_body.typ
      | None -> ())
    body_map;
  before

let rec check_stmt env locals ~in_function ~allow_globals ret_type = function
  | Set (name, e) ->
      let t = infer_expr env locals ~allow_globals e in
      let locals =
        match StrMap.find_opt name locals with
        | None -> StrMap.add name (mono_schema t) locals
        | Some prev ->
            unify prev.typ t;
            locals
      in
      (env, locals)
  | SetIndex (e_list, e_index, e_value) ->
      let t_list = infer_expr env locals ~allow_globals e_list in
      let t_index = infer_expr env locals ~allow_globals e_index in
      let t_value = infer_expr env locals ~allow_globals e_value in
      ignore (expect t_index TInt);
      let t_elem = fresh_var () in
      unify t_list (TList t_elem);
      unify t_elem t_value;
      (env, locals)
  | Print e ->
      ignore (infer_expr env locals ~allow_globals e);
      (env, locals)
  | Expr e ->
      ignore (infer_expr env locals ~allow_globals e);
      (env, locals)
  | If (cond, then_stmts, else_stmts) ->
      let tcond = infer_expr env locals ~allow_globals cond in
      ignore (expect tcond TBool);
      let _, locals_then =
        check_block env locals ~in_function ~allow_globals ret_type then_stmts
      in
      let _, locals_else =
        check_block env locals ~in_function ~allow_globals ret_type else_stmts
      in
      let locals = merge_branch_maps locals locals_then locals_else in
      (env, locals)
  | While (cond, body) ->
      let tcond = infer_expr env locals ~allow_globals cond in
      ignore (expect tcond TBool);
      let _, locals_body =
        check_block env locals ~in_function ~allow_globals ret_type body
      in
      let locals = merge_loop_maps locals locals_body in
      (env, locals)
  | For (name, e_list, body) ->
      let t_list = infer_expr env locals ~allow_globals e_list in
      let t_elem = fresh_var () in
      unify t_list (TList t_elem);
      let locals =
        match StrMap.find_opt name locals with
        | None -> StrMap.add name (mono_schema t_elem) locals
        | Some prev ->
            unify prev.typ t_elem;
            locals
      in
      let _, locals_body =
        check_block env locals ~in_function ~allow_globals ret_type body
      in
      let locals = merge_loop_maps locals locals_body in
      (env, locals)
  | Return e ->
      if not in_function then raise ReturnOutside;
      let t = infer_expr env locals ~allow_globals e in
      ignore (expect t ret_type);
      (env, locals)

and check_block env locals ~in_function ~allow_globals ret_type stmts =
  List.fold_left
    (fun (_env, locals) stmt ->
      check_stmt env locals ~in_function ~allow_globals ret_type stmt)
    (env, locals) stmts

let register_function env (name, params, _body) =
  if StrMap.mem name env.functions then raise (FuncRedef name);
  let param_types = List.map (fun _ -> fresh_var ()) params in
  let ret_type = fresh_var () in
  let fun_type = TFun (param_types, ret_type) in
  { env with functions = StrMap.add name fun_type env.functions }

let rec stmt_has_return = function
  | Return _ -> true
  | If (_, then_stmts, else_stmts) ->
      List.exists stmt_has_return then_stmts
      || List.exists stmt_has_return else_stmts
  | While (_, body) | For (_, _, body) -> List.exists stmt_has_return body
  | Set _ | SetIndex _ | Print _ | Expr _ -> false

let check_def env (name, params, body) =
  match lookup_fun env name with
  | TFun (param_types, ret_type) ->
      let locals =
        List.fold_left2
          (fun acc param t -> StrMap.add param (mono_schema t) acc)
          StrMap.empty params param_types
      in
      ignore
        (check_block env locals ~in_function:true ~allow_globals:false ret_type
           body);
      if not (List.exists stmt_has_return body) then unify ret_type TNone
  | _ -> assert false

let check_program (defs, stmts) =
  let env = List.fold_left register_function empty_env defs in
  List.iter (check_def env) defs;
  let _, globals =
    check_block env env.globals ~in_function:false ~allow_globals:true
      (fresh_var ()) stmts
  in
  { env with globals }
