open Format

type typ =
  | Tint
  | Tarrow of typ * typ
  | Tproduct of typ * typ
  | Tvar of tvar

and tvar = {
  id : int;
  mutable def : typ option;
}

let rec pp_typ fmt = function
  | Tproduct (t1, t2) -> fprintf fmt "%a *@ %a" pp_atom t1 pp_atom t2
  | Tarrow (t1, t2) -> fprintf fmt "%a ->@ %a" pp_atom t1 pp_typ t2
  | (Tint | Tvar _) as t -> pp_atom fmt t

and pp_atom fmt = function
  | Tint -> fprintf fmt "int"
  | Tvar v -> pp_tvar fmt v
  | Tarrow _ | Tproduct _ as t -> fprintf fmt "@[<1>(%a)@]" pp_typ t

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
  | Tvar v -> (
      match v.def with
      | None -> Tvar v
      | Some t ->
          let h = head t in
          v.def <- Some h;
          h)
  | t -> t

let rec canon t =
  match head t with
  | Tint -> Tint
  | Tvar v -> Tvar v
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)
  | Tproduct (t1, t2) -> Tproduct (canon t1, canon t2)

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

let rec occur v t =
  match head t with
  | Tvar v' -> V.equal v v'
  | Tint -> false
  | Tarrow (t1, t2) | Tproduct (t1, t2) -> occur v t1 || occur v t2

let rec unify t1 t2 =
  match (head t1, head t2) with
  | Tint, Tint -> ()
  | Tarrow (t1, t2), Tarrow (t1', t2') ->
      unify t1 t1';
      unify t2 t2'
  | Tproduct (t1, t2), Tproduct (t1', t2') ->
      unify t1 t1';
      unify t2 t2'
  | Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
  | Tvar v, t | t, Tvar v ->
      if occur v t then unification_error (Tvar v) t else v.def <- Some t
  | t1', t2' -> unification_error t1' t2'

module Vset = Set.Make (V)

let rec fvars t =
  match head t with
  | Tint -> Vset.empty
  | Tvar v -> Vset.singleton v
  | Tarrow (t1, t2) | Tproduct (t1, t2) ->
      Vset.union (fvars t1) (fvars t2)

type schema = { vars : Vset.t; typ : typ }

module Smap = Map.Make (String)
module Vmap = Map.Make (V)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = { bindings = Smap.empty; fvars = Vset.empty }

let refresh_fvars env =
  Vset.fold (fun v acc -> Vset.union (fvars (Tvar v)) acc) env.fvars Vset.empty

let add name typ env =
  let env_fvars = refresh_fvars env in
  let bindings = Smap.add name { vars = Vset.empty; typ } env.bindings in
  { bindings; fvars = Vset.union env_fvars (fvars typ) }

let add_gen name typ env =
  let env_fvars = refresh_fvars env in
  let vars = Vset.diff (fvars typ) env_fvars in
  let bindings = Smap.add name { vars; typ } env.bindings in
  { bindings; fvars = env_fvars }

let find name env =
  let schema = Smap.find name env.bindings in
  let subs = ref Vmap.empty in
  let rec inst t =
    match head t with
    | Tint -> Tint
    | Tarrow (t1, t2) -> Tarrow (inst t1, inst t2)
    | Tproduct (t1, t2) -> Tproduct (inst t1, inst t2)
    | Tvar v -> (
        match Vset.mem v schema.vars with
        | false -> Tvar v
        | true -> (
            match Vmap.find_opt v !subs with
            | Some v' -> Tvar v'
            | None ->
                let v' = V.create () in
                subs := Vmap.add v v' !subs;
                Tvar v'))
  in
  inst schema.typ

type expression =
  | Var of string
  | Const of int
  | Op of string
  | Fun of string * expression
  | App of expression * expression
  | Pair of expression * expression
  | Let of string * expression * expression

let rec w env = function
  | Var x -> find x env
  | Const _ -> Tint
  | Op "+" -> Tarrow (Tproduct (Tint, Tint), Tint)
  | Op op -> failwith ("Unknown operator: " ^ op)
  | Fun (x, e) ->
      let tv = Tvar (V.create ()) in
      let env' = add x tv env in
      Tarrow (tv, w env' e)
  | App (e1, e2) ->
      let t1 = w env e1 in
      let t2 = w env e2 in
      let tv = Tvar (V.create ()) in
      unify t1 (Tarrow (t2, tv));
      tv
  | Pair (e1, e2) -> Tproduct (w env e1, w env e2)
  | Let (x, e1, e2) ->
      let t1 = w env e1 in
      let env' = add_gen x t1 env in
      w env' e2

let typeof e = canon (w empty e)

let () =
  let a = V.create () in
  let b = V.create () in
  let ta = Tvar a in
  let tb = Tvar b in
  assert (head ta == ta);
  assert (head tb == tb);
  let ty = Tarrow (ta, tb) in
  a.def <- Some tb;
  assert (head ta == tb);
  assert (head tb == tb);
  b.def <- Some Tint;
  assert (head ta = Tint);
  assert (head tb = Tint);
  assert (canon ta = Tint);
  assert (canon tb = Tint);
  assert (canon ty = Tarrow (Tint, Tint))

let () =
  let a = V.create () in
  let b = V.create () in
  let ta = Tvar a in
  let tb = Tvar b in
  assert (occur a ta);
  assert (occur b tb);
  assert (not (occur a tb));
  let ty = Tarrow (ta, tb) in
  assert (occur a ty);
  assert (occur b ty);
  unify ty (Tarrow (Tint, Tint));
  assert (canon ta = Tint);
  assert (canon ty = Tarrow (Tint, Tint));
  let c = V.create () in
  let tc = Tvar c in
  unify tc ty;
  assert (canon tc = Tarrow (Tint, Tint))

let cant_unify ty1 ty2 =
  try
    unify ty1 ty2;
    false
  with
  | UnificationFailure _ -> true

let () =
  assert (cant_unify Tint (Tarrow (Tint, Tint)));
  assert (cant_unify Tint (Tproduct (Tint, Tint)));
  let a = V.create () in
  let ta = Tvar a in
  unify ta (Tarrow (Tint, Tint));
  assert (cant_unify ta Tint)

let () =
  assert (Vset.is_empty (fvars (Tarrow (Tint, Tint))));
  let a = V.create () in
  let ta = Tvar a in
  let ty = Tarrow (ta, ta) in
  assert (Vset.equal (fvars ty) (Vset.singleton a));
  unify ty (Tarrow (Tint, Tint));
  assert (Vset.is_empty (fvars ty))

let () = assert (typeof (Const 1) = Tint)

let () =
  assert (
    match typeof (Fun ("x", Var "x")) with
    | Tarrow (Tvar v1, Tvar v2) -> V.equal v1 v2
    | _ -> false)

let () =
  assert (
    typeof (Fun ("x", App (Op "+", Pair (Var "x", Const 1))))
    = Tarrow (Tint, Tint))

let () =
  assert (
    typeof (Fun ("x", App (Op "+", Pair (Var "x", Var "x"))))
    = Tarrow (Tint, Tint))

let () =
  assert (
    typeof (Let ("x", Const 1, App (Op "+", Pair (Var "x", Var "x"))))
    = Tint)

let () =
  assert (
    typeof (Let ("id", Fun ("x", Var "x"), App (Var "id", Const 1)))
    = Tint)

let () =
  assert (
    typeof
      (Let
         ( "id",
           Fun ("x", Var "x"),
           App (App (Var "id", Var "id"), Const 1) ))
    = Tint)

let () =
  assert (
    typeof
      (Let
         ( "id",
           Fun ("x", Var "x"),
           Pair
             ( App (Var "id", Const 1),
               App (Var "id", Pair (Const 1, Const 2)) ) ))
    = Tproduct (Tint, Tproduct (Tint, Tint)))

let () =
  let ty =
    typeof
      (Fun ("f", Fun ("x", Let ("y", App (Var "f", Var "x"), Var "y"))))
  in
  assert (
    match ty with
    | Tarrow (Tarrow (Tvar v1, Tvar v2), Tarrow (Tvar v3, Tvar v4)) ->
        V.equal v1 v3 && V.equal v2 v4
    | _ -> false)

let cant_type e =
  try
    let _ = typeof e in
    false
  with
  | UnificationFailure _ -> true

let () = assert (cant_type (App (Const 1, Const 2)))
let () = assert (cant_type (Fun ("x", App (Var "x", Var "x"))))

let () =
  assert
    (cant_type
       (App (Fun ("f", App (Op "+", App (Var "f", Const 1))), Fun ("x", Var "x"))))

let () =
  assert
    (cant_type
       (Fun
          ( "x",
            Pair (App (Var "x", Const 1), App (Var "x", Pair (Const 1, Const 2)))
          )))

let () =
  assert
    (cant_type
       (Fun
          ( "x",
            Let
              ( "z",
                Var "x",
                Pair
                  (App (Var "z", Const 1), App (Var "z", Pair (Const 1, Const 2)))
              ) )))

let () =
  assert
    (cant_type
       (Let
          ( "distr_pair",
            Fun
              ( "f",
                Pair
                  ( App (Var "f", Const 1),
                    App (Var "f", Pair (Const 1, Const 2)) ) ),
            App (Var "distr_pair", Fun ("x", Var "x")) )))
