# TD 3 - Algoritmo W

O objectivo desta Prática Laboratorial é a programação do algoritmo W para mini-ML. Vamos aqui considerar uma versão com unificão destrutiva.

#### Tipos e variáveis de tipo

Comecemos por introduzir a sintaxe abstracta dos tipos.

```ocaml
type typ =
  | Tint
  | Tarrow of typ * typ
  | Tproduct of typ * typ
  | Tvar of tvar

and tvar =
  { id : int;
    mutable def : typ option }
```

O tipo `typ` dos tipos de mini-ML é mutualmente recursivo com o tipo `tvar` das variáveis de tipo. De facto, uma variável de tipo contém uma eventual definição (obtida aquando de uma unificação), que é um tipo. Assim `{ id = 1; def = None }` "ainda" é uma variável de tipo, mas `{ id = 2; def = Some Tint }` é uma antiga variável de tipo que foi definida como sendo igual ao tipo `Tint` e que deve ser doravante tratada como se fosse o tipo `Tint`.

Para testar, poderemos utilizar a função de visualização seguinte: 

```ocaml
let rec pp_typ fmt = function
  | Tproduct (t1, t2) -> Format.fprintf fmt "%a *@ %a" pp_atom t1 pp_atom t2
  | Tarrow (t1, t2) -> Format.fprintf fmt "%a ->@ %a" pp_atom t1 pp_typ t2
  | (Tint | Tvar _) as t -> pp_atom fmt t
and pp_atom fmt = function
  | Tint -> Format.fprintf fmt "int"
  | Tvar v -> pp_tvar fmt v
  | Tarrow _ | Tproduct _ as t -> Format.fprintf fmt "@[<1>(%a)@]" pp_typ t
and pp_tvar fmt = function
  | { def = None; id } -> Format.fprintf fmt "'%d" id
  | { def = Some t; id } -> Format.fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t
```



Introduzimos em seguida um módulo `V` que encapsula o tipo `tvar` com uma função de comparação e uma função que cria uma nova variável de tipo.

```ocaml
module V = struct
  type t = tvar
  let compare v1 v2 = Pervasives.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end
```

#### Questão 1. Normalização

Para facilitar a gestão das variáveis de tipo, vamos introduzir duas funções que normalizam uma expressão de tipo seguindo as definições contidas nas variáveis de tipo.

Escrever uma função

```ocaml
  val head : typ -> typ
```

que normaliza um tipo em cabeça, *i.e.*, `head t` retorna um tipo igual a `t` que não é da forma `Tvar { def = Some _}`. Dito de outra forma, `head t` segue as definições de variável de tipo em cabeça `t`, enquanto os houver.

Escrever uma função

```ocaml
  val canon : typ -> typ
```

que normaliza um tipo integralmente, *i.e.*, que aplica a função `head` acima definida, em profundidade. Esta segunda função será utilizada somente para a visualização dos tipos (no resultados de uma operação de tipagem ou numa mensagem de erro).

Poderemos testar com

```ocaml
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
```

#### Questão 2. Unificação

Para assinalar os erros de unificação, damo-nos a excepção e a função seguinte :

```ocaml
exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))
```

Escrever uma função

```ocaml
  val occur : tvar -> typ -> bool
```

que testa a ocorrência de uma variável de tipo num tipo (*occur-check*). Poderemos supor que a variável não está definida. No entanto, o tipo pode conter variáveis definidas e convém aplicar-lhe a função `head` antes de a examinar.

Escrever uma função

```ocaml
  val unify : typ -> typ -> unit
```

realizando a unificação de dois tipos. Aqui também convém utilizar a função `head` sobre os tipos passados em argumento antes de os examinar.

Poderemos testar com

```ocaml
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
  (* unifica 'a-> 'b e int->int *)
  unify ty (Tarrow (Tint, Tint));
  assert (canon ta = Tint);
  assert (canon ty = Tarrow (Tint, Tint));
  (* unifica 'c e int->int *)
  let c = V.create () in
  let tc = Tvar c in
  unify tc ty;
  assert (canon tc = Tarrow (Tint, Tint))

let cant_unify ty1 ty2 =
  try let _ = unify ty1 ty2 in false with UnificationFailure _ -> true

let () =
  assert (cant_unify Tint (Tarrow (Tint, Tint)));
  assert (cant_unify Tint (Tproduct (Tint, Tint)));
  let a = V.create () in
  let ta = Tvar a in
  unify ta (Tarrow (Tint, Tint));
  assert (cant_unify ta Tint)
```

#### Questão 3. Variáveis livres de um tipo

Para representar o conjunto das variáveis livres de um tipo, utilizamos o modulo `Set` de OCaml.

```ocaml
module Vset = Set.Make(V)
```

Escrever uma função

```ocaml
  val fvars : typ -> Vset.t
```

que calcula um conjunto das variáveis livres de um tipo. Uma vez ainda, convém utilizar a função `head` com o devido proveito para só considerar as variáveis que ainda não estão definidas.

Poderemos testar com

```ocaml
let () =
  assert (Vset.is_empty (fvars (Tarrow (Tint, Tint))));
  let a = V.create () in
  let ta = Tvar a in
  let ty = Tarrow (ta, ta) in
  assert (Vset.equal (fvars ty) (Vset.singleton a));
  unify ty (Tarrow (Tint, Tint));
  assert (Vset.is_empty (fvars ty))
```

#### Questão 4. Ambientes de tipagem

Definimos o tipo OCaml para os esquemas de tipos :

```ocaml
type schema = { vars : Vset.t; typ : typ }
```

Introduzimos então dicionários utilizando cadeias de caracteres como chaves

```ocaml
module Smap = Map.Make(String)
```

e o tipo OCaml seguinte para os ambientes de tipagem :

```ocaml
type env = { bindings : schema Smap.t; fvars : Vset.t }
```

O primeiro campo, `bindings`, contém os elementos do ambiente de tipagem. O segundo, `fvars`, contém o conjunto de todas as variáveis livres nas entradas de `bindings`. O campo `fvars` só está aqui para evitar recalcular de cada vez o conjunto das variáveis livres do ambiente aquando da operação de generalização. Nota : certas destas variáveis podem ficar definidas entre o momento onde se encontram neste conjunto e o momento onde este utilizado. Será conveniente actualizar este conjunto antes de se servir dele (por exemplo aplicando `fvars (Tvar v)` a cada variável `v` e calculando a união dos resultados).

Definir o ambiente de tipagem vazio :

```ocaml
  val empty : env
```

escrever a função

```ocaml
  val add : string -> typ -> env -> env
```

que junta um elemento dentro do ambiente de tipagem sem generalização (este será utilizado para a tipagem de uma função). Não esqueceremos a actualização do campo `fvars` do ambiente.

Escrever a função

```ocaml
  val add_gen : string -> typ -> env -> env
```

que junta um elemento no ambiente de tipagem generalizando o seu tipo relativamente a todas as variáveis de tipo livres não aparecendo no ambiente (esta será utilizada na tipagem de um `let`).

Escrever a função

```ocaml
  val find : string -> env -> typ
```

que retorna o tipo associado a um identificador num ambiente, depois de ter substituido todas as variáveis do esquema correspondente por variáveis de tipos frescas. Cuidado : pode haver várias ocorrências da mesma variável no tipo e convém, claro, substitui-las por uma mesma variável fresca. 

**Atenção:** pode haver várias ocorrências da mesma variável no tipo, e é necessário, naturalmente, substituí-las todas pela **mesma variável fresca**.
 **Indicação:** Pode introduzir um módulo `module Vmap = Map.Make(V)` e utilizar um valor do tipo `tvar Vmap.t` para representar essa substituição.

#### Questão 5. Algoritmo W

Damo-nos o tipo seguinte para as expressões de mini-ML :

```ocaml
type expression =
  | Var of string
  | Const of int
  | Op of string
  | Fun of string * expression
  | App of expression * expression
  | Pair of expression * expression
  | Let of string * expression * expression
```

Escrever a função

```ocaml
  val w : env -> expression -> typ
```

que implementa o algoritmo W. Para os operadores (`Op`), pode-se ficar apenas com `Op "+"` do tipo `int * int -> int`.

Para testar, utilizaremos a função seguinte

```ocaml
let typeof e = canon (w empty e)
```

Alguns testes positivos (a expressão e o tipo esperado estão indicados nos comentários) :

```ocaml
(* 1 : int *)
let () = assert (typeof (Const 1) = Tint)

(* fun x -> x : 'a -> 'a *)
let () = assert (match typeof (Fun ("x", Var "x")) with
  | Tarrow (Tvar v1, Tvar v2) -> V.equal v1 v2
  | _ -> false)

(* fun x -> x+1 : int -> int *)
let () = assert (typeof (Fun ("x", App (Op "+", Pair (Var "x", Const 1))))
                 = Tarrow (Tint, Tint))

(* fun x -> x+x : int -> int *)
let () = assert (typeof (Fun ("x", App (Op "+", Pair (Var "x", Var "x"))))
                 = Tarrow (Tint, Tint))

(* let x = 1 in x+x : int *)
let () =
  assert (typeof (Let ("x", Const 1, App (Op "+", Pair (Var "x", Var "x"))))
          = Tint)

(* let id = fun x -> x in id 1 *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"), App (Var "id", Const 1)))
          = Tint)

(* let id = fun x -> x in id id 1 *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"),
		       App (App (Var "id", Var "id"), Const 1)))
          = Tint)

(* let id = fun x -> x in (id 1, id (1,2)) : int * (int * int) *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"),
		       Pair (App (Var "id", Const 1),
			     App (Var "id", Pair (Const 1, Const 2)))))
          = Tproduct (Tint, Tproduct (Tint, Tint)))

(* app = fun f x -> let y = f x in y : ('a -> 'b) -> 'a -> 'b *)
let () =
  let ty =
    typeof (Fun ("f", Fun ("x", Let ("y", App (Var "f", Var "x"), Var "y"))))
  in
  assert (match ty with
    | Tarrow (Tarrow (Tvar v1, Tvar v2), Tarrow (Tvar v3, Tvar v4)) ->
        V.equal v1 v3 && V.equal v2 v4
    | _ -> false)
```

Alguns testes negativos :

```ocaml
let cant_type e =
  try let _ = typeof e in false with UnificationFailure _ -> true

(* 1 2 *)
let () = assert (cant_type (App (Const 1, Const 2)))

(* fun x -> x x *)
let () = assert (cant_type (Fun ("x", App (Var "x", Var "x"))))

(* (fun f -> +(f 1)) (fun x -> x) *)
let () = assert (cant_type
                   (App (Fun ("f", App (Op "+", App (Var "f", Const 1))),
	                 Fun ("x", Var "x"))))

(* fun x -> (x 1, x (1,2)) *)
let () = assert (cant_type
                   (Fun ("x", Pair (App (Var "x", Const 1),
			            App (Var "x", Pair (Const 1, Const 2))))))

(* fun x -> let z = x in (z 1, z (1,2)) *)
let () = assert (cant_type
                   (Fun ("x",
		         Let ("z", Var "x",
			      Pair (App (Var "z", Const 1),
			            App (Var "z", Pair (Const 1, Const 2)))))))

(* let distr_pair = fun f -> (f 1, f (1,2)) in distr_pair (fun x -> x) *)
let () =
  assert (cant_type
            (Let ("distr_pair",
		  Fun ("f", Pair (App (Var "f", Const 1),
				  App (Var "f", Pair (Const 1, Const 2)))),
		  App (Var "distr_pair", (Fun ("x", Var "x"))))))
```

[- solução -](POR FORNECER)

