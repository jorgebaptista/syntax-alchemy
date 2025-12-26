# TD 5 - Análise descendente

Nesta trabalho dirigido, construiremos um analisador LL(1). Damo-nos para tal os tipos OCaml seguintes para definir a noção de gramática.

```ocaml
type terminal = string

type non_terminal = string

type symbol =
  | Terminal of terminal
  | NonTerminal of non_terminal

type production = symbol list

type rule = non_terminal * production

type grammar = {
  start : non_terminal;
  rules : rule list;
}
```

No que se segue, admitiremos que todos os não terminais são acessíveis e produtivos. Admitiremos igualmente que o símbolo de partida *S'* está associado a uma única regra da forma *S' -> S#* onde # é um símbolo terminal que não aparece em mais nenhuma regra da gramática. Assim, o exemplo exposto nas aulas teóricas

```
     E  ->  T E'
     E' ->  + T E'
         |  epsilon
     T  ->  F T'
     T' ->  * F T'
         |  epsilon
     F  -> ( E )
         | int
```

fica codificado pelo valor OCaml :

```ocaml
let g_arith =
  { start = "S'";
    rules = [ "S'", [ NonTerminal "E"; Terminal "#" ];
              "E",  [ NonTerminal "T"; NonTerminal "E'"; ];
              "E'", [ Terminal "+"; NonTerminal "T"; NonTerminal "E'"; ];
              "E'", [ ];
              "T",  [ NonTerminal "F"; NonTerminal "T'"; ];
              "T'", [ Terminal "*"; NonTerminal "F"; NonTerminal "T'"; ];
              "T'", [ ];
              "F",  [ Terminal "("; NonTerminal "E"; Terminal ")"; ];
              "F",  [ Terminal "int" ]; ] }
```

### Cálculo de ponto fixo

Para facilitar o cálculo de pontos fixos nas questões seguintes, definir uma função

```ocaml
val fixpoint : ('a -> 'a * bool) -> 'a -> 'a
```

tal que `fixpoint f x` itera a função `f` partindo do valor `x` enquanto o booleano devolvido por `f` valer `true`.

### Cálculo dos Null

Damo-nos o módulo OCaml seguinte para representar os conjuntos de não-terminais :

```ocaml
module Ntset = Set.Make(String)
type nulls = Ntset.t
```

Definir uma função `is_null_production` que, dado o conjunto dos não-terminais que podem derivar a palavra vazia, determina se uma palavra pode derivar-se na palavra vazia.

```ocaml
val is_null_production : nulls -> symbol list -> bool
```

(É a função NULL(α) dada na aula.)
A função `List.for_all` poderá, caso oportuno, ser utilizada.

Deduzir a definição de uma função `null : grammar -> nulls` que calcula o conjunto dos não-terminais de uma dada gramática. Poderá utilizar-se a função `fixpoint` da forma seguinte :

```ocaml
let null g =
  let step nulls =
    ...calculamos o novo conjunto de nulls e
       indica-se por um booleano se houve mudanças...
  in
  fixpoint step Ntset.empty
```

Nota : é, claro, possível utilizar `Ntset.equal` para determinar se o conjunto `nulls` mudou, mas é igualmente possível ser mais preciso usando `is_null_production` somente sobre as regras cujo membro esquerdo ainda não está em `nulls` e detectando neste caso se `nulls` precisa ser modificado.

### Cálculo dos primeiros

Definimos os módulos OCaml seguintes para representar o conjunto de terminais (`Tset`) et dos dicionários indexados por não-terminais (`Ntmap`) :

```ocaml
module Ntmap = Map.Make(String)
module Tset = Set.Make(String)
```

O conjuntos dos primeiros que vamos agora calcular nesta questão terão consequentemente os tipos seguintes :

```ocaml
type firsts = Tset.t Ntmap.t
```

ou seja, un dicionário que associa a cada símbolo não-terminal um conjunto de símbolos terminais.

Definir uma função `val empty_map : grammar -> Tset.t Ntmap.t `que constrói um dicionário que associa a cada não-terminalda gramática um conjunto vazio de terminais.

Definir uma função

```ocaml
val first_production_step : nulls -> firsts -> symbol list -> Tset.t
```

que calcula o conjunto dos primeiros de uma produção, dados o conjunto dos não-terminais nulos e um dicionário para os primeiros de cada não-terminal. (É a função FIRST(α) dada na aula.)

Deduzir do esforço prévio uma função `val first : grammar -> nulls -> firsts` que calcula o conjunto dos primeiros dos não-terminais de uma gramática, dado o conjunto dos não terminais nulos desta gramática (este é passado em parâmetro para evitar ser recalculado). Para este efeito, poder-se-á usar a função `fixpoint` e determinar se o conjunto dos primeiros é modificado, utilizando a função `Tset.subset` que calcula o predicado de conjuntos.

### Cálculos dos seguintes

Vamos agora calcular o conjunto dos seguintes da gramática. O conjunto é representado pelo mesmo tipo OCaml que o dos primeiros :

```ocaml
type follows = Tset.t Ntmap.t
```

Definir uma função

```ocaml
val follow : grammar -> nulls -> firsts -> follows
```

que calcula os seguintes de uma gramática, dados os seus não-terminais nulos e os seus primeiros. Poderemos adotar o esquema seguinte :

```ocaml
let follow g nulls firsts =
  let update (follows,b) nt follow_nt =
     ...
     ... actualizar a tabela follows com nt -> follow_nt
     ... e substituir b por true se a tabela foi alterada
     ...
  in
  let rec update_prod ((follows,b) as acc) nt p =
     ...
     ... examinar a produção nt -> p da gramática
     ... e actualizar o par (follows,b) para todo o não-terminal X de p
     ...
  in
  let step follows =
    List.fold_left
      (fun acc (nt,p) -> update_prod acc nt p)
      (follows,false) g.rules
  in
  fixpoint step (empty_map g)
```

### Construção da tabela de análise LL(1)

Considera-se os tipos OCaml seguintes para representar os dicionários indexados pelos terminais e os conjuntos de produções :

```ocaml
module Tmap = Map.Make(String)
module Pset = Set.Make(struct type t = production let compare = compare end)
```

Define-se então o tipo seguinte para as tabelas de análises descendentes :

```ocaml
type expansion_table = Pset.t Tmap.t Ntmap.t
```

Numa tabela destas é um dicionário associando a cada não-terminal e também a cada terminal um conjunto de produções. Os dois dicionários são ocos : quando uma linha ou uma coluna da tabela encontra-se vazia, não há entrada correspondente na tabela.

Definir uma função

```ocaml
val add_entry : expansion_table -> non_terminal -> terminal -> production -> expansion_table
```

que junta uma entrada na tabela. Dever-se-á ter em atenção o correcto tratamento do caso em que uma primeira ocorrência de uma linha ou de uma coluna.

Definir uma função

```
val expansions : grammar -> expansion_table
```

que calcula uma tabela de análise descendente de uma dada gramática.

O código produzido poderá ser testado com a gramática seguinte (que caracteriza as palavras que contêm tantos `a` quantos `b`) :

```ocaml
let g1 = {
  start = "S'";
  rules = ["S'", [NonTerminal "S"; Terminal "#"];
	   "S", [];
	   "S", [Terminal "a"; NonTerminal "A"; NonTerminal "S"];
	   "S", [Terminal "b"; NonTerminal "B"; NonTerminal "S"];
	   "A", [Terminal "a"; NonTerminal "A"; NonTerminal "A"];
	   "A", [Terminal "b"];
	   "B", [Terminal "b"; NonTerminal "B"; NonTerminal "B"];
	   "B", [Terminal "a"];
	  ] }

let table1 = expansions g1
```

Para visualizar o resultado, poderemos utilizar o código seguinte que define um 

pretty-printer  `pp_table` para as tabelas de expansão :

```ocaml
let pp_symbol fmt = function
  | Terminal s -> Format.fprintf fmt "\"%s\"" s
  | NonTerminal s -> Format.fprintf fmt "%s" s

let rec pp_production fmt = function
  | [] -> ()
  | [x] -> pp_symbol fmt x
  | x :: l -> Format.fprintf fmt "%a %a" pp_symbol x pp_production l

let pp_table fmt t =
  let print_entry c p =
    Format.fprintf fmt "  %s: @[%a@]@\n" c pp_production p in
  let print_row nt m =
       Format.fprintf fmt "@[Expansions for %s:@\n" nt;
       Tmap.iter (fun c rs -> Pset.iter (print_entry c) rs) m;
       Format.fprintf fmt "@]" in
  Ntmap.iter print_row t
```

Usando o exemplo acima introduzido, o resultado de

```ocaml
  let () = Format.printf "%a@." pp_table table1
```

deve ser o seguinte :

```
Expansions for A:
  a: "a" A A
  b: "b"
Expansions for B:
  a: "a"
  b: "b" B B
Expansions for S:
  #:
  a: "a" A S
  b: "b" B S
Expansions for S':
  #: S "#"
  a: S "#"
  b: S "#"
```

Testar igualmente com a gramática `g_arith` dada no início deste enunciado.

```ocaml
let table_arith = expansions g_arith
let () = Format.printf "%a@." pp_table table_arith
```

### Caracterização LL(1)

Definir uma função `is_ll1 : expansion_table -> bool` que determina se a tabela de expansão contém no máximo uma regra por célula (o que, por definição, caracteriza que a dita gramática pertença à classe LL(1)).

Testar com

```ocaml
let () = assert (is_ll1 table1)
let () = assert (is_ll1 table_arith)
```



Testar igualmente com uma gramática da sua escolha que não seja LL(1).

### Reconhecimento de uma palavra

Definir uma função

```ocaml
val analyze : non_terminal -> expansion_table -> string list -> bool
```

que determina se uma palavra é reconhecida por uma gramática, dados o não-terminal inicial e a sua tabela de expansão. (Não nos preocuparemos aqui em saber se a tabela corresponde a uma gramática LL(1) e, em caso de ambiguidade escolhemos uma produção ao acaso com recurso à função `Pset.choose`.) Não esquecer juntar o símbolo `"#"` no fim da palavra.
Nota : a análise termina com sucesso se e só se esta resulta numa pilha e numa entrada ambas iguais à lista vazia `[]`, visto que acrescentamos uma regra *S' -> S#*.

Poderemos testar com a gramática `g1` utilizando o código seguinte

```ocaml
let explode s =
  let n = String.length s in
  let rec make i = if i = n then [] else String.make 1 s.[i] :: make (i+1) in
  make 0

let test1 s = analyze g1.start (expansions g1) (explode s)

let () = assert (test1 "")
let () = assert (test1 "ab")
let () = assert (test1 "ba")
let () = assert (test1 "abab")
let () = assert (test1 "aaabbb")
let () = assert (test1 "aaabababbbababab")

let () = assert (not (test1 "a"))
let () = assert (not (test1 "b"))
let () = assert (not (test1 "aab"))
let () = assert (not (test1 "aaabbba"))
let () = assert (not (test1 "aaabbbaabab"))
let () = assert (not (test1 "aaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbb"))
```

### Bootstrap : a gramática das gramáticas

Considera-se a gramática seguinte :

```ocaml
let g_gram =
  { start = "S'";
    rules = [ "S'", [ NonTerminal "S"; Terminal "#" ];
              "S",  [ NonTerminal "R" ];
              "S",  [ NonTerminal "R"; Terminal ";"; NonTerminal "S" ];
              "R",  [ Terminal "ident"; Terminal "::="; NonTerminal "P"];
              "P",  [ NonTerminal "W" ];
              "P",  [ NonTerminal "W"; Terminal "|"; NonTerminal "P" ];
              "W",  [ ];
              "W",  [ NonTerminal "C"; NonTerminal "W";];
              "C",  [ Terminal "ident"];
              "C",  [ Terminal "string"];
            ] }
```

Esta é a gramática das gramáticas, em que os diferentes não-terminais significam :

- `S` = sequência de regras separadas por pontos-e-vírgulas ;
- `R` = regras para um não-terminal ;
- `P` = produções separados por barras verticais ;
- `W` = membro direitos de uma produção ;
- `C` = símbolo terminal ou não-terminal.

Verificar que esta gramática não é LL(1) :

```ocaml
let table_gram = expansions g_gram
let () = Format.printf "%a@." pp_table table_gram
let () = assert (not (is_ll1 table_gram))
```

Propor uma gramática LL(1) que reconhece esta linguagem.

[solução](./TOBEDEFINED)