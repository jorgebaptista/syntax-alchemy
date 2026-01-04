# TD 2 - Interpretador de mini-Python

O objectivo desta Prática Laboratorial é de implementar um interpretador para um fragmento muito simples de Python, designado de **mini-Python** ; não é pre-requisito conhecer a linguagem Python.

Para ajudar-vos nesta tarefa, fornecemos a sua estrutura de base (na forma de um conjunto de ficheiros OCaml e um Makefile) que podem recuperar aqui : [mini-python.zip](./mini-python.zip). Uma vez descompactado, este arquivo produz uma pasta `mini-python/` com os ficheiros seguintes :

| ast.mli    | a sintaxe abstracta de **mini-Python** (completo) |
| ---------- | ------------------------------------------------- |
| lexer.mll  | o analizador léxico (completo)                    |
| parser.mly | o analizador sintáctico (completo)                |
| interp.ml  | o interpretador **(por completar)**               |
| main.ml    | o programa principal (completo)                   |
| Makefile   | para automatizar a compilação (completo)          |

Como para o TD anterior, o código fornecido compila (basta digitar `make`, que executa `dune build`), mas está incompleto. Deverá completar o ficheiro `interp.ml`.

O executável aplica-se a um ficheiro mini-Python com o sufixo `.py`.
 Ao executar `make`, o programa é lançado automaticamente sobre o ficheiro `test.py`.

 O executável aplica-se sobre um ficheiro **mini-Python** com a extensão `.py`, assim :

```
  ./mini-python fich.py
```

Um ficheiro **mini-Python** tem a estrutura seguinte :

```python
# zero, uma ou várias definições de funções no início do ficheiro
def fibaux(a, b, k):
    if k == 0:
        return a
    else:
        return fibaux(b, a+b, k-1)

def fib(n):
    return fibaux(0, 1, n)

# uma ou várias instruções no fim do ficheiro
print "alguns valores da sequência de Fibonacci :"
for n in [0, 1, 11, 42]:
    print(fib(n))
```

Mais geralmente, um ficheiro **mini-Python** é composto de uma lista opcional de declarações de funções, seguido de uma lista de instruções. As instruções são : a atribuição, a condicional, o ciclo (`for`), a visualização de uma expressão com `print`, a devolução de um valor com `return` ou a avaliação de uma expressão. As expressões inteiras são : uma constante (booleana, inteira ou uma string), o acesso a uma variável, a construção de uma lista (com a notação `[e1, e2, ..., en]`), o acesso a um elemento de lista (com a notação `e[i]`), a chamada de uma função, ou uma das operações `+`, `-`, `*` e `//`, `=`, `<>`, `<`, `<=`, `>`, `>=`, `and`, `or` e `not`.

Consideramos igualmente as três funções primitivas: `list(range(n))` constrói a lista `[0, 1, 2, ..., n-1]` e `len(l)` devolve o comprimento da lista `l`. (Apenas usaremos `list` e `range` em conjunto desta forma.)

#### Questão 1. Expressões aritméticas

Consideramos somente por enquanto as expressões aritméticas que não contêm variáveis. Completar a função `expr` para interpretar estas expressões. (Ignoraremos por enquanto o primeiro argumento `ctx` da função `expr`.) Testar com o programa seguinte :

```python
print 1 + 2*3
print (3*3 + 4*4) // 5
print 10-3-4
```

o resultado deve ser

```structured text
7
5
3
```

As operações de divisão e de módulo devem assinalar um erro no caso de divisão por zero. Utilizarmos para esse efeito a função `error` fornecida no ficheiro `interp.ml`.

Para testar facilmente, podemos editar o ficheiro `test.py` e invocar o comando `make`. Este comando compila o interpretador `mini-python` e executa este último sobre o ficheiro `test.py`.

#### Questão 2. Expressões booleanas e condicionais

Completar as funções `is_true` e `is_false`, que determinam respectivamente se um valor é verdadeiro ou falso. Em Python, o valor `None`, o booleano `False`, o inteiro `0`, a string vazia `""` e a lista vazia `[]` são considerados como falsos e os outros valores são considerados como verdadeiros.

Complete em seguida a função `expr` para interpretar as constantes booleanas, as operações de comparação e as operações `and`, `or` e `not`. Em Python, a comparação é estrutural ; poderemos utilizar directamente a comparação estrutural de OCaml, isto é utilizar operações como `<` sobre valores de tipo `value`. (Tal procedimento não é 100% compatível com Python, mas remediaremos esta situação mais adiante.)

Completar finalmente a função `stmt` para poder interpretar a condicional (construção `Sif`).

Testar com o programa seguinte :

```python
print not True and 1//0==0
print 1<2
if False or True:
    print "ok"
else:
    print "oops"
```

cujo resultado deve ser

```python
False
True
ok
```

#### Questão 3. Variáveis

Para manipular as variáveis (do programa principal mas também das variáveis locais e parâmetros) vamos utilizar um *ambiente* (*environment*), ou seja aqui, uma tabela de hash passada às funções `expr` e `stmt` na forma de um argumento `ctx`. Esta tabela associa a cada variável o seu valor. Esta tabela de associação é implementada com recurso ao módulo `Hashtbl` de OCaml e tem assim o tipo:

```ocaml
  (string, value) Hashtbl.t
```

Completar a função `expr` para que se possa aceder às variáveis. É o caso do padrão (*pattern matching*) `Eident id`. Tentar aceder a uma variável que não se encontra ainda na tabela deve provocar um erro. Nesta mesma linha, completar a função `stmt` para que se possa atribuir uma variável. É o caso do padrão `Sassign (id, e1)`. Desta vez a variável pode estar ou não na tabela. No caso de la estar, o seu valor é modificado.

Finalmente, completar a função `expr` para que se possa concatenar duas strings com a operação `+`.

Testar com o programa seguinte :

```python
x = 41
x = x+1
print(x)
b = True and False
print(b)
s = "hello" + " world!"
print(s)
```

o resultado deve ser

```structured text
42
False
hello world!
```

#### Questão 4. Funções

Vamos juntar o processamento das funções. estas últimas são arquivadas na tabela global declarada da seguinte forma :

```ocaml
let functions = (Hashtbl.create 17 : (string, ident list * stmt) Hashtbl.t)
```

A cada nome de função está associada um par constituído da lista dos parâmetro e da instrução que define o corpo da função. Completar a função `file` para que esta preencha esta tabela com as funções contidas na lista `fl`.

Completar na sequência as funções `expr` e `stmt` para interpretar uma chamada de função. Para uma chamada da forma `f(e1,...,en)` a uma função `f` da forma `def f(x1,...,xn): body` deve-se construir um *novo* ambiente que associe a cada argumento formal `xi` o valor de `ei`. Podemos então interpretar a instrução `body` (o corpo da função) neste novo ambiente. A instrução `return` sera interpretada utilizando a excepção OCaml `Return` (já definida). Se a função terminar sem executar um `return`, o valor `None` é retornado.

Testar com o programa seguinte :

```python
def fact(n):
    if n <= 1: return 1
    return n * fact(n-1)

print fact(10)
```

cujo resultado deve ser

```structured text
3628800
```

#### Questão 5. Listas

Juntar finalmente o suporte para as listas. Para isso, completar a função `expr` para que se possa concatenar duas listas com a operação `+`, para que se possa interpretar a chamada das funções primitivas `len` (comprimento de uma lista) e `list(range(n))` (lista `[0, 1, 2, ..., n-1]`), e, finalmente, para que se possa interpretar as construções `[e1, e2, ..., en]` e `e1[e2]`.

Completar em sequência a função `stmt` para poder interpretar a atribuição de um elemento de lista (caso do padrão `Sset (e1, e2, e3)`).

Finalmente, completar a função `stmt` para poder interpretar a construção `for`. A construção `Sfor(x,e,s)` atribuí a variável `x` sucessivamente aos diferentes elementos da lista `e` e executa de cada vez a instrução `s`. A expressão `e` deve ser avaliada uma só vez.

Testar sobre o programa dado no início do enunciado. o resultado deve ser:

```structured text
0
1
89
267914296
```

#### Questão 6. Outros testes

Testes positivos e negativos são disponibilizados. Para executar o vosso interpretador com estes testes basta invocar `make tests`.

#### Questão 7 (bónus). Comparação Estrutural

Nas listas, a comparação estrutural de Python não é exactamente a mesma que a da igualdade de OCaml sobre os valores de tipo `value array`. De facto, OCaml compara começando por averiguar o comprimentos dos vectores e só depois os elementos. Assim, OCaml declara que `[|0;1;1|]` é maior que `[|1|]`, enquanto Python declara que `[0,1,1]` é menor que `[1]` porque implementa uma ordem lexicográfca sobre as listas.

Escrever uma função `compare_value: value -> value -> int` para comparar dois valores Python. Esta função devolve um inteiro estritamente negativo (resp. nulo, resp. estritamente positivo) quando o primeiro valor é menor (resp. igual, resp. maior) que o segundo. Poder-se-á recorrer ao interpretador de Python como referência em caso de dúvidas. Utilizar esta função para rectificar o que foi feito na questão 2.

Fazer testes suplementares para constatar a correção da solução proposta.

[solução -](./POR FORNECER)

------

