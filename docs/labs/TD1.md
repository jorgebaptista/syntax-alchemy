# TD 1 - Assembly X86-64 AT&T

Esta Prática Laboratorial introduz a produção de código Assembly x86-64 (AT&T). Caso necessário, poderão utilizar [Nemiver](https://wiki.gnome.org/Apps/Nemiver) para executar passo a passo código assembly, ou ainda `gdb.

Esta [página de Andrew Tolmach](http://web.cecs.pdx.edu/~apt/cs491/x86-64.html) agrupa um conjunto relevante de apontadores para escrever / depurar código assembly X86-64 e em particular as suas [notas sobre o assembly X86-64](./x86-64.pdf).

## 1. Pequenos exercícios sobre o assembly X86-64

Relembramos que um pequeno programa assembly escreve-se num ficheiro com extensão `.s` e tem a forma seguinte :

```
      .text
      .globl main
main:
      ...
      mov  $0, %rax       # código de saída
      ret
      .data
      ...
```

Pode compilar e executar este ficheiro de forma não-interactiva com o comando seguinte:

```
  gcc -g -no-pie ficheiro.s && ./a.out
```

#### a. Expressões aritméticas

Implementar pequenos programas para avaliar e vizualizar o resultado das expressões aritméticas seguintes :

- `4 + 6`
- `21 * 2`
- `4 + 7 / 2`
- `3 - 6 * (10 / 5)`

O resultado esperado é

```
10
42
7
-9
```

Para vizualizar um inteiro, poderão utilizar a função seguinte :

```
print_int:
        mov     %rdi, %rsi
        mov     $message, %rdi  # argumentos para printf
        mov     $0, %rax
        call    printf
        ret
        .data
message:
        .string "%d\n"
```

[solução com registos](COMING_SOON)

[solução com recurso à pilha](COMING _SOON)

#### b. Expressões Booleanas

Usando a convenção de que o inteiro `0` representa o valor booleano *falso* e que qualquer outro inteiro codifica o valor *verdade*, escrever os programas em assembly que avaliam e vizualizam o resultado das expressões seguintes (deve mostrar `true` ou `false` no caso de um valor booleano) :

- `true && false`
- `if 3 <> 4 then 10 * 2 else 14`
- `2 = 3 || 4 <= 2*3`

O resultado esperado é

```
false
20
true
```

Poderá ser útil escrever uma função `print_bool` para mostrar um boleano.

[solução](COMING_SOON)

#### c. Variáveis globais

Escrever um programa em assembly que avalia as três instruções seguintes :

```
  let x = 2
  let y = x * x
  print (y + x)
```

Alocar-se-á as variáveis `x` e `y` no mesmo segmento de dados. O resultado esperado é `6`.

[solução](COMING_SOON)

#### d. Variáveis locais

Escrever um programa em assembly que avalia o programa seguinte :

```
  print (let x = 3 in x * x)
  print (let x = 3 in (let y = x + x in x * y) + (let z = x + 3 in z / z))
```

Alocar-se-á as variáveis `x`, `y` e `z` na pilha. O resultado esperado é

```
9
19
```

[solução](COMING_SOON)



## 2. Compilação de uma mini linguagem

O objectivo deste exercício é a escrita de um pequeno compilador para uma mini-linguagem aritmética, designada de **Arith**, para a linguagem assembly X86-64. Um programa da linguagem **Arith** é composto por uma sequência de instruções que são, alternativamente, a introdução de uma variável global com a sintaxe

```
  set <ident> = <expr>
```

a vizualização do valor de uma expressão, com a sintaxe

```
  print <expr>
```

Aqui, `<ident>` designa o nome de uma variável e `<expr>` uma expressão aritmética. As expressões aritméticas podem ser construidas a partir de constantes inteiros, variáveis, da soma, da substracção, da multiplicação, da divisão, de parêntesis e de uma construção `let in` que introduz uma variável local. Mais formalmente a sintaxe das expressões aritméticas é a seguinte :

```
  <expr> ::= <constante inteira>
           | <ident>
           | ( <expr> )
           | <expr> + <expr>
           | <expr> - <expr>
           | <expr> * <expr>
           | <expr> / <expr>
           | - <expr>
           | let <ident> = <expr> in <expr>
```

Um exemplo de programa **Arith** pode ser :

```
set x = 1 + 2 + 3*4
print (let y = 10 in x + y)
```

Os identificadores das variáveis são formados de letras e de algarismos mas não podem começar por um algarismo. As palavras `set`, `print`, `let` et `in` são palavras reservadas, *i.e.* não podem ser utilizados como identificadores de variáveis. A prioridade dos operadors aritméticos é a usual e a construção `let in` tem a prioridade mais baixa.

Para ajudar à construção deste compilador, é-vos dado a sua estrutura de base (na forma de um conjunto de ficheiro OCaml e de um Makefile) que pode ser descarregado aqui : [arithc.zip](./arithc.zip). Uma vez este arquivo descomprimido obtém-se um directório `arithc/` que contém os ficheiros seguintes :

| Ficheiro                                  | Resumo                                             |
| ----------------------------------------- | -------------------------------------------------- |
| ast.mli                                   | a sintaxe abstracta de **Arith** (completo)        |
| lexer.mll                                 | o lexer (completo)                                 |
| parser.mly                                | o parser (completo)                                |
| [x86_64.mli](./html/X86_64.md), x86_64.ml | para gerar código X86-64 (completo)                |
| compile.ml                                | o processo de compilação em si (**por completar**) |
| main.ml                                   | o programa principal (completo)                    |
| Makefile                                  | para automatizar a compilação (completo)           |

O código fornecido compila ; para iniciar o processo de compilação, basta executar `make` num terminal (este comando vai invocar `dune build`)), ou melhor, compilar dentro do Emacs com `M-x compile` ou ainda `C-c C-c`.

 O código fornecido é no entanto incompleto : o código assembly produzido é vazio. Deve completar o ficheiro `compile.ml`. Quando o processo de compilação falha, pode colocar o cursor do editor no local de erro com o comando Emacs `M-x next-error` ou ainda `Ctrl-x `.

O programa espera um ficheiro **Arith** com o sufixo **.exp**. Ao executar `make`, o programa é lançado sobre o ficheiro `test.exp`, o que tem como efeito produzir um ficheiro `test.s` contendo o código *assembly*; em seguida, são executados os comandos

```
gcc -g -no-pie test.s -o test.out
./test.out
```

Para depurar, pode-se examinar o conteúdo de `test.s` e, se necessário, usar um depurador como o **gdb** com o comando `gdb ./test.out`, seguido do modo passo-a-passo com `step`.

**Nota para utilizadores de macOS:** é necessário modificar a linha `let mangle = mangle_none` no ficheiro `x86_64.ml` fornecido, substituindo-a por `let mangle = mangle_leading_underscore`. É também necessário substituir `let lab = abslab` por `let lab = rellab`.

#### Esquema de compilação

Realizar-se-á um processo compilação simples utilizando a pilha para arquivar os valores intermédios (*i.e.* os valores das sub-expressões). Relembra-se que um valor inteiro ocupa 8 bytes em memória. Pode alocar-se 8 bytes na pilha *substraindo* 8 ao valor de `%rsp` ou então utilizando a instrução `pushq`.

As variáveis globais são alocadas no segmento de dados (diretiva `.data` do assembly ; corresponde aqui ao campo `data` do tipo `X86_64.program`).

As variáveis locais são alocadas no fundo da pilha. O espaço necessário para o conjunto das variáveis locais será alocado no início da execução do programa (via uma substracção adequada sobre `%rsp`), após a adequada salvaguarda de `%rsp`. O registo `%rbp` é posicionado por forma a apontar para a parte superior do espaço reservado às variáveis locais, da seguinte forma:

```structured text
          +----------------+
          | ender. retorno |
          +----------------+
          |  antigo %rbp   |
  %rbp -> +----------------+  ^
          |  var. local    |  |
          |  var. local    |  |  frame_size (multiplo de 16)
          |   ...          |  |
  %rsp -> +----------------+  v
```



Assim toda a referência a uma variável local poderá ser feita com base em `%rbp`, com um _offset_ de -8, -16, etc.  conforme a variável em causa.

**Atenção**: antes de chamar uma função de biblioteca como `printf`, a pilha deve estar alinhada a 16 bytes. Uma vez determinada a variável `frame_size`, garante-se, portanto, que é um múltiplo de 16 (ver o código fornecido).



#### Tarefas por realizar

Deve ler CUIDADOSAMENTE o código que está em `compile.ml`. As partes por completar que estão assinaladas por `(* POR COMPLETAR *)`, são as seguintes :

1. a função `compile_expr` que compila uma expressão aritmética `e` numa sequência de instruções X86-64 cujo efeito é de colocar o valor de `e` no topo da pilha. Esta função está definida com recurso a função recursiva local `comprec` que toma como argumentos :

   - um parâmetro `env` de tipo `StrMap.t` : é um dicionário que associa a cada variável local a sua posição na pilha (relativamente a `%rbp`) ;
   - um parâmetro e `next` de tipo `int` : indica o primeiro local livre para a próxima variável local (relativamente a `%rbp`) ;
   - a expressão por compilar, sobre a qual se faz o filtro (*pattern matching*).

   

2. a função `compile_instr` que compila uma instrução de **Arith** numa sequência de instruções X86-64. Nos dois casos (`set x = e` ou `print e`), deve-se começar por compilar `e`, para depois encontrar o valor de `e` nop topo da pilha (não se esqueça de desempilhar).

   

3. A função `compile_program` que aplica `compile_instr` a todas as instruções do programa e junta o código :

   - antes, em especial para alocar espaço para as variáveis locais e posicionar `%rbp` ;
   - depois, para restaurar a pilha e terminar o programa com `ret`.

Indicações : poder-se-á proceder elemento (da linguagem) por elemento, testando de cada vez, na ordem seguinte :

1. expressões constantes `Cst`, instrução `Print` e saída com `ret` ;
2. operações aritméticas (constructor `Binop`) ;
3. variáveis globais (constructores `Var` e `Set`) ;
4. variáveis locais (constructores `Letin` e `Var`).

Testar-se-á no final com o ficheiro [test.exp](./test.exp) (igualmente fornecido), e cujo resultado deverá ser :

```
60
50
0
10
55
60
20
43
```

[solução](COMMING SOON)

#### Desafio opcional

Para utilizar um poco menos de espaço na pilha, podemos melhorar um pouco o esquema de compilação para que o resultado de `compile_expr` se encontre no registo `%rax` no lugar do topo da pilha. Assim só os resultados das sub-expressões esquerdas precisam de serem empilhadas.

[solução](COMING_SOON)

------

