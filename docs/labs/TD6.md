# TD 6 - Interpretador de mini-Turtle

O objectivo desta aula laboratorial é a implementação de uma análise sintáctica para a pequena linguagem Logo (tartaruga gráfica, aqui designada de mini-Turtle) para a qual é fornecido um interpretador ; não é necessário para este exercício estar familiarizado com a linguagem Logo.

É fornecido uma estrutura de base (na base de um conjunto de ficheiros Ocaml e de um Makefile) que pode descarregar aqui : [mini-turtle.tar.gz](./mini-turtle.tar.gz). Uma vez descomprimido com `tar zxvf mini-turtle.tar.gz`, obtém-se uma pasta chamada `mini-turtle/` que contém os ficheiros seguintes :

| turtle.ml(i) | a tartaruga gráfica (completo)                    |
| ------------ | ------------------------------------------------- |
| ast.mli      | a sintaxe abstracta de **mini-Turtle** (completo) |
| lexer.mll    | o analisador léxico (por completar)               |
| parser.mly   | o analisador sintáctico (por completar)           |
| interp.ml    | o interpretador (completo)                        |
| main.ml      | o programa principal (completo)                   |
| Makefile     | para automatizar a compilação (completo)          |

O código fornecido compila mas não está completo. O executável tem por nome `mini-turtle` e invoca-se com a informação de um ficheiro **mini-Turtle** com um sufixo `.logo`, da seguinte forma :

```
  ./mini-turtle ficheiro.logo
```

### Sintaxe de mini-Turtle

#### Convenções léxicas

Os espaços, tabulações e carrier-return são caracteres brancos. Há dois tipos de comentários : os que começam com `//` e terminam com o fim de linha, ou comentários enquadrados por `(*` e `*)` (não aninhados). Os identificadores seguintes são palavras chaves da linguagem :

```
     if else def repeat penup pendown forward turnleft
     turnright color black white red green blue
```

Um identificador `ident` contém letras, algarismos e o caracter `_` e este começa por uma letra. Uma constante inteira `integer` é uma sequência de algarismos.

#### Sintaxe

Os nomes em itálico, como `*expr*`, designam não-terminais. A notação `*stmt**` designa uma repetição zero, uma ou mais vezes do não-terminal `*stmt*`. A notação `*expr\*,*` designa uma repetição do não-terminal `*expr*`, estando as ocorrências separadas pelo lexema `,` (uma vírgula).

```
  file ::= def* stmt*
  def  ::= def ident ( ident*, ) stmt
  stmt ::= penup
         | pendown
         | forward expr
         | turnleft expr
         | turnright expr
         | color color
         | ident ( expr*, )
         | if expr stmt
         | if expr stmt else stmt
         | repeat expr stmt
         | { stmt* }
  expr ::= integer
         | ident
         | expr + expr
         | expr - expr
         | expr * expr
         | expr / expr
         | - expr
         | ( expr )
 color ::= black | white | red | green | blue
```

As prioridades dos operadores aritméticos binários são usuais e a negação tem uma prioridade maior.

### Trabalho por realizar

O trabalho consiste em completar e terminar os ficheiros `lexer.mll` (ocamllex) e `parser.mly` (menhir). As questões seguintes sugiram uma esquema possível de resolução. Há outros possíveis, claro. É possível testar cada tentativa via a modificação do ficheiro `test.logo`. O comando `make` constrói o executável `mini-turtle/` e executa-o sobre o ficheiro `test.logo`. Uma janela gráfica abre-se e mostra a interpretação do programa logo em causa. A janela pode ser fechada ao carregar numa qualquer tecla.

#### Questão 1. Comentários

Completar o ficheiro `lexer.mll` para ignorar os caracteres brancos e os comentários, e devolver o lexema `EOF` quando o fim de ficheiro é atingido. O comando `make` deve neste caso abrir uma janela vazia, porque o ficheiro `test.logo` fornecido originalmente só contém comentários.

#### Questão 2. Expressão aritméticas

Juntar regras de gramáticas necessárias ao reconhecimento das expressões aritméticas e unicamente da instrução `forward`. O ficheiro `test.logo` contendo

```
  forward 100
```

deve então ser aceite e a janela deve abrir-se com um desenho simples constituído de um traço horizontal (com comprimento de 100 píxel). Verificar que as prioridades dos operadores aritméticos estão devidamente estabelecidas, por exemplo com o comando seguinte :

```
  forward 100 + 1 * 0
```

Se as prioridades não estiverem devidamente especificadas, um ponto deverá aparecer no lugar de um esperado traço.

#### Questão 3. Outras instruções atómicas

Juntar a sintaxe das instruções atómicas restantes, ou seja `penup`, `pendown`, `turnleft`, `turnright` e `color`.

Testar com programas como

```
forward 100
turnleft 90
color red
forward 100
```

#### Questão 4. Blocos e estruturas de controlo

Juntar a sintaxe abstracta para blocos e estruturas de controlo `if` e `repeat`. As duas regras de gramáticas para instrução `if` devem produzir um conflito de 

shift/reduce

. Identificá-lo, diagnosticar o fenómeno que provoca tal conflito e resolver em consequência o conflito da forma que parecer mais apropriado.

Testar com programas como

```
repeat 4 {
  forward 100
  turnleft 90
}
```

#### Questão 5. Funções

Juntar, finalmente, sintaxe para declarações de funções e para as chamadas de funções nas instruções.

Poderemos testar com os ficheiros fornecidos na pasta `tests` do arquivo, ou seja :

- [hilbert.logo](./hilbert.logo)
- [poly.logo](./poly.logo)
- [von_koch.logo](./von_koch.logo)
- [zigzag.logo](./zigzag.logo)

O comando `make tests` invoca `mini-turtle` em cada um destes ficheiros. Deve-se obter as quatro imagens seguintes (carregando numa tecla após o 

display

 de cada imagem) :

| ![img](./hilbert.png) | ![img](./poly.png) | ![img](./von_koch.png) | ![img](./zigzag.png) |
| --------------------- | ------------------ | ---------------------- | -------------------- |
|                       |                    |                        |                      |

solução : [lexer.mll](./TOBEDEFINED) / [parser.mly](-/TOBEDEFINED)

