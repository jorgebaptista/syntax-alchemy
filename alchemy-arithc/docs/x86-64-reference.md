# Módulo X86_64

Biblioteca para a escrita de programas em assembly X86-64.  
Apenas um fragmento relativamente pequeno do assembly é aqui modelado.

Autores: Jean-Christophe Filliâtre (CNRS), Kim Nguyen (Université Paris-Sud)

[tradução e adaptação - Simão Melo de Sousa]

---

## Tipos principais

```ocaml
type 'a asm
```
Tipo abstrato de código assembly. O parâmetro `'a` é usado como tipo fantasma.

```ocaml
type text = [ `text ] asm
type data = [ `data ] asm
```
Código assembly localizado na secção de texto ou na secção de dados.

```ocaml
type label = string
```
Etiquetas de endereços são cadeias de caracteres.

---

## Funções básicas

```ocaml
val nop : [> ] asm
```
Instrução vazia (no-op). Pode aparecer tanto em text como em data.

```ocaml
val (++) : ([< `data | `text ] as 'a) asm -> 'a asm -> 'a asm
```
Concatena dois fragmentos de código (de mesmo tipo).

```ocaml
val inline : string -> [> ] asm
```
`inline s` copia literalmente a cadeia `s` no ficheiro assembly gerado.

---

## Programa completo

```ocaml
type program = {
  text : text;
  data : data;
}
```

Um programa é constituído por uma zona de texto e uma zona de dados.

```ocaml
val print_program : Format.formatter -> program -> unit
val print_in_file : file:string -> program -> unit
```

`print_program fmt p` imprime o código do programa `p`no _formatter_ `fmt`.

`print_in_file file p` imprime o código do programa `p` no ficheiro de nome `file`.



---

## Registos

```ocaml
type size = [ `B | `W | `L | `Q ]
type 'size register
```

`register`: Tipo abstratos para os registos

### Registos de 64 bits

rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, r8 … r15

### Registos de 32 bits
eax, ebx, ecx, edx, esi, edi, ebp, esp, r8d … r15d

### Registos de 16 bits
ax, bx, cx, dx, si, di, bp, sp, r8w … r15w

### Registos de 8 bits
al, bl, cl, dl, ah, bh, ch, dh, sil, dil, bpl, spl, r8b … r15b

---

## Operandos

```ocaml
type 'size operand
```

- `imm n` — operando imediato `$i` (com `i` inteiro) 

  ```ocaml
  val imm : int -> [>  ] operand
  ```

  

- `imm32`, `imm64` — `$i` imediatos de 32 ou 64 bits

  ```ocaml
  val imm32 : int32 -> [>  ] operand
  val imm64 : int64 -> [>  ] operand
  ```

  

- `reg r` , `!%r` — registros 

  ```ocaml
  val reg : 'size register -> 'size operand
  val (!%) : 'size register -> 'size operand
  ```

- operando indireto `ofs(base, index, scale)`

  ``` ocaml
  val ind : ?ofs:int ->
         ?index:'size1 register ->
         ?scale:int -> 'size2 register -> [>  ] operand
  ```

  

- `lab L` — label L

  ```ocaml 
  val lab : label -> [>  ] operand
  ```

- `ilab L` — label imediato `$L` 

  ```ocaml
  val ilab : label -> [ `Q ] operand
  ```

  

---

## Instruções

### Transferência

- `movb`, `movw`, `movl`, `movq` — movimentos entre operandos (8/16/32/64 bits)

- Extensões com sinal: `movsbw`, `movsbl`, `movsbq`, `movswl`, `movswq`, `movslq`

- Extensões com zero: `movzbw`, `movzbl`, `movzbq`, `movzwl`, `movzwq`

- `movabsq` — cópia de um imediato de 64 bits para register

  ```ocaml
  val movb : [ `B ] operand -> [ `B ] operand -> text
  val movw : [ `W ] operand -> [ `W ] operand -> text
  val movl : [ `L ] operand -> [ `L ] operand -> text
  val movq : [ `Q ] operand -> [ `Q ] operand -> text
  ```

  Nota de atenção: nem todas as combinações de opérandes são autorizadas.

  ```ocaml
  val movsbw : [ `B ] operand -> [ `W ] register -> text
  val movsbl : [ `B ] operand -> [ `L ] register -> text
  val movsbq : [ `B ] operand -> [ `Q ] register -> text
  val movswl : [ `W ] operand -> [ `L ] register -> text
  val movswq : [ `W ] operand -> [ `Q ] register -> text
  val movslq : [ `L ] operand -> [ `Q ] register -> text
  ```

  8->64 bit, com uma extensão de sinal

  ```ocaml
  val movzbw : [ `B ] operand -> [ `W ] register -> text
  val movzbl : [ `B ] operand -> [ `L ] register -> text
  val movzbq : [ `B ] operand -> [ `Q ] register -> text
  val movzwl : [ `W ] operand -> [ `L ] register -> text
  val movzwq : [ `W ] operand -> [ `Q ] register -> text
  ```

  8->64 bit, avec extension par zéro

  ```ocaml
  val movabsq : [ `Q ] operand -> [ `Q ] register -> text
  ```

  Copia um valor imediata de 64 bits para um registo

### Aritmética

`lea`, `inc`, `dec`, `neg`, `add`, `sub`, `imul`, `idiv`, `cqto`  
Disponíveis em variantes b/w/l/q.

```ocaml
val leab : [ `B ] operand -> [ `B ] register -> text
val leaw : [ `W ] operand -> [ `W ] register -> text
val leal : [ `L ] operand -> [ `L ] register -> text
val leaq : [ `Q ] operand -> [ `Q ] register -> text
val incb : [ `B ] operand -> text
val incw : [ `W ] operand -> text
val incl : [ `L ] operand -> text
val incq : [ `Q ] operand -> text
val decb : [ `B ] operand -> text
val decw : [ `W ] operand -> text
val decl : [ `L ] operand -> text
val decq : [ `Q ] operand -> text
val negb : [ `B ] operand -> text
val negw : [ `W ] operand -> text
val negl : [ `L ] operand -> text
val negq : [ `Q ] operand -> text
val addb : [ `B ] operand -> [ `B ] operand -> text
val addw : [ `W ] operand -> [ `W ] operand -> text
val addl : [ `L ] operand -> [ `L ] operand -> text
val addq : [ `Q ] operand -> [ `Q ] operand -> text
val subb : [ `B ] operand -> [ `B ] operand -> text
val subw : [ `W ] operand -> [ `W ] operand -> text
val subl : [ `L ] operand -> [ `L ] operand -> text
val subq : [ `Q ] operand -> [ `Q ] operand -> text
val imulw : [ `W ] operand -> [ `W ] operand -> text
val imull : [ `L ] operand -> [ `L ] operand -> text
val imulq : [ `Q ] operand -> [ `Q ] operand -> text
val idivq : [ `Q ] operand -> text
val cqto : text
```

### Lógicas

`not`, `and`, `or`, `xor`  
Variantes b/w/l/q.

```ocaml
val notb : [ `B ] operand -> text
val notw : [ `W ] operand -> text
val notl : [ `L ] operand -> text
val notq : [ `Q ] operand -> text
val andb : [ `B ] operand -> [ `B ] operand -> text
val andw : [ `W ] operand -> [ `W ] operand -> text
val andl : [ `L ] operand -> [ `L ] operand -> text
val andq : [ `Q ] operand -> [ `Q ] operand -> text
val orb : [ `B ] operand -> [ `B ] operand -> text
val orw : [ `W ] operand -> [ `W ] operand -> text
val orl : [ `L ] operand -> [ `L ] operand -> text
val orq : [ `Q ] operand -> [ `Q ] operand -> text
val xorb : [ `B ] operand -> [ `B ] operand -> text
val xorw : [ `W ] operand -> [ `W ] operand -> text
val xorl : [ `L ] operand -> [ `L ] operand -> text
val xorq : [ `Q ] operand -> [ `Q ] operand -> text
```

Operações de manipulação de bits. "and" bit-wise, "or" bit-wise, "not" bit-wise

### Deslocamentos

`shl` / `sal` — shift left  
`shr` / `sar` — shift right lógico / aritmético  
Variantes b/w/l/q.

```ocaml
val shlb : [ `B ] operand -> [ `B ] operand -> text
val shlw : [ `W ] operand -> [ `W ] operand -> text
val shll : [ `L ] operand -> [ `L ] operand -> text
val shlq : [ `Q ] operand -> [ `Q ] operand -> text
```

nota: `shl`é idêntico à `sal`

```ocaml
val shrb : [ `B ] operand -> [ `B ] operand -> text
val shrw : [ `W ] operand -> [ `W ] operand -> text
val shrl : [ `L ] operand -> [ `L ] operand -> text
val shrq : [ `Q ] operand -> [ `Q ] operand -> text
val sarb : [ `B ] operand -> [ `B ] operand -> text
val sarw : [ `W ] operand -> [ `W ] operand -> text
val sarl : [ `L ] operand -> [ `L ] operand -> text
val sarq : [ `Q ] operand -> [ `Q ] operand -> text
```

### Saltos

- Chamada de função e retorno

  ```ocaml
  val call : label -> text
  val call_star : [ `Q ] operand -> text
  val leave : text
  val ret : text
  ```

- Salto incondicional, salto incondicional para um endereço calculado

  ```ocaml
  val jmp : label -> text
  val jmp_star : [ `Q ] operand -> text
  ```

  

- Saltos condicionais: `je`, `jz`, `jne`, `jnz`, `js`, `jns`, `jg`, `jge`, `jl`, `jle`, `ja`, `jae`, `jb`, `jbe`

  ```ocaml
  val je : label -> text
  val jz : label -> text
  val jne : label -> text
  val jnz : label -> text
  val js : label -> text
  val jns : label -> text
  val jg : label -> text
  val jge : label -> text
  val jl : label -> text
  val jle : label -> text
  val ja : label -> text
  val jae : label -> text
  val jb : label -> text
  val jbe : label -> text
  ```

  Saltos condicionais

---

### Comparações e condições

- `cmpb`, `cmpw`, `cmpl`, `cmpq`

- `testb`, `testw`, `testl`, `testq`

- Instruções `set*`: `sete`, `setne`, `sets`, `setns`, `setg`, `setge`, `setl`, `setle`, `seta`, `setae`, `setb`, `setbe`

  ```ocaml
  val cmpb : [ `B ] operand -> [ `B ] operand -> text
  val cmpw : [ `W ] operand -> [ `W ] operand -> text
  val cmpl : [ `L ] operand -> [ `L ] operand -> text
  val cmpq : [ `Q ] operand -> [ `Q ] operand -> text
  val testb : [ `B ] operand -> [ `B ] operand -> text
  val testw : [ `W ] operand -> [ `W ] operand -> text
  val testl : [ `L ] operand -> [ `L ] operand -> text
  val testq : [ `Q ] operand -> [ `Q ] operand -> text
  val sete : [ `B ] operand -> text
  val setne : [ `B ] operand -> text
  val sets : [ `B ] operand -> text
  val setns : [ `B ] operand -> text
  val setg : [ `B ] operand -> text
  val setge : [ `B ] operand -> text
  val setl : [ `B ] operand -> text
  val setle : [ `B ] operand -> text
  val seta : [ `B ] operand -> text
  val setae : [ `B ] operand -> text
  val setb : [ `B ] operand -> text
  val setbe : [ `B ] operand -> text
  ```

Estas colocam o byte-operando a 1 ou 0 conforme o resultado do teste.

---

### Pilha

- `pushq` — empilha um valor de 64 bits

  ```ocaml
  val pushq : [ `Q ] operand -> text
  ```

  `pushq r` coloca o conteúdo de `r`no topo da pilha. De notar que `%rsp` aponta para o endereço do último local usado

- `popq` — desempilha um valor para o registo

  ```
  val popq : [ `Q ] register -> text
  ```

  `popq r` coloca a palavra presente no topo da pilha para `r` e remove-o da pilha

---

## Diretivas e utilitários

- `label l` — define uma etiqueta. Pode este ficar na zona de `text` ou na zona `data`   

  ```ocaml
  val label : label -> [>  ] asm
  ```

- `globl l` — declaração `.globl` (tipicamente para `main`)

  ```ocaml
  val globl : label -> [>  ] asm
  ```

- `comment s` — coloca um comentário `s` no código gerado. Pode este ficar na zona de `text` ou na zona `data`   

  ```ocaml
  val comment : string -> [>  ] asm
  ```

---

## Dados

- `string s` — cadeia de caracteres terminada por 0

  ```ocaml
  val string : string -> data
  ```

- `dbyte`, `dword`, `dint`, `dquad` — listas de valores de 1/2/4/8 bytes

  ```ocaml
  val dbyte : int list -> data
  val dword : int list -> data
  val dint : int list -> data
  val dquad : int list -> data
  ```

- `address [l1; l2; …]` — lista de endereços (labels) armazenada na zona de dados (`data`) com `.quad`

  ```ocaml
  val address : label list -> data
  ```

- `space n` — reserva `n` bytes no segmento de dados (inicializados a 0)

  ```ocaml
  val space : int -> data
  ```

---

## Módulo Completo

```ocaml
module X86_64: sig
  type 'a asm
  type text = [ `text ] asm
  type data = [ `data ] asm
  type label = string
  val nop : [>  ] asm
  val ( ++ ) :
    ([< `data | `text ] as 'a) asm -> 'a asm -> 'a asm
  val inline : string -> [>  ] asm
  type program = { text : text; data : data; }
  val print_program : Stdlib.Format.formatter -> program -> unit
  val print_in_file : file:string -> program -> unit
  type size = [ `B | `L | `Q | `W ]
  type 'size register
  val rax : [ `Q ] register
  val rbx : [ `Q ] register
  val rcx : [ `Q ] register
  val rdx : [ `Q ] register
  val rsi : [ `Q ] register
  val rdi : [ `Q ] register
  val rbp : [ `Q ] register
  val rsp : [ `Q ] register
  val r8 : [ `Q ] register
  val r9 : [ `Q ] register
  val r10 : [ `Q ] register
  val r11 : [ `Q ] register
  val r12 : [ `Q ] register
  val r13 : [ `Q ] register
  val r14 : [ `Q ] register
  val r15 : [ `Q ] register
  val eax : [ `L ] register
  val ebx : [ `L ] register
  val ecx : [ `L ] register
  val edx : [ `L ] register
  val esi : [ `L ] register
  val edi : [ `L ] register
  val ebp : [ `L ] register
  val esp : [ `L ] register
  val r8d : [ `L ] register
  val r9d : [ `L ] register
  val r10d : [ `L ] register
  val r11d : [ `L ] register
  val r12d : [ `L ] register
  val r13d : [ `L ] register
  val r14d : [ `L ] register
  val r15d : [ `L ] register
  val ax : [ `W ] register
  val bx : [ `W ] register
  val cx : [ `W ] register
  val dx : [ `W ] register
  val si : [ `W ] register
  val di : [ `W ] register
  val bp : [ `W ] register
  val sp : [ `W ] register
  val r8w : [ `W ] register
  val r9w : [ `W ] register
  val r10w : [ `W ] register
  val r11w : [ `W ] register
  val r12w : [ `W ] register
  val r13w : [ `W ] register
  val r14w : [ `W ] register
  val r15w : [ `W ] register
  val al : [ `B ] register
  val bl : [ `B ] register
  val cl : [ `B ] register
  val dl : [ `B ] register
  val ah : [ `B ] register
  val bh : [ `B ] register
  val ch : [ `B ] register
  val dh : [ `B ] register
  val sil : [ `B ] register
  val dil : [ `B ] register
  val bpl : [ `B ] register
  val spl : [ `B ] register
  val r8b : [ `B ] register
  val r9b : [ `B ] register
  val r10b : [ `B ] register
  val r11b : [ `B ] register
  val r12b : [ `B ] register
  val r13b : [ `B ] register
  val r14b : [ `B ] register
  val r15b : [ `B ] register
  type 'size operand
  val imm : int -> [>  ] operand
  val imm32 : int32 -> [>  ] operand
  val imm64 : int64 -> [>  ] operand
  val reg : 'size register -> 'size operand
  val ( !% ) : 'size register -> 'size operand
  val ind :
    ?ofs:int ->
    ?index:'size1 register ->
    ?scale:int -> 'size2 register -> [>  ] operand
  val lab : label -> [>  ] operand
  val ilab : label -> [ `Q ] operand
  val movb : [ `B ] operand -> [ `B ] operand -> text
  val movw : [ `W ] operand -> [ `W ] operand -> text
  val movl : [ `L ] operand -> [ `L ] operand -> text
  val movq : [ `Q ] operand -> [ `Q ] operand -> text
  val movsbw : [ `B ] operand -> [ `W ] register -> text
  val movsbl : [ `B ] operand -> [ `L ] register -> text
  val movsbq : [ `B ] operand -> [ `Q ] register -> text
  val movswl : [ `W ] operand -> [ `L ] register -> text
  val movswq : [ `W ] operand -> [ `Q ] register -> text
  val movslq : [ `L ] operand -> [ `Q ] register -> text
  val movzbw : [ `B ] operand -> [ `W ] register -> text
  val movzbl : [ `B ] operand -> [ `L ] register -> text
  val movzbq : [ `B ] operand -> [ `Q ] register -> text
  val movzwl : [ `W ] operand -> [ `L ] register -> text
  val movzwq : [ `W ] operand -> [ `Q ] register -> text
  val movabsq : [ `Q ] operand -> [ `Q ] register -> text
  val leab : [ `B ] operand -> [ `B ] register -> text
  val leaw : [ `W ] operand -> [ `W ] register -> text
  val leal : [ `L ] operand -> [ `L ] register -> text
  val leaq : [ `Q ] operand -> [ `Q ] register -> text
  val incb : [ `B ] operand -> text
  val incw : [ `W ] operand -> text
  val incl : [ `L ] operand -> text
  val incq : [ `Q ] operand -> text
  val decb : [ `B ] operand -> text
  val decw : [ `W ] operand -> text
  val decl : [ `L ] operand -> text
  val decq : [ `Q ] operand -> text
  val negb : [ `B ] operand -> text
  val negw : [ `W ] operand -> text
  val negl : [ `L ] operand -> text
  val negq : [ `Q ] operand -> text
  val addb : [ `B ] operand -> [ `B ] operand -> text
  val addw : [ `W ] operand -> [ `W ] operand -> text
  val addl : [ `L ] operand -> [ `L ] operand -> text
  val addq : [ `Q ] operand -> [ `Q ] operand -> text
  val subb : [ `B ] operand -> [ `B ] operand -> text
  val subw : [ `W ] operand -> [ `W ] operand -> text
  val subl : [ `L ] operand -> [ `L ] operand -> text
  val subq : [ `Q ] operand -> [ `Q ] operand -> text
  val imulw : [ `W ] operand -> [ `W ] operand -> text
  val imull : [ `L ] operand -> [ `L ] operand -> text
  val imulq : [ `Q ] operand -> [ `Q ] operand -> text
  val idivq : [ `Q ] operand -> text
  val cqto : text
  val notb : [ `B ] operand -> text
  val notw : [ `W ] operand -> text
  val notl : [ `L ] operand -> text
  val notq : [ `Q ] operand -> text
  val andb : [ `B ] operand -> [ `B ] operand -> text
  val andw : [ `W ] operand -> [ `W ] operand -> text
  val andl : [ `L ] operand -> [ `L ] operand -> text
  val andq : [ `Q ] operand -> [ `Q ] operand -> text
  val orb : [ `B ] operand -> [ `B ] operand -> text
  val orw : [ `W ] operand -> [ `W ] operand -> text
  val orl : [ `L ] operand -> [ `L ] operand -> text
  val orq : [ `Q ] operand -> [ `Q ] operand -> text
  val xorb : [ `B ] operand -> [ `B ] operand -> text
  val xorw : [ `W ] operand -> [ `W ] operand -> text
  val xorl : [ `L ] operand -> [ `L ] operand -> text
  val xorq : [ `Q ] operand -> [ `Q ] operand -> text
  val shlb : [ `B ] operand -> [ `B ] operand -> text
  val shlw : [ `W ] operand -> [ `W ] operand -> text
  val shll : [ `L ] operand -> [ `L ] operand -> text
  val shlq : [ `Q ] operand -> [ `Q ] operand -> text
  val shrb : [ `B ] operand -> [ `B ] operand -> text
  val shrw : [ `W ] operand -> [ `W ] operand -> text
  val shrl : [ `L ] operand -> [ `L ] operand -> text
  val shrq : [ `Q ] operand -> [ `Q ] operand -> text
  val sarb : [ `B ] operand -> [ `B ] operand -> text
  val sarw : [ `W ] operand -> [ `W ] operand -> text
  val sarl : [ `L ] operand -> [ `L ] operand -> text
  val sarq : [ `Q ] operand -> [ `Q ] operand -> text
  val call : label -> text
  val call_star : [ `Q ] operand -> text
  val leave : text
  val ret : text
  val jmp : label -> text
  val jmp_star : [ `Q ] operand -> text
  val je : label -> text
  val jz : label -> text
  val jne : label -> text
  val jnz : label -> text
  val js : label -> text
  val jns : label -> text
  val jg : label -> text
  val jge : label -> text
  val jl : label -> text
  val jle : label -> text
  val ja : label -> text
  val jae : label -> text
  val jb : label -> text
  val jbe : label -> text
  val cmpb : [ `B ] operand -> [ `B ] operand -> text
  val cmpw : [ `W ] operand -> [ `W ] operand -> text
  val cmpl : [ `L ] operand -> [ `L ] operand -> text
  val cmpq : [ `Q ] operand -> [ `Q ] operand -> text
  val testb : [ `B ] operand -> [ `B ] operand -> text
  val testw : [ `W ] operand -> [ `W ] operand -> text
  val testl : [ `L ] operand -> [ `L ] operand -> text
  val testq : [ `Q ] operand -> [ `Q ] operand -> text
  val sete : [ `B ] operand -> text
  val setne : [ `B ] operand -> text
  val sets : [ `B ] operand -> text
  val setns : [ `B ] operand -> text
  val setg : [ `B ] operand -> text
  val setge : [ `B ] operand -> text
  val setl : [ `B ] operand -> text
  val setle : [ `B ] operand -> text
  val seta : [ `B ] operand -> text
  val setae : [ `B ] operand -> text
  val setb : [ `B ] operand -> text
  val setbe : [ `B ] operand -> text
  val pushq : [ `Q ] operand -> text
  val popq : [ `Q ] register -> text
  val label : label -> [>  ] asm
  val globl : label -> [>  ] asm
  val comment : string -> [>  ] asm
  val string : string -> data
  val dbyte : int list -> data
  val dword : int list -> data
  val dint : int list -> data
  val dquad : int list -> data
  val address : label list -> data
  val space : int -> X86_64.data
end
```

