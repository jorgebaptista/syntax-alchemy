
(* Produção de código para a linguagem Arith *)

open Format
open X86_64
open Ast

(* Excepção por lançar quando uma variável (local ou global) não é usada como deve ser *)
exception VarUndef of string

(* Tamanho da frame, em byte (cada variável local ocupa 8 bytes) *)
let frame_size = ref 0

(* As variáveis globais são arquivadas numa hash table (uma tabela de símbolos globais) *)
let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

(* Usamos uma tabela associativa cujas chaves são variáveis locais, i.e. uma tabela de
   símbolos locais cujas chaves são strings e os valores são a posição desta variável
   relativamente a %rbp (em bytes) *)
module StrMap = Map.Make(String)

(* Compilação de uma expressão *)
let compile_expr =
(* Função recursiva local à compile_expr usada para gerar o código máquina a partir
   da árvore de sintaxe abstracta associada ao valor de tipo Ast.expr.
   No fim da execução deste código, o valor *deve* estar no topo da pilha *)
  let rec comprec env next = function
    | Cst i ->
        nop (* POR COMPLETAR *)
    | Var x ->
        nop (* POR COMPLETAR *)
    | Binop (o, e1, e2)->
        nop (* POR COMPLETAR *)
    | Letin (x, e1, e2) ->
        if !frame_size = next then frame_size := 8 + !frame_size;
        nop (* POR COMPLETAR *)
  in
  comprec StrMap.empty 0

(* Compilação de uma instrução *)
let compile_instr = function
  | Set (x, e) ->
      nop (* POR COMPLETAR *)
  | Print e ->
      nop (* POR COMPLETAR *)


(* Compila o programa p e grava o código no ficheiro ofile *)
let compile_program p ofile =
  let code = List.map compile_instr p in
  let code = List.fold_right (++) code nop in
  if !frame_size mod 16 = 8 then frame_size := 8 + !frame_size;
  let p =
    { text =
        globl "main" ++ label "main" ++
        nop (* POR COMPLETAR *) ++
        code ++
        nop (* POR COMPLETAR *) ++
        label "print_int" ++
        pushq !%rbp ++ (* assegura, em particular, as questões de alignamento *)
        movq !%rdi !%rsi ++
        leaq (lab ".Sprint_int") rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        popq rbp ++
        ret;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  (* "flush" do buffer para assegurar que tudo o que há por gravar foi de facto gravado antes de fecha-lo *)
  fprintf fmt "@?";
  close_out f
