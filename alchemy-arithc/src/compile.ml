(* Produção de código para a linguagem Arith *)

open Format
open X86_64
open Ast

(* Excepção por lançar quando uma variável (local ou global) não é usada como deve ser *)
exception VarUndef of string

(* Tamanho da frame, em byte (cada variável local ocupa 8 bytes) *)
let frame_size = ref 0

(* Counter for generating unique labels *)
let label_counter = ref 0

let new_label prefix =
  let n = !label_counter in
  incr label_counter;
  prefix ^ string_of_int n

(* As variáveis globais são arquivadas numa hash table (uma tabela de símbolos globais) *)
let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

(* Usamos uma tabela associativa cujas chaves são variáveis locais, i.e. uma tabela de
   símbolos locais cujas chaves são strings e os valores são a posição desta variável
   relativamente a %rbp (em bytes) *)
module StrMap = Map.Make (String)

(* Compilação de uma expressão *)
let compile_expr =
  (* Função recursiva local à compile_expr usada para gerar o código máquina a partir
   da árvore de sintaxe abstracta associada ao valor de tipo Ast.expr.
   No fim da execução deste código, o valor *deve* estar no topo da pilha *)
  let rec comprec env next = function
    | Cst i ->
        (* Push constant value onto stack *)
        pushq (imm i)
    | Bool b ->
        (* Booleans: true = 1, false = 0 *)
        pushq (imm (if b then 1 else 0))
    | Var x ->
        (* Check if it's a local variable *)
        if StrMap.mem x env then
          (* Local variable - load from stack frame *)
          let offset = StrMap.find x env in
          pushq (ind ~ofs:offset rbp)
        else if Hashtbl.mem genv x then
          (* Global variable - load from data segment *)
          pushq (lab x)
        else raise (VarUndef x)
    | Unop (Neg, e) ->
        (* Compile expression, negate result *)
        comprec env next e ++ popq rax ++ negq !%rax ++ pushq !%rax
    | Unop (Not, e) ->
        (* Compile expression, logical not: 0 -> 1, nonzero -> 0 *)
        comprec env next e ++ popq rax ++ testq !%rax !%rax ++ sete !%al
        ++ movzbq !%al rax ++ pushq !%rax
    | Binop (And, e1, e2) ->
        (* Short-circuit AND: if e1 is false, don't evaluate e2 *)
        let lbl_false = new_label ".Land_false" in
        let lbl_end = new_label ".Land_end" in
        comprec env next e1 ++ popq rax ++ testq !%rax !%rax ++ jz lbl_false
        ++ comprec env next e2 ++ popq rax ++ testq !%rax !%rax ++ jz lbl_false
        ++ pushq (imm 1)
        ++ jmp lbl_end ++ label lbl_false
        ++ pushq (imm 0)
        ++ label lbl_end
    | Binop (Or, e1, e2) ->
        (* Short-circuit OR: if e1 is true, don't evaluate e2 *)
        let lbl_true = new_label ".Lor_true" in
        let lbl_end = new_label ".Lor_end" in
        comprec env next e1 ++ popq rax ++ testq !%rax !%rax ++ jnz lbl_true
        ++ comprec env next e2 ++ popq rax ++ testq !%rax !%rax ++ jnz lbl_true
        ++ pushq (imm 0)
        ++ jmp lbl_end ++ label lbl_true
        ++ pushq (imm 1)
        ++ label lbl_end
    | Binop (o, e1, e2) -> (
        (* Compile e1, result on stack *)
        comprec env next e1
        ++
        (* Compile e2, result on stack *)
        comprec env next e2
        ++
        (* Pop e2 into %rdi, e1 into %rax *)
        popq rdi ++ popq rax
        ++
        (* Perform operation: %rax = %rax op %rdi *)
        match o with
        | Add -> addq !%rdi !%rax ++ pushq !%rax
        | Sub -> subq !%rdi !%rax ++ pushq !%rax
        | Mul -> imulq !%rdi !%rax ++ pushq !%rax
        | Div ->
            (* For division: rax = rax / rdi *)
            (* Need to sign-extend rax into rdx:rax *)
            cqto ++ idivq !%rdi ++ pushq !%rax
        | Eq -> cmpq !%rdi !%rax ++ sete !%al ++ movzbq !%al rax ++ pushq !%rax
        | Neq ->
            cmpq !%rdi !%rax ++ setne !%al ++ movzbq !%al rax ++ pushq !%rax
        | Lt -> cmpq !%rdi !%rax ++ setl !%al ++ movzbq !%al rax ++ pushq !%rax
        | Le -> cmpq !%rdi !%rax ++ setle !%al ++ movzbq !%al rax ++ pushq !%rax
        | Gt -> cmpq !%rdi !%rax ++ setg !%al ++ movzbq !%al rax ++ pushq !%rax
        | Ge -> cmpq !%rdi !%rax ++ setge !%al ++ movzbq !%al rax ++ pushq !%rax
        | And | Or -> assert false (* handled above *))
    | Letin (x, e1, e2) ->
        (* Allocate space for local variable if needed *)
        if !frame_size = next then frame_size := 8 + !frame_size;
        (* Compile e1 (result on stack) *)
        comprec env next e1
        ++
        (* Pop result into local variable slot *)
        popq rax
        ++ movq !%rax (ind ~ofs:(-next - 8) rbp)
        ++
        (* Compile e2 with x in environment, pointing to its stack location *)
        comprec (StrMap.add x (-next - 8) env) (next + 8) e2
    | IfExpr (cond, e1, e2) ->
        (* Compile conditional expression: if cond then e1 else e2 *)
        let lbl_else = new_label ".Lelse" in
        let lbl_end = new_label ".Lendif" in
        comprec env next cond ++ popq rax ++ testq !%rax !%rax ++ jz lbl_else
        ++ comprec env next e1 ++ jmp lbl_end ++ label lbl_else
        ++ comprec env next e2 ++ label lbl_end
  in
  comprec StrMap.empty 0

(* Compilação de uma instrução *)
let rec compile_instr env = function
  | Set (x, e) ->
      (* Add variable to global environment if not already there *)
      if not (Hashtbl.mem genv x) then Hashtbl.add genv x ();
      (* Compile expression (result will be on stack) *)
      compile_expr e
      ++
      (* Pop value from stack and store in global variable *)
      popq rax
      ++ movq !%rax (lab x)
  | Print e ->
      (* Compile expression (result will be on stack) *)
      compile_expr e
      ++
      (* Pop value from stack into %rdi (first argument for print_int) *)
      popq rdi
      ++
      (* Call print_int function *)
      call "print_int"
  | If (cond, then_stmts, else_stmts) ->
      let lbl_else = new_label ".Lelse" in
      let lbl_end = new_label ".Lendif" in
      (* Compile condition *)
      compile_expr cond ++ popq rax ++ testq !%rax !%rax ++ jz lbl_else
      ++
      (* Then branch *)
      compile_block env then_stmts
      ++ jmp lbl_end
      ++
      (* Else branch *)
      label lbl_else
      ++ compile_block env else_stmts
      ++ label lbl_end
  | While (cond, body) ->
      let lbl_start = new_label ".Lwhile" in
      let lbl_end = new_label ".Lendwhile" in
      (* Loop start *)
      label lbl_start
      ++
      (* Compile condition *)
      compile_expr cond ++ popq rax ++ testq !%rax !%rax ++ jz lbl_end
      ++
      (* Body *)
      compile_block env body
      ++
      (* Jump back to start *)
      jmp lbl_start
      ++
      (* End of loop *)
      label lbl_end

and compile_block env stmts =
  List.fold_left (fun code stmt -> code ++ compile_instr env stmt) nop stmts

(* Compila o programa p e grava o código no ficheiro ofile *)
let compile_program p ofile =
  (* Reset state for each compilation *)
  label_counter := 0;
  Hashtbl.clear genv;
  frame_size := 0;

  let code = List.map (compile_instr StrMap.empty) p in
  let code = List.fold_right ( ++ ) code nop in
  if !frame_size mod 16 = 8 then frame_size := 8 + !frame_size;
  let p =
    {
      text =
        globl "main" ++ label "main"
        ++
        (* Allocate stack frame and set up %rbp *)
        pushq !%rbp ++ movq !%rsp !%rbp
        ++ subq (imm !frame_size) !%rsp
        ++ code
        ++
        (* Restore stack and return *)
        movq !%rbp !%rsp ++ popq rbp
        ++ movq (imm 0) !%rax
        ++
        (* exit code 0 *)
        ret ++ label "print_int" ++ pushq !%rbp
        ++
        (* assegura, em particular, as questões de alignamento *)
        movq !%rdi !%rsi
        ++ leaq (lab ".Sprint_int") rdi
        ++ movq (imm 0) !%rax
        ++ call "printf" ++ popq rbp ++ ret;
      data =
        Hashtbl.fold
          (fun x _ l -> label x ++ dquad [ 1 ] ++ l)
          genv
          (label ".Sprint_int" ++ string "%d\n");
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  (* "flush" do buffer para assegurar que tudo o que há por gravar foi de facto gravado antes de fecha-lo *)
  fprintf fmt "@?";
  close_out f
