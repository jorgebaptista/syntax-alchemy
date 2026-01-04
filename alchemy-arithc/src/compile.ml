(* Produção de código para a linguagem Arith *)

open Format
open X86_64
open Ast

(* Excepção por lançar quando uma variável (local ou global) não é usada como deve ser *)
exception VarUndef of string

(* Counter for generating unique labels *)
let label_counter = ref 0

let new_label prefix =
  let n = !label_counter in
  incr label_counter;
  prefix ^ string_of_int n

let func_label name = "fun_" ^ name

(* As variáveis globais são arquivadas numa hash table (uma tabela de símbolos globais) *)
let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

(* Usamos uma tabela associativa cujas chaves são variáveis locais, i.e. uma tabela de
   símbolos locais cujas chaves são strings e os valores são a posição desta variável
   relativamente a %rbp (em bytes) *)
module StrMap = Map.Make (String)

let align_frame_size size = if size mod 16 = 8 then size + 8 else size

(* Compilação de uma expressão *)
let compile_expr ~frame_size ~allow_globals env next depth expr =
  (* Função recursiva local à compile_expr usada para gerar o código máquina a partir
     da árvore de sintaxe abstracta associada ao valor de tipo Ast.expr.
     No fim da execução deste código, o valor *deve* estar no topo da pilha *)
  let rec comprec env next depth = function
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
        else if allow_globals && Hashtbl.mem genv x then
          (* Global variable - load from data segment *)
          pushq (lab x)
        else raise (VarUndef x)
    | Call (f, args) ->
        let n = List.length args in
        let pad = if (depth + n) mod 2 = 0 then 0 else 1 in
        let code_pad = if pad = 1 then subq (imm 8) !%rsp else nop in
        let rec compile_args depth = function
          | [] -> nop
          | a :: rest ->
              comprec env next depth a ++ compile_args (depth + 1) rest
        in
        let code_args = compile_args (depth + pad) args in
        let cleanup =
          if n + pad = 0 then nop else addq (imm (8 * (n + pad))) !%rsp
        in
        code_pad ++ code_args ++ call (func_label f) ++ cleanup ++ pushq !%rax
    | Unop (Neg, e) ->
        (* Compile expression, negate result *)
        comprec env next depth e ++ popq rax ++ negq !%rax ++ pushq !%rax
    | Unop (Not, e) ->
        (* Compile expression, logical not: 0 -> 1, nonzero -> 0 *)
        comprec env next depth e ++ popq rax ++ testq !%rax !%rax ++ sete !%al
        ++ movzbq !%al rax ++ pushq !%rax
    | Binop (And, e1, e2) ->
        (* Short-circuit AND: if e1 is false, don't evaluate e2 *)
        let lbl_false = new_label ".Land_false" in
        let lbl_end = new_label ".Land_end" in
        comprec env next depth e1 ++ popq rax ++ testq !%rax !%rax ++ jz lbl_false
        ++ comprec env next depth e2 ++ popq rax ++ testq !%rax !%rax ++ jz lbl_false
        ++ pushq (imm 1)
        ++ jmp lbl_end ++ label lbl_false
        ++ pushq (imm 0)
        ++ label lbl_end
    | Binop (Or, e1, e2) ->
        (* Short-circuit OR: if e1 is true, don't evaluate e2 *)
        let lbl_true = new_label ".Lor_true" in
        let lbl_end = new_label ".Lor_end" in
        comprec env next depth e1 ++ popq rax ++ testq !%rax !%rax ++ jnz lbl_true
        ++ comprec env next depth e2 ++ popq rax ++ testq !%rax !%rax ++ jnz lbl_true
        ++ pushq (imm 0)
        ++ jmp lbl_end ++ label lbl_true
        ++ pushq (imm 1)
        ++ label lbl_end
    | Binop (o, e1, e2) -> (
        (* Compile e1, result on stack *)
        comprec env next depth e1
        ++
        (* Compile e2, result on stack *)
        comprec env next (depth + 1) e2
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
        comprec env next depth e1
        ++
        (* Pop result into local variable slot *)
        popq rax
        ++ movq !%rax (ind ~ofs:(-next - 8) rbp)
        ++
        (* Compile e2 with x in environment, pointing to its stack location *)
        comprec (StrMap.add x (-next - 8) env) (next + 8) depth e2
    | IfExpr (cond, e1, e2) ->
        (* Compile conditional expression: if cond then e1 else e2 *)
        let lbl_else = new_label ".Lelse" in
        let lbl_end = new_label ".Lendif" in
        comprec env next depth cond ++ popq rax ++ testq !%rax !%rax ++ jz lbl_else
        ++ comprec env next depth e1 ++ jmp lbl_end ++ label lbl_else
        ++ comprec env next depth e2 ++ label lbl_end
  in
  comprec env next depth expr

let compile_expr_main frame_size e =
  compile_expr ~frame_size ~allow_globals:true StrMap.empty 0 0 e

(* Compilação de uma instrução (main) *)
let rec compile_instr_main frame_size = function
  | Set (x, e) ->
      (* Add variable to global environment if not already there *)
      if not (Hashtbl.mem genv x) then Hashtbl.add genv x ();
      (* Compile expression (result will be on stack) *)
      compile_expr_main frame_size e
      ++
      (* Pop value from stack and store in global variable *)
      popq rax
      ++ movq !%rax (lab x)
  | Print e ->
      (* Compile expression (result will be on stack) *)
      compile_expr_main frame_size e
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
      compile_expr_main frame_size cond ++ popq rax ++ testq !%rax !%rax
      ++ jz lbl_else
      ++
      (* Then branch *)
      compile_block_main frame_size then_stmts
      ++ jmp lbl_end
      ++
      (* Else branch *)
      label lbl_else
      ++ compile_block_main frame_size else_stmts
      ++ label lbl_end
  | While (cond, body) ->
      let lbl_start = new_label ".Lwhile" in
      let lbl_end = new_label ".Lendwhile" in
      (* Loop start *)
      label lbl_start
      ++
      (* Compile condition *)
      compile_expr_main frame_size cond ++ popq rax ++ testq !%rax !%rax
      ++ jz lbl_end
      ++
      (* Body *)
      compile_block_main frame_size body
      ++
      (* Jump back to start *)
      jmp lbl_start
      ++
      (* End of loop *)
      label lbl_end
  | Return _ -> failwith "return used outside of a function"

and compile_block_main frame_size stmts =
  List.fold_left
    (fun code stmt -> code ++ compile_instr_main frame_size stmt)
    nop stmts

(* Compilação de uma instrução (função) *)
let rec compile_stmt_fn frame_size ret_label env next = function
  | Set (x, e) ->
      let code =
        compile_expr ~frame_size ~allow_globals:false env next 0 e ++ popq rax
      in
      if StrMap.mem x env then
        let offset = StrMap.find x env in
        (code ++ movq !%rax (ind ~ofs:offset rbp), env, next)
      else (
        if !frame_size = next then frame_size := 8 + !frame_size;
        let offset = -next - 8 in
        let env = StrMap.add x offset env in
        (code ++ movq !%rax (ind ~ofs:offset rbp), env, next + 8))
  | Print e ->
      let code =
        compile_expr ~frame_size ~allow_globals:false env next 0 e
        ++ popq rdi ++ call "print_int"
      in
      (code, env, next)
  | Return e ->
      let code =
        compile_expr ~frame_size ~allow_globals:false env next 0 e
        ++ popq rax ++ jmp ret_label
      in
      (code, env, next)
  | If (cond, then_stmts, else_stmts) ->
      let lbl_else = new_label ".Lelse" in
      let lbl_end = new_label ".Lendif" in
      let cond_code =
        compile_expr ~frame_size ~allow_globals:false env next 0 cond
        ++ popq rax ++ testq !%rax !%rax ++ jz lbl_else
      in
      let then_code, env, next =
        compile_block_fn frame_size ret_label env next then_stmts
      in
      let else_code, env, next =
        compile_block_fn frame_size ret_label env next else_stmts
      in
      ( cond_code ++ then_code ++ jmp lbl_end ++ label lbl_else ++ else_code
        ++ label lbl_end,
        env,
        next )
  | While (cond, body) ->
      let lbl_start = new_label ".Lwhile" in
      let lbl_end = new_label ".Lendwhile" in
      let cond_code =
        compile_expr ~frame_size ~allow_globals:false env next 0 cond
        ++ popq rax ++ testq !%rax !%rax ++ jz lbl_end
      in
      let body_code, env, next =
        compile_block_fn frame_size ret_label env next body
      in
      ( label lbl_start ++ cond_code ++ body_code ++ jmp lbl_start ++ label lbl_end,
        env,
        next )

and compile_block_fn frame_size ret_label env next stmts =
  List.fold_left
    (fun (code, env, next) stmt ->
      let code_stmt, env, next =
        compile_stmt_fn frame_size ret_label env next stmt
      in
      (code ++ code_stmt, env, next))
    (nop, env, next) stmts

let compile_function (name, params, body) =
  let frame_size = ref 0 in
  let ret_label = new_label (".Lret_" ^ name) in
  let nparams = List.length params in
  let env =
    List.mapi
      (fun i param -> (param, 16 + (8 * (nparams - i - 1))))
      params
    |> List.fold_left (fun acc (param, ofs) -> StrMap.add param ofs acc)
         StrMap.empty
  in
  let body_code, _env, _next =
    compile_block_fn frame_size ret_label env 0 body
  in
  let size = align_frame_size !frame_size in
  label (func_label name)
  ++ pushq !%rbp ++ movq !%rsp !%rbp ++ subq (imm size) !%rsp
  ++ body_code
  ++ movq (imm 0) !%rax
  ++ label ret_label
  ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let print_int_text =
  label "print_int" ++ pushq !%rbp
  ++
  (* assegura, em particular, as questões de alignamento *)
  movq !%rdi !%rsi
  ++ leaq (lab ".Sprint_int") rdi
  ++ movq (imm 0) !%rax
  ++ call "printf" ++ popq rbp ++ ret

(* Compila o programa p e grava o código no ficheiro ofile *)
let compile_program (defs, stmts) ofile =
  (* Reset state for each compilation *)
  label_counter := 0;
  Hashtbl.clear genv;

  let fun_texts = List.map compile_function defs in

  let frame_size = ref 0 in
  let main_code = compile_block_main frame_size stmts in
  let main_frame = align_frame_size !frame_size in
  let main_text =
    globl "main" ++ label "main"
    ++
    (* Allocate stack frame and set up %rbp *)
    pushq !%rbp ++ movq !%rsp !%rbp
    ++ subq (imm main_frame) !%rsp
    ++ main_code
    ++
    (* Restore stack and return *)
    movq !%rbp !%rsp ++ popq rbp
    ++ movq (imm 0) !%rax
    ++
    (* exit code 0 *)
    ret
  in

  let text =
    List.fold_left ( ++ ) nop (fun_texts @ [ main_text; print_int_text ])
  in
  let data =
    Hashtbl.fold
      (fun x _ l -> label x ++ dquad [ 1 ] ++ l)
      genv
      (label ".Sprint_int" ++ string "%d\n")
  in
  let p = { text; data } in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  (* "flush" do buffer para assegurar que tudo o que há por gravar foi de facto gravado antes de fecha-lo *)
  fprintf fmt "@?";
  close_out f
