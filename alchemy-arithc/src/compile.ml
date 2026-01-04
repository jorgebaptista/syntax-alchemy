(* Produção de código para a linguagem Arith *)

open Format
open X86_64
open Ast
module Tc = Typecheck

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

let alloc_temp frame_size =
  let ofs = -(!frame_size + 8) in
  frame_size := !frame_size + 8;
  ofs

let alloc_local frame_size next =
  if !frame_size = next then frame_size := !frame_size + 8;
  (-next - 8, next + 8)

let call_aligned depth label =
  let pad = if depth mod 2 = 0 then 0 else 1 in
  let code_pad = if pad = 1 then subq (imm 8) !%rsp else nop in
  let code_unpad = if pad = 1 then addq (imm 8) !%rsp else nop in
  code_pad ++ call label ++ code_unpad

let fmt_int_label = ".Sprint_int"
let fmt_str_label = ".Sprint_str"
let str_none_label = ".Snone"
let str_lbrack_label = ".Slbrack"
let str_rbrack_label = ".Srbrack"
let str_comma_label = ".Scomma"
let string_labels : (string, string) Hashtbl.t = Hashtbl.create 17
let string_data : (string * string) list ref = ref []

let add_string_literal s =
  match Hashtbl.find_opt string_labels s with
  | Some lbl -> lbl
  | None ->
      let lbl = new_label ".Lstr" in
      Hashtbl.add string_labels s lbl;
      string_data := (lbl, s) :: !string_data;
      lbl

let expr_type types tlocals ~allow_globals expr =
  Tc.canon (Tc.infer_expr types tlocals ~allow_globals expr)

let rec type_id = function
  | Tc.TInt -> "int"
  | Tc.TBool -> "bool"
  | Tc.TString -> "string"
  | Tc.TNone -> "none"
  | Tc.TList t -> "list_" ^ type_id t
  | Tc.TFun _ -> failwith "function value is not supported"
  | Tc.TVar _ -> failwith "ambiguous type"

let runtime_texts : text list ref = ref []
let print_list_needed : (string, unit) Hashtbl.t = Hashtbl.create 17
let compare_list_needed : (string, unit) Hashtbl.t = Hashtbl.create 17

let rec print_label_for_type t =
  match Tc.canon t with
  | Tc.TInt | Tc.TBool -> "print_int"
  | Tc.TString -> "print_string"
  | Tc.TNone -> "print_none"
  | Tc.TList elem -> ensure_print_list elem
  | _ -> failwith "unsupported print type"

and ensure_print_list elem =
  let elem = Tc.canon elem in
  let id = type_id elem in
  let lbl = "print_list_" ^ id in
  if not (Hashtbl.mem print_list_needed id) then begin
    Hashtbl.add print_list_needed id ();
    runtime_texts := gen_print_list elem lbl :: !runtime_texts
  end;
  lbl

and gen_print_list elem lbl =
  let frame = align_frame_size 24 in
  let list_ofs = -8 in
  let len_ofs = -16 in
  let idx_ofs = -24 in
  let lbl_loop = new_label ".Lplist_loop" in
  let lbl_done = new_label ".Lplist_done" in
  let lbl_sep = new_label ".Lplist_sep" in
  let elem_print = print_label_for_type elem in
  label lbl ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ subq (imm frame) !%rsp
  ++ movq !%rdi (ind ~ofs:list_ofs rbp)
  ++ movq (ind rdi) !%rax
  ++ movq !%rax (ind ~ofs:len_ofs rbp)
  ++ movq (imm 0) (ind ~ofs:idx_ofs rbp)
  ++ leaq (lab str_lbrack_label) rdi
  ++ call "print_string"
  ++ movq (ind ~ofs:len_ofs rbp) !%rax
  ++ testq !%rax !%rax ++ jz lbl_done ++ label lbl_loop
  ++ movq (ind ~ofs:idx_ofs rbp) !%rax
  ++ testq !%rax !%rax ++ jz lbl_sep
  ++ leaq (lab str_comma_label) rdi
  ++ call "print_string" ++ label lbl_sep
  ++ movq (ind ~ofs:list_ofs rbp) !%rax
  ++ movq (ind ~ofs:idx_ofs rbp) !%rdi
  ++ movq (ind ~ofs:8 ~index:rdi ~scale:8 rax) !%rdi
  ++ call elem_print
  ++ movq (ind ~ofs:idx_ofs rbp) !%rax
  ++ incq !%rax
  ++ movq !%rax (ind ~ofs:idx_ofs rbp)
  ++ movq (ind ~ofs:idx_ofs rbp) !%rax
  ++ cmpq (ind ~ofs:len_ofs rbp) !%rax
  ++ jl lbl_loop ++ label lbl_done
  ++ leaq (lab str_rbrack_label) rdi
  ++ call "print_string" ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let rec ensure_compare_list elem =
  let elem = Tc.canon elem in
  let id = type_id elem in
  let lbl = "compare_list_" ^ id in
  if not (Hashtbl.mem compare_list_needed id) then begin
    Hashtbl.add compare_list_needed id ();
    runtime_texts := gen_compare_list elem lbl :: !runtime_texts
  end;
  lbl

and gen_compare_list elem lbl =
  let frame = align_frame_size 40 in
  let list1_ofs = -8 in
  let list2_ofs = -16 in
  let len1_ofs = -24 in
  let len2_ofs = -32 in
  let idx_ofs = -40 in
  let lbl_loop = new_label ".Lcmp_loop" in
  let lbl_return = new_label ".Lcmp_ret" in
  let lbl_lt = new_label ".Lcmp_lt" in
  let lbl_gt = new_label ".Lcmp_gt" in
  let lbl_len = new_label ".Lcmp_len" in
  let elem_compare =
    match Tc.canon elem with
    | Tc.TInt | Tc.TBool | Tc.TNone ->
        cmpq !%rdi !%rax ++ jl lbl_lt ++ jg lbl_gt
    | Tc.TString ->
        movq !%rdi !%rsi ++ movq !%rax !%rdi ++ call "strcmp"
        ++ cmpl (imm 0) !%eax
        ++ jl lbl_lt ++ jg lbl_gt
    | Tc.TList inner ->
        let inner_lbl = ensure_compare_list inner in
        movq !%rdi !%rsi ++ movq !%rax !%rdi ++ call inner_lbl
        ++ cmpq (imm 0) !%rax
        ++ jl lbl_lt ++ jg lbl_gt
    | _ -> failwith "unsupported list element compare type"
  in
  label lbl ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ subq (imm frame) !%rsp
  ++ movq !%rdi (ind ~ofs:list1_ofs rbp)
  ++ movq !%rsi (ind ~ofs:list2_ofs rbp)
  ++ movq (ind rdi) !%rax
  ++ movq !%rax (ind ~ofs:len1_ofs rbp)
  ++ movq (ind rsi) !%rax
  ++ movq !%rax (ind ~ofs:len2_ofs rbp)
  ++ movq (imm 0) (ind ~ofs:idx_ofs rbp)
  ++ label lbl_loop
  ++ movq (ind ~ofs:idx_ofs rbp) !%rax
  ++ cmpq (ind ~ofs:len1_ofs rbp) !%rax
  ++ jge lbl_len
  ++ movq (ind ~ofs:idx_ofs rbp) !%rax
  ++ cmpq (ind ~ofs:len2_ofs rbp) !%rax
  ++ jge lbl_len
  ++ movq (ind ~ofs:list1_ofs rbp) !%rax
  ++ movq (ind ~ofs:idx_ofs rbp) !%rdi
  ++ movq (ind ~ofs:8 ~index:rdi ~scale:8 rax) !%rax
  ++ movq (ind ~ofs:list2_ofs rbp) !%rdx
  ++ movq (ind ~ofs:idx_ofs rbp) !%rdi
  ++ movq (ind ~ofs:8 ~index:rdi ~scale:8 rdx) !%rdi
  ++ elem_compare
  ++ movq (ind ~ofs:idx_ofs rbp) !%rax
  ++ incq !%rax
  ++ movq !%rax (ind ~ofs:idx_ofs rbp)
  ++ jmp lbl_loop ++ label lbl_len
  ++ movq (ind ~ofs:len1_ofs rbp) !%rax
  ++ cmpq (ind ~ofs:len2_ofs rbp) !%rax
  ++ jl lbl_lt ++ jg lbl_gt
  ++ movq (imm 0) !%rax
  ++ jmp lbl_return ++ label lbl_lt
  ++ movq (imm (-1)) !%rax
  ++ jmp lbl_return ++ label lbl_gt
  ++ movq (imm 1) !%rax
  ++ label lbl_return ++ movq !%rbp !%rsp ++ popq rbp ++ ret

(* Compilação de uma expressão *)
let compile_expr types ~frame_size ~allow_globals env tlocals next depth expr =
  (* Função recursiva local à compile_expr usada para gerar o código máquina a partir
     da árvore de sintaxe abstracta associada ao valor de tipo Ast.expr.
     No fim da execução deste código, o valor *deve* estar no topo da pilha *)
  let rec comprec env tlocals next depth = function
    | Cst i ->
        (* Push constant value onto stack *)
        pushq (imm i)
    | Bool b ->
        (* Booleans: true = 1, false = 0 *)
        pushq (imm (if b then 1 else 0))
    | Str s ->
        let lbl = add_string_literal s in
        pushq (ilab lbl)
    | NoneLit -> pushq (imm 0)
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
              comprec env tlocals next depth a ++ compile_args (depth + 1) rest
        in
        let code_args = compile_args (depth + pad) args in
        let cleanup =
          if n + pad = 0 then nop else addq (imm (8 * (n + pad))) !%rsp
        in
        code_pad ++ code_args ++ call (func_label f) ++ cleanup ++ pushq !%rax
    | ListLit elems ->
        let n = List.length elems in
        let rec compile_elems depth = function
          | [] -> nop
          | e :: rest ->
              comprec env tlocals next depth e ++ compile_elems (depth + 1) rest
        in
        let code_elems = compile_elems depth elems in
        let pad = if (depth + n) mod 2 = 0 then 0 else 1 in
        let code_pad = if pad = 1 then subq (imm 8) !%rsp else nop in
        let code_unpad = if pad = 1 then addq (imm 8) !%rsp else nop in
        let alloc_bytes = (n + 1) * 8 in
        let code_alloc = movq (imm alloc_bytes) !%rdi ++ call "malloc" in
        let code_len = movq (imm n) (ind rax) in
        let rec store i =
          if i < 0 then nop
          else
            popq rdx ++ movq !%rdx (ind ~ofs:(8 * (i + 1)) rax) ++ store (i - 1)
        in
        code_elems ++ code_pad ++ code_alloc ++ code_unpad ++ code_len
        ++ store (n - 1)
        ++ pushq !%rax
    | ListRange e ->
        let n_ofs = alloc_temp frame_size in
        let list_ofs = alloc_temp frame_size in
        let idx_ofs = alloc_temp frame_size in
        let lbl_loop = new_label ".Lrange_loop" in
        let lbl_end = new_label ".Lrange_end" in
        let pad = if depth mod 2 = 0 then 0 else 1 in
        let code_pad = if pad = 1 then subq (imm 8) !%rsp else nop in
        let code_unpad = if pad = 1 then addq (imm 8) !%rsp else nop in
        comprec env tlocals next depth e
        ++ popq rax
        ++ movq !%rax (ind ~ofs:n_ofs rbp)
        ++ movq (ind ~ofs:n_ofs rbp) !%rdi
        ++ addq (imm 1) !%rdi
        ++ shlq (imm 3) !%rdi
        ++ code_pad ++ call "malloc" ++ code_unpad
        ++ movq !%rax (ind ~ofs:list_ofs rbp)
        ++ movq (ind ~ofs:n_ofs rbp) !%rdx
        ++ movq !%rdx (ind rax)
        ++ movq (imm 0) (ind ~ofs:idx_ofs rbp)
        ++ label lbl_loop
        ++ movq (ind ~ofs:idx_ofs rbp) !%rax
        ++ cmpq (ind ~ofs:n_ofs rbp) !%rax
        ++ jge lbl_end
        ++ movq (ind ~ofs:list_ofs rbp) !%rax
        ++ movq (ind ~ofs:idx_ofs rbp) !%rdi
        ++ movq !%rdi (ind ~ofs:8 ~index:rdi ~scale:8 rax)
        ++ movq (ind ~ofs:idx_ofs rbp) !%rax
        ++ incq !%rax
        ++ movq !%rax (ind ~ofs:idx_ofs rbp)
        ++ jmp lbl_loop ++ label lbl_end
        ++ movq (ind ~ofs:list_ofs rbp) !%rax
        ++ pushq !%rax
    | Get (e_list, e_index) ->
        comprec env tlocals next depth e_list
        ++ comprec env tlocals next (depth + 1) e_index
        ++ popq rdi ++ popq rax
        ++ movq (ind ~ofs:8 ~index:rdi ~scale:8 rax) !%rax
        ++ pushq !%rax
    | Len e ->
        comprec env tlocals next depth e
        ++ popq rax
        ++ movq (ind rax) !%rax
        ++ pushq !%rax
    | Unop (Neg, e) ->
        (* Compile expression, negate result *)
        comprec env tlocals next depth e
        ++ popq rax ++ negq !%rax ++ pushq !%rax
    | Unop (Not, e) ->
        (* Compile expression, logical not: 0 -> 1, nonzero -> 0 *)
        comprec env tlocals next depth e
        ++ popq rax ++ testq !%rax !%rax ++ sete !%al ++ movzbq !%al rax
        ++ pushq !%rax
    | Binop (And, e1, e2) ->
        (* Short-circuit AND: if e1 is false, don't evaluate e2 *)
        let lbl_false = new_label ".Land_false" in
        let lbl_end = new_label ".Land_end" in
        comprec env tlocals next depth e1
        ++ popq rax ++ testq !%rax !%rax ++ jz lbl_false
        ++ comprec env tlocals next depth e2
        ++ popq rax ++ testq !%rax !%rax ++ jz lbl_false
        ++ pushq (imm 1)
        ++ jmp lbl_end ++ label lbl_false
        ++ pushq (imm 0)
        ++ label lbl_end
    | Binop (Or, e1, e2) ->
        (* Short-circuit OR: if e1 is true, don't evaluate e2 *)
        let lbl_true = new_label ".Lor_true" in
        let lbl_end = new_label ".Lor_end" in
        comprec env tlocals next depth e1
        ++ popq rax ++ testq !%rax !%rax ++ jnz lbl_true
        ++ comprec env tlocals next depth e2
        ++ popq rax ++ testq !%rax !%rax ++ jnz lbl_true
        ++ pushq (imm 0)
        ++ jmp lbl_end ++ label lbl_true
        ++ pushq (imm 1)
        ++ label lbl_end
    | Binop (Add, e1, e2) -> (
        let t = expr_type types tlocals ~allow_globals e1 in
        comprec env tlocals next depth e1
        ++ comprec env tlocals next (depth + 1) e2
        ++
        match Tc.canon t with
        | Tc.TInt | Tc.TBool ->
            popq rdi ++ popq rax ++ addq !%rdi !%rax ++ pushq !%rax
        | Tc.TString ->
            popq rsi ++ popq rdi
            ++ call_aligned depth "string_concat"
            ++ pushq !%rax
        | Tc.TList _ ->
            popq rsi ++ popq rdi
            ++ call_aligned depth "list_concat"
            ++ pushq !%rax
        | _ -> assert false)
    | Binop (Sub, e1, e2) ->
        comprec env tlocals next depth e1
        ++ comprec env tlocals next (depth + 1) e2
        ++ popq rdi ++ popq rax ++ subq !%rdi !%rax ++ pushq !%rax
    | Binop (Mul, e1, e2) ->
        comprec env tlocals next depth e1
        ++ comprec env tlocals next (depth + 1) e2
        ++ popq rdi ++ popq rax ++ imulq !%rdi !%rax ++ pushq !%rax
    | Binop (Div, e1, e2) ->
        comprec env tlocals next depth e1
        ++ comprec env tlocals next (depth + 1) e2
        ++ popq rdi ++ popq rax ++ cqto ++ idivq !%rdi ++ pushq !%rax
    | Binop (Mod, e1, e2) ->
        comprec env tlocals next depth e1
        ++ comprec env tlocals next (depth + 1) e2
        ++ popq rdi ++ popq rax ++ cqto ++ idivq !%rdi ++ pushq !%rdx
    | Binop (((Eq | Neq | Lt | Le | Gt | Ge) as op), e1, e2) -> (
        let t = expr_type types tlocals ~allow_globals e1 in
        let setcc =
          match op with
          | Eq -> sete !%al
          | Neq -> setne !%al
          | Lt -> setl !%al
          | Le -> setle !%al
          | Gt -> setg !%al
          | Ge -> setge !%al
          | _ -> assert false
        in
        match Tc.canon t with
        | Tc.TString ->
            comprec env tlocals next depth e1
            ++ comprec env tlocals next (depth + 1) e2
            ++ popq rsi ++ popq rdi
            ++ call_aligned depth "strcmp"
            ++ cmpl (imm 0) !%eax
            ++ setcc ++ movzbq !%al rax ++ pushq !%rax
        | Tc.TList elem ->
            let cmp_lbl = ensure_compare_list elem in
            comprec env tlocals next depth e1
            ++ comprec env tlocals next (depth + 1) e2
            ++ popq rsi ++ popq rdi ++ call_aligned depth cmp_lbl
            ++ cmpq (imm 0) !%rax
            ++ setcc ++ movzbq !%al rax ++ pushq !%rax
        | _ ->
            comprec env tlocals next depth e1
            ++ comprec env tlocals next (depth + 1) e2
            ++ popq rdi ++ popq rax ++ cmpq !%rdi !%rax ++ setcc
            ++ movzbq !%al rax ++ pushq !%rax)
    | Letin (x, e1, e2) ->
        (* Allocate space for local variable if needed *)
        if !frame_size = next then frame_size := 8 + !frame_size;
        let t1 = expr_type types tlocals ~allow_globals e1 in
        (* Compile e1 (result on stack) *)
        comprec env tlocals next depth e1
        ++
        (* Pop result into local variable slot *)
        popq rax
        ++ movq !%rax (ind ~ofs:(-next - 8) rbp)
        ++
        (* Compile e2 with x in environment, pointing to its stack location *)
        comprec
          (StrMap.add x (-next - 8) env)
          (Tc.StrMap.add x t1 tlocals)
          (next + 8) depth e2
    | IfExpr (cond, e1, e2) ->
        (* Compile conditional expression: if cond then e1 else e2 *)
        let lbl_else = new_label ".Lelse" in
        let lbl_end = new_label ".Lendif" in
        comprec env tlocals next depth cond
        ++ popq rax ++ testq !%rax !%rax ++ jz lbl_else
        ++ comprec env tlocals next depth e1
        ++ jmp lbl_end ++ label lbl_else
        ++ comprec env tlocals next depth e2
        ++ label lbl_end
  in
  comprec env tlocals next depth expr

let compile_expr_main types frame_size e =
  compile_expr types ~frame_size ~allow_globals:true StrMap.empty
    Tc.StrMap.empty 0 0 e

let compile_expr_main_depth types frame_size depth e =
  compile_expr types ~frame_size ~allow_globals:true StrMap.empty
    Tc.StrMap.empty 0 depth e

let compile_print types ~frame_size ~allow_globals env tlocals next expr =
  let t = expr_type types tlocals ~allow_globals expr in
  let print_label = print_label_for_type t in
  compile_expr types ~frame_size ~allow_globals env tlocals next 0 expr
  ++ popq rdi ++ call print_label ++ call "print_newline"

(* Compilação de uma instrução (main) *)
let rec compile_instr_main types frame_size = function
  | Set (x, e) ->
      (* Add variable to global environment if not already there *)
      if not (Hashtbl.mem genv x) then Hashtbl.add genv x ();
      (* Compile expression (result will be on stack) *)
      compile_expr_main types frame_size e
      ++
      (* Pop value from stack and store in global variable *)
      popq rax
      ++ movq !%rax (lab x)
  | SetIndex (e_list, e_index, e_value) ->
      compile_expr_main_depth types frame_size 0 e_list
      ++ compile_expr_main_depth types frame_size 1 e_index
      ++ compile_expr_main_depth types frame_size 2 e_value
      ++ popq rdx ++ popq rdi ++ popq rax
      ++ movq !%rdx (ind ~ofs:8 ~index:rdi ~scale:8 rax)
  | Print e ->
      compile_print types ~frame_size ~allow_globals:true StrMap.empty
        Tc.StrMap.empty 0 e
  | Expr e -> compile_expr_main types frame_size e ++ popq rax
  | If (cond, then_stmts, else_stmts) ->
      let lbl_else = new_label ".Lelse" in
      let lbl_end = new_label ".Lendif" in
      (* Compile condition *)
      compile_expr_main types frame_size cond
      ++ popq rax ++ testq !%rax !%rax ++ jz lbl_else
      ++
      (* Then branch *)
      compile_block_main types frame_size then_stmts
      ++ jmp lbl_end
      ++
      (* Else branch *)
      label lbl_else
      ++ compile_block_main types frame_size else_stmts
      ++ label lbl_end
  | While (cond, body) ->
      let lbl_start = new_label ".Lwhile" in
      let lbl_end = new_label ".Lendwhile" in
      (* Loop start *)
      label lbl_start
      ++
      (* Compile condition *)
      compile_expr_main types frame_size cond
      ++ popq rax ++ testq !%rax !%rax ++ jz lbl_end
      ++
      (* Body *)
      compile_block_main types frame_size body
      ++
      (* Jump back to start *)
      jmp lbl_start
      ++
      (* End of loop *)
      label lbl_end
  | For (name, e_list, body) ->
      let lbl_start = new_label ".Lfor" in
      let lbl_end = new_label ".Lendfor" in
      let list_ofs = alloc_temp frame_size in
      let len_ofs = alloc_temp frame_size in
      let idx_ofs = alloc_temp frame_size in
      let list_code = compile_expr_main types frame_size e_list in
      if not (Hashtbl.mem genv name) then Hashtbl.add genv name ();
      list_code ++ popq rax
      ++ movq !%rax (ind ~ofs:list_ofs rbp)
      ++ movq (ind ~ofs:list_ofs rbp) !%rax
      ++ movq (ind rax) !%rax
      ++ movq !%rax (ind ~ofs:len_ofs rbp)
      ++ movq (imm 0) (ind ~ofs:idx_ofs rbp)
      ++ label lbl_start
      ++ movq (ind ~ofs:idx_ofs rbp) !%rax
      ++ cmpq (ind ~ofs:len_ofs rbp) !%rax
      ++ jge lbl_end
      ++ movq (ind ~ofs:list_ofs rbp) !%rax
      ++ movq (ind ~ofs:idx_ofs rbp) !%rdi
      ++ movq (ind ~ofs:8 ~index:rdi ~scale:8 rax) !%rdx
      ++ movq !%rdx (lab name)
      ++ compile_block_main types frame_size body
      ++ movq (ind ~ofs:idx_ofs rbp) !%rax
      ++ incq !%rax
      ++ movq !%rax (ind ~ofs:idx_ofs rbp)
      ++ jmp lbl_start ++ label lbl_end
  | Return _ -> failwith "return used outside of a function"

and compile_block_main types frame_size stmts =
  List.fold_left
    (fun code stmt -> code ++ compile_instr_main types frame_size stmt)
    nop stmts

(* Compilação de uma instrução (função) *)
let rec compile_stmt_fn types frame_size ret_label env tlocals next = function
  | Set (x, e) ->
      let t = expr_type types tlocals ~allow_globals:false e in
      let code =
        compile_expr types ~frame_size ~allow_globals:false env tlocals next 0 e
        ++ popq rax
      in
      let tlocals =
        match Tc.StrMap.find_opt x tlocals with
        | None -> Tc.StrMap.add x t tlocals
        | Some prev ->
            Tc.unify prev t;
            tlocals
      in
      if StrMap.mem x env then
        let offset = StrMap.find x env in
        (code ++ movq !%rax (ind ~ofs:offset rbp), env, tlocals, next)
      else (
        if !frame_size = next then frame_size := 8 + !frame_size;
        let offset = -next - 8 in
        let env = StrMap.add x offset env in
        (code ++ movq !%rax (ind ~ofs:offset rbp), env, tlocals, next + 8))
  | SetIndex (e_list, e_index, e_value) ->
      let code =
        compile_expr types ~frame_size ~allow_globals:false env tlocals next 0
          e_list
        ++ compile_expr types ~frame_size ~allow_globals:false env tlocals next
             1 e_index
        ++ compile_expr types ~frame_size ~allow_globals:false env tlocals next
             2 e_value
        ++ popq rdx ++ popq rdi ++ popq rax
        ++ movq !%rdx (ind ~ofs:8 ~index:rdi ~scale:8 rax)
      in
      (code, env, tlocals, next)
  | Print e ->
      let code =
        compile_print types ~frame_size ~allow_globals:false env tlocals next e
      in
      (code, env, tlocals, next)
  | Expr e ->
      let code =
        compile_expr types ~frame_size ~allow_globals:false env tlocals next 0 e
        ++ popq rax
      in
      (code, env, tlocals, next)
  | Return e ->
      let code =
        compile_expr types ~frame_size ~allow_globals:false env tlocals next 0 e
        ++ popq rax ++ jmp ret_label
      in
      (code, env, tlocals, next)
  | If (cond, then_stmts, else_stmts) ->
      let lbl_else = new_label ".Lelse" in
      let lbl_end = new_label ".Lendif" in
      let cond_code =
        compile_expr types ~frame_size ~allow_globals:false env tlocals next 0
          cond
        ++ popq rax ++ testq !%rax !%rax ++ jz lbl_else
      in
      let then_code, env, tlocals, next =
        compile_block_fn types frame_size ret_label env tlocals next then_stmts
      in
      let else_code, env, tlocals, next =
        compile_block_fn types frame_size ret_label env tlocals next else_stmts
      in
      ( cond_code ++ then_code ++ jmp lbl_end ++ label lbl_else ++ else_code
        ++ label lbl_end,
        env,
        tlocals,
        next )
  | While (cond, body) ->
      let lbl_start = new_label ".Lwhile" in
      let lbl_end = new_label ".Lendwhile" in
      let cond_code =
        compile_expr types ~frame_size ~allow_globals:false env tlocals next 0
          cond
        ++ popq rax ++ testq !%rax !%rax ++ jz lbl_end
      in
      let body_code, env, tlocals, next =
        compile_block_fn types frame_size ret_label env tlocals next body
      in
      ( label lbl_start ++ cond_code ++ body_code ++ jmp lbl_start
        ++ label lbl_end,
        env,
        tlocals,
        next )
  | For (name, e_list, body) ->
      let env_before = env in
      let next0 = next in
      let var_ofs, next1, env_after =
        match StrMap.find_opt name env_before with
        | Some ofs -> (ofs, next0, env_before)
        | None ->
            let ofs, next1 = alloc_local frame_size next0 in
            (ofs, next1, StrMap.add name ofs env_before)
      in
      let tlocals_before = tlocals in
      let list_ty =
        expr_type types tlocals_before ~allow_globals:false e_list
      in
      let elem_ty =
        match Tc.canon list_ty with
        | Tc.TList t -> t
        | _ -> failwith "for expects a list"
      in
      let tlocals_after =
        match Tc.StrMap.find_opt name tlocals_before with
        | None -> Tc.StrMap.add name elem_ty tlocals_before
        | Some prev ->
            Tc.unify prev elem_ty;
            tlocals_before
      in
      let list_ofs, next2 = alloc_local frame_size next1 in
      let len_ofs, next3 = alloc_local frame_size next2 in
      let idx_ofs, next4 = alloc_local frame_size next3 in
      let lbl_start = new_label ".Lfor" in
      let lbl_end = new_label ".Lendfor" in
      let init_code =
        compile_expr types ~frame_size ~allow_globals:false env_before
          tlocals_before next4 0 e_list
        ++ popq rax
        ++ movq !%rax (ind ~ofs:list_ofs rbp)
        ++ movq (ind ~ofs:list_ofs rbp) !%rax
        ++ movq (ind rax) !%rax
        ++ movq !%rax (ind ~ofs:len_ofs rbp)
        ++ movq (imm 0) (ind ~ofs:idx_ofs rbp)
      in
      let body_code, env, tlocals_after, next =
        compile_block_fn types frame_size ret_label env_after tlocals_after
          next4 body
      in
      let loop_code =
        label lbl_start
        ++ movq (ind ~ofs:idx_ofs rbp) !%rax
        ++ cmpq (ind ~ofs:len_ofs rbp) !%rax
        ++ jge lbl_end
        ++ movq (ind ~ofs:list_ofs rbp) !%rax
        ++ movq (ind ~ofs:idx_ofs rbp) !%rdi
        ++ movq (ind ~ofs:8 ~index:rdi ~scale:8 rax) !%rdx
        ++ movq !%rdx (ind ~ofs:var_ofs rbp)
        ++ body_code
        ++ movq (ind ~ofs:idx_ofs rbp) !%rax
        ++ incq !%rax
        ++ movq !%rax (ind ~ofs:idx_ofs rbp)
        ++ jmp lbl_start ++ label lbl_end
      in
      (init_code ++ loop_code, env, tlocals_after, next)

and compile_block_fn types frame_size ret_label env tlocals next stmts =
  List.fold_left
    (fun (code, env, tlocals, next) stmt ->
      let code_stmt, env, tlocals, next =
        compile_stmt_fn types frame_size ret_label env tlocals next stmt
      in
      (code ++ code_stmt, env, tlocals, next))
    (nop, env, tlocals, next) stmts

let compile_function types (name, params, body) =
  let frame_size = ref 0 in
  let ret_label = new_label (".Lret_" ^ name) in
  let nparams = List.length params in
  let fn_ty = Tc.lookup_fun types name in
  let param_tys =
    match fn_ty with Tc.TFun (args, _ret) -> args | _ -> assert false
  in
  let env =
    List.mapi (fun i param -> (param, 16 + (8 * (nparams - i - 1)))) params
    |> List.fold_left
         (fun acc (param, ofs) -> StrMap.add param ofs acc)
         StrMap.empty
  in
  let tlocals =
    List.fold_left2
      (fun acc param ty -> Tc.StrMap.add param ty acc)
      Tc.StrMap.empty params param_tys
  in
  let body_code, _env, _tlocals, _next =
    compile_block_fn types frame_size ret_label env tlocals 0 body
  in
  let size = align_frame_size !frame_size in
  label (func_label name)
  ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ subq (imm size) !%rsp
  ++ body_code
  ++ movq (imm 0) !%rax
  ++ label ret_label ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let print_int_text =
  label "print_int" ++ pushq !%rbp ++ movq !%rsp !%rbp ++ movq !%rdi !%rsi
  ++ leaq (lab fmt_int_label) rdi
  ++ movq (imm 0) !%rax
  ++ call "printf" ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let print_string_text =
  label "print_string" ++ pushq !%rbp ++ movq !%rsp !%rbp ++ movq !%rdi !%rsi
  ++ leaq (lab fmt_str_label) rdi
  ++ movq (imm 0) !%rax
  ++ call "printf" ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let print_none_text =
  label "print_none" ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ leaq (lab str_none_label) rdi
  ++ call "print_string" ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let print_newline_text =
  label "print_newline" ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ movq (imm 10) !%rdi
  ++ call "putchar" ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let string_concat_text =
  let frame = align_frame_size 40 in
  let s1_ofs = -8 in
  let s2_ofs = -16 in
  let len1_ofs = -24 in
  let len2_ofs = -32 in
  let buf_ofs = -40 in
  label "string_concat" ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ subq (imm frame) !%rsp
  ++ movq !%rdi (ind ~ofs:s1_ofs rbp)
  ++ movq !%rsi (ind ~ofs:s2_ofs rbp)
  ++ movq (ind ~ofs:s1_ofs rbp) !%rdi
  ++ call "strlen"
  ++ movq !%rax (ind ~ofs:len1_ofs rbp)
  ++ movq (ind ~ofs:s2_ofs rbp) !%rdi
  ++ call "strlen"
  ++ movq !%rax (ind ~ofs:len2_ofs rbp)
  ++ movq (ind ~ofs:len1_ofs rbp) !%rax
  ++ addq (ind ~ofs:len2_ofs rbp) !%rax
  ++ addq (imm 1) !%rax
  ++ movq !%rax !%rdi ++ call "malloc"
  ++ movq !%rax (ind ~ofs:buf_ofs rbp)
  ++ movq (ind ~ofs:buf_ofs rbp) !%rdi
  ++ movq (ind ~ofs:s1_ofs rbp) !%rsi
  ++ movq (ind ~ofs:len1_ofs rbp) !%rdx
  ++ call "memcpy"
  ++ movq (ind ~ofs:buf_ofs rbp) !%rdi
  ++ movq (ind ~ofs:len1_ofs rbp) !%rax
  ++ addq !%rax !%rdi
  ++ movq (ind ~ofs:s2_ofs rbp) !%rsi
  ++ movq (ind ~ofs:len2_ofs rbp) !%rdx
  ++ call "memcpy"
  ++ movq (ind ~ofs:len1_ofs rbp) !%rdx
  ++ addq (ind ~ofs:len2_ofs rbp) !%rdx
  ++ movq (ind ~ofs:buf_ofs rbp) !%rax
  ++ movb (imm 0) (ind ~index:rdx ~scale:1 rax)
  ++ movq (ind ~ofs:buf_ofs rbp) !%rax
  ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let list_concat_text =
  let frame = align_frame_size 48 in
  let l1_ofs = -8 in
  let l2_ofs = -16 in
  let len1_ofs = -24 in
  let len2_ofs = -32 in
  let total_ofs = -40 in
  let buf_ofs = -48 in
  label "list_concat" ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ subq (imm frame) !%rsp
  ++ movq !%rdi (ind ~ofs:l1_ofs rbp)
  ++ movq !%rsi (ind ~ofs:l2_ofs rbp)
  ++ movq (ind rdi) !%rax
  ++ movq !%rax (ind ~ofs:len1_ofs rbp)
  ++ movq (ind rsi) !%rax
  ++ movq !%rax (ind ~ofs:len2_ofs rbp)
  ++ movq (ind ~ofs:len1_ofs rbp) !%rax
  ++ addq (ind ~ofs:len2_ofs rbp) !%rax
  ++ movq !%rax (ind ~ofs:total_ofs rbp)
  ++ addq (imm 1) !%rax
  ++ shlq (imm 3) !%rax
  ++ movq !%rax !%rdi ++ call "malloc"
  ++ movq !%rax (ind ~ofs:buf_ofs rbp)
  ++ movq (ind ~ofs:buf_ofs rbp) !%rax
  ++ movq (ind ~ofs:total_ofs rbp) !%rdx
  ++ movq !%rdx (ind rax)
  ++ movq (ind ~ofs:buf_ofs rbp) !%rdi
  ++ addq (imm 8) !%rdi
  ++ movq (ind ~ofs:l1_ofs rbp) !%rsi
  ++ addq (imm 8) !%rsi
  ++ movq (ind ~ofs:len1_ofs rbp) !%rdx
  ++ shlq (imm 3) !%rdx
  ++ call "memcpy"
  ++ movq (ind ~ofs:buf_ofs rbp) !%rdi
  ++ movq (ind ~ofs:len1_ofs rbp) !%rax
  ++ shlq (imm 3) !%rax
  ++ addq (imm 8) !%rax
  ++ addq !%rax !%rdi
  ++ movq (ind ~ofs:l2_ofs rbp) !%rsi
  ++ addq (imm 8) !%rsi
  ++ movq (ind ~ofs:len2_ofs rbp) !%rdx
  ++ shlq (imm 3) !%rdx
  ++ call "memcpy"
  ++ movq (ind ~ofs:buf_ofs rbp) !%rax
  ++ movq !%rbp !%rsp ++ popq rbp ++ ret

(* Compila o programa p e grava o código no ficheiro ofile *)
let compile_program types (defs, stmts) ofile =
  (* Reset state for each compilation *)
  label_counter := 0;
  Hashtbl.clear genv;
  Hashtbl.clear string_labels;
  string_data := [];
  Hashtbl.clear print_list_needed;
  Hashtbl.clear compare_list_needed;
  runtime_texts := [];

  let fun_texts = List.map (compile_function types) defs in

  let frame_size = ref 0 in
  let main_code = compile_block_main types frame_size stmts in
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

  let runtime_text =
    List.rev !runtime_texts
    @ [
        print_int_text;
        print_string_text;
        print_none_text;
        print_newline_text;
        string_concat_text;
        list_concat_text;
      ]
  in
  let text =
    List.fold_left ( ++ ) nop (fun_texts @ runtime_text @ [ main_text ])
  in
  let data =
    label fmt_int_label ++ string "%d" ++ label fmt_str_label ++ string "%s"
    ++ label str_none_label ++ string "None" ++ label str_lbrack_label
    ++ string "[" ++ label str_rbrack_label ++ string "]"
    ++ label str_comma_label ++ string ", "
    ++ List.fold_left
         (fun acc (lbl, s) -> acc ++ label lbl ++ string s)
         nop (List.rev !string_data)
    ++ Hashtbl.fold (fun x _ l -> label x ++ dquad [ 1 ] ++ l) genv nop
    ++ inline "\t.section .note.GNU-stack,\"\",@progbits\n"
  in
  let p = { text; data } in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  (* "flush" do buffer para assegurar que tudo o que há por gravar foi de facto gravado antes de fecha-lo *)
  fprintf fmt "@?";
  close_out f
