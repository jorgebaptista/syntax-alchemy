(* Produção de código para a linguagem Arith (valores dinâmicos) *)

open Format
open X86_64
open Ast

exception VarUndef of string

let label_counter = ref 0

let new_label prefix =
  let n = !label_counter in
  incr label_counter;
  prefix ^ string_of_int n

let func_label name = "fun_" ^ name
let genv : (string, unit) Hashtbl.t = Hashtbl.create 17

module StrMap = Map.Make (String)

let align_frame_size size = if size mod 16 = 8 then size + 8 else size

let alloc_temp frame_size =
  let ofs = -(!frame_size + 8) in
  frame_size := !frame_size + 8;
  ofs

let alloc_local frame_size next =
  if !frame_size = next then frame_size := !frame_size + 8;
  (-next - 8, next + 8)

let tag_mask = 7
let tag_int = 1
let tag_bool = 2
let tag_none = 3
let tag_string = 4
let tag_bool_true = tag_bool lor 8
let tag_bool_false = tag_bool
let fmt_int_label = ".Sprint_int"
let fmt_str_label = ".Sprint_str"
let str_true_label = ".Strue"
let str_false_label = ".Sfalse"
let str_none_label = ".Snone"
let str_lbrack_label = ".Slbrack"
let str_rbrack_label = ".Srbrack"
let str_comma_label = ".Scomma"
let err_div_zero_label = ".Serr_div_zero"
let err_unsupported_label = ".Serr_unsupported"
let err_len_label = ".Serr_len"
let err_range_label = ".Serr_range"
let err_range_neg_label = ".Serr_range_neg"
let err_index_label = ".Serr_index"
let err_index_range_label = ".Serr_index_range"
let err_list_assign_label = ".Serr_list_assign"
let err_for_label = ".Serr_for"
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

let call_aligned depth label =
  let pad = if depth mod 2 = 0 then 0 else 1 in
  let code_pad = if pad = 1 then subq (imm 8) !%rsp else nop in
  let code_unpad = if pad = 1 then addq (imm 8) !%rsp else nop in
  code_pad ++ call label ++ code_unpad

let runtime_error_call depth msg_label =
  leaq (lab msg_label) rdi ++ call_aligned depth "runtime_error"

let check_tag reg tag msg_label depth =
  let lbl_ok = new_label ".Ltag_ok" in
  movq !%reg !%rcx
  ++ andq (imm tag_mask) !%rcx
  ++ cmpq (imm tag) !%rcx
  ++ je lbl_ok
  ++ runtime_error_call depth msg_label
  ++ label lbl_ok

let tag_order reg =
  let lbl_none = new_label ".Ltag_none" in
  let lbl_bool = new_label ".Ltag_bool" in
  let lbl_int = new_label ".Ltag_int" in
  let lbl_str = new_label ".Ltag_str" in
  let lbl_list = new_label ".Ltag_list" in
  let lbl_done = new_label ".Ltag_done" in
  cmpq (imm tag_none) !%reg
  ++ je lbl_none
  ++ cmpq (imm tag_bool) !%reg
  ++ je lbl_bool
  ++ cmpq (imm tag_int) !%reg
  ++ je lbl_int
  ++ cmpq (imm tag_string) !%reg
  ++ je lbl_str
  ++ cmpq (imm 0) !%reg
  ++ je lbl_list
  ++ movq (imm 5) !%reg
  ++ jmp lbl_done ++ label lbl_none
  ++ movq (imm 0) !%reg
  ++ jmp lbl_done ++ label lbl_bool
  ++ movq (imm 1) !%reg
  ++ jmp lbl_done ++ label lbl_int
  ++ movq (imm 2) !%reg
  ++ jmp lbl_done ++ label lbl_str
  ++ movq (imm 3) !%reg
  ++ jmp lbl_done ++ label lbl_list
  ++ movq (imm 4) !%reg
  ++ label lbl_done

let tag_int_rax = shlq (imm 3) !%rax ++ orq (imm tag_int) !%rax
let untag_int_rax = sarq (imm 3) !%rax
let untag_int_rdi = sarq (imm 3) !%rdi
let untag_ptr_rax = andq (imm (-8)) !%rax
let untag_ptr_rdi = andq (imm (-8)) !%rdi
let untag_ptr_rsi = andq (imm (-8)) !%rsi

let push_int i =
  movq (imm i) !%rax
  ++ shlq (imm 3) !%rax
  ++ orq (imm tag_int) !%rax
  ++ pushq !%rax

let push_bool b = pushq (imm (if b then tag_bool_true else tag_bool_false))
let push_none = pushq (imm tag_none)

let push_string s =
  let lbl = add_string_literal s in
  leaq (lab lbl) rdi
  ++ movq !%rdi !%rax
  ++ orq (imm tag_string) !%rax
  ++ pushq !%rax

let rec compile_expr ~frame_size ~allow_globals env next depth expr =
  let rec comprec env next depth = function
    | Cst i -> push_int i
    | Bool b -> push_bool b
    | Str s -> push_string s
    | NoneLit -> push_none
    | Var x ->
        if StrMap.mem x env then
          let offset = StrMap.find x env in
          pushq (ind ~ofs:offset rbp)
        else if allow_globals && Hashtbl.mem genv x then pushq (lab x)
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
    | ListLit elems ->
        let n = List.length elems in
        let rec compile_elems depth = function
          | [] -> nop
          | e :: rest ->
              comprec env next depth e ++ compile_elems (depth + 1) rest
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
        let lbl_nonneg = new_label ".Lrange_nonneg" in
        comprec env next depth e ++ popq rax
        ++ check_tag rax tag_int err_range_label depth
        ++ untag_int_rax
        ++ cmpq (imm 0) !%rax
        ++ jge lbl_nonneg
        ++ runtime_error_call depth err_range_neg_label
        ++ label lbl_nonneg
        ++ movq !%rax (ind ~ofs:n_ofs rbp)
        ++ movq (ind ~ofs:n_ofs rbp) !%rdi
        ++ addq (imm 1) !%rdi
        ++ shlq (imm 3) !%rdi
        ++ call_aligned depth "malloc"
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
        ++ movq !%rdi !%rdx
        ++ shlq (imm 3) !%rdx
        ++ orq (imm tag_int) !%rdx
        ++ movq !%rdx (ind ~ofs:8 ~index:rdi ~scale:8 rax)
        ++ movq (ind ~ofs:idx_ofs rbp) !%rax
        ++ incq !%rax
        ++ movq !%rax (ind ~ofs:idx_ofs rbp)
        ++ jmp lbl_loop ++ label lbl_end
        ++ movq (ind ~ofs:list_ofs rbp) !%rax
        ++ pushq !%rax
    | Get (e_list, e_index) ->
        let lbl_index_ok = new_label ".Lindex_ok" in
        let lbl_index_err = new_label ".Lindex_err" in
        comprec env next depth e_list
        ++ comprec env next (depth + 1) e_index
        ++ popq rdi ++ popq rax
        ++ check_tag rax 0 err_index_label depth
        ++ check_tag rdi tag_int err_index_label depth
        ++ untag_int_rdi ++ untag_ptr_rax
        ++ cmpq (imm 0) !%rdi
        ++ jl lbl_index_err
        ++ movq (ind rax) !%rcx
        ++ cmpq !%rcx !%rdi ++ jge lbl_index_err
        ++ movq (ind ~ofs:8 ~index:rdi ~scale:8 rax) !%rax
        ++ pushq !%rax ++ jmp lbl_index_ok ++ label lbl_index_err
        ++ runtime_error_call depth err_index_range_label
        ++ label lbl_index_ok
    | Len e ->
        comprec env next depth e ++ popq rax
        ++ check_tag rax 0 err_len_label depth
        ++ untag_ptr_rax
        ++ movq (ind rax) !%rax
        ++ tag_int_rax ++ pushq !%rax
    | Unop (Neg, e) ->
        comprec env next depth e ++ popq rax
        ++ check_tag rax tag_int err_unsupported_label depth
        ++ untag_int_rax ++ negq !%rax ++ tag_int_rax ++ pushq !%rax
    | Unop (Not, e) ->
        comprec env next depth e ++ popq rdi
        ++ call_aligned depth "is_true"
        ++ testq !%rax !%rax ++ sete !%al ++ movzbq !%al rax
        ++ shlq (imm 3) !%rax
        ++ orq (imm tag_bool) !%rax
        ++ pushq !%rax
    | Binop (And, e1, e2) ->
        let lbl_false = new_label ".Land_false" in
        let lbl_end = new_label ".Land_end" in
        comprec env next depth e1
        ++ movq (ind rsp) !%rdi
        ++ call_aligned (depth + 1) "is_true"
        ++ testq !%rax !%rax ++ jz lbl_false
        ++ addq (imm 8) !%rsp
        ++ comprec env next depth e2 ++ jmp lbl_end ++ label lbl_false ++ nop
        ++ label lbl_end
    | Binop (Or, e1, e2) ->
        let lbl_true = new_label ".Lor_true" in
        let lbl_end = new_label ".Lor_end" in
        comprec env next depth e1
        ++ movq (ind rsp) !%rdi
        ++ call_aligned (depth + 1) "is_true"
        ++ testq !%rax !%rax ++ jnz lbl_true
        ++ addq (imm 8) !%rsp
        ++ comprec env next depth e2 ++ jmp lbl_end ++ label lbl_true ++ nop
        ++ label lbl_end
    | Binop (Add, e1, e2) ->
        let lbl_int = new_label ".Ladd_int" in
        let lbl_str = new_label ".Ladd_str" in
        let lbl_list = new_label ".Ladd_list" in
        let lbl_tag_ok = new_label ".Ladd_tag_ok" in
        let lbl_done = new_label ".Ladd_done" in
        comprec env next depth e1
        ++ comprec env next (depth + 1) e2
        ++ popq rdi ++ popq rax ++ movq !%rax !%rcx
        ++ andq (imm tag_mask) !%rcx
        ++ movq !%rdi !%rdx
        ++ andq (imm tag_mask) !%rdx
        ++ cmpq !%rdx !%rcx ++ je lbl_tag_ok
        ++ runtime_error_call depth err_unsupported_label
        ++ label lbl_tag_ok
        ++ cmpq (imm tag_int) !%rcx
        ++ je lbl_int
        ++ cmpq (imm tag_string) !%rcx
        ++ je lbl_str
        ++ cmpq (imm 0) !%rcx
        ++ je lbl_list
        ++ runtime_error_call depth err_unsupported_label
        ++ label lbl_int
        ++ check_tag rax tag_int err_unsupported_label depth
        ++ check_tag rdi tag_int err_unsupported_label depth
        ++ untag_int_rax ++ untag_int_rdi ++ addq !%rdi !%rax ++ tag_int_rax
        ++ pushq !%rax ++ jmp lbl_done ++ label lbl_str
        ++ check_tag rax tag_string err_unsupported_label depth
        ++ check_tag rdi tag_string err_unsupported_label depth
        ++ untag_ptr_rax ++ untag_ptr_rdi ++ movq !%rdi !%rsi
        ++ movq !%rax !%rdi
        ++ call_aligned depth "string_concat"
        ++ orq (imm tag_string) !%rax
        ++ pushq !%rax ++ jmp lbl_done ++ label lbl_list
        ++ check_tag rax 0 err_unsupported_label depth
        ++ check_tag rdi 0 err_unsupported_label depth
        ++ movq !%rdi !%rsi ++ movq !%rax !%rdi
        ++ call_aligned depth "list_concat"
        ++ pushq !%rax ++ label lbl_done
    | Binop (Sub, e1, e2) ->
        comprec env next depth e1
        ++ comprec env next (depth + 1) e2
        ++ popq rdi ++ popq rax
        ++ check_tag rax tag_int err_unsupported_label depth
        ++ check_tag rdi tag_int err_unsupported_label depth
        ++ untag_int_rax ++ untag_int_rdi ++ subq !%rdi !%rax ++ tag_int_rax
        ++ pushq !%rax
    | Binop (Mul, e1, e2) ->
        comprec env next depth e1
        ++ comprec env next (depth + 1) e2
        ++ popq rdi ++ popq rax
        ++ check_tag rax tag_int err_unsupported_label depth
        ++ check_tag rdi tag_int err_unsupported_label depth
        ++ untag_int_rax ++ untag_int_rdi ++ imulq !%rdi !%rax ++ tag_int_rax
        ++ pushq !%rax
    | Binop (Div, e1, e2) ->
        let lbl_div_ok = new_label ".Ldiv_ok" in
        comprec env next depth e1
        ++ comprec env next (depth + 1) e2
        ++ popq rdi ++ popq rax
        ++ check_tag rax tag_int err_unsupported_label depth
        ++ check_tag rdi tag_int err_unsupported_label depth
        ++ untag_int_rax ++ untag_int_rdi
        ++ cmpq (imm 0) !%rdi
        ++ jne lbl_div_ok
        ++ runtime_error_call depth err_div_zero_label
        ++ label lbl_div_ok ++ cqto ++ idivq !%rdi ++ tag_int_rax ++ pushq !%rax
    | Binop (Mod, e1, e2) ->
        let lbl_mod_ok = new_label ".Lmod_ok" in
        comprec env next depth e1
        ++ comprec env next (depth + 1) e2
        ++ popq rdi ++ popq rax
        ++ check_tag rax tag_int err_unsupported_label depth
        ++ check_tag rdi tag_int err_unsupported_label depth
        ++ untag_int_rax ++ untag_int_rdi
        ++ cmpq (imm 0) !%rdi
        ++ jne lbl_mod_ok
        ++ runtime_error_call depth err_div_zero_label
        ++ label lbl_mod_ok ++ cqto ++ idivq !%rdi ++ movq !%rdx !%rax
        ++ tag_int_rax ++ pushq !%rax
    | Binop (((Eq | Neq | Lt | Le | Gt | Ge) as op), e1, e2) ->
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
        comprec env next depth e1
        ++ comprec env next (depth + 1) e2
        ++ popq rsi ++ popq rdi
        ++ call_aligned depth "compare_value"
        ++ cmpq (imm 0) !%rax
        ++ setcc ++ movzbq !%al rax
        ++ shlq (imm 3) !%rax
        ++ orq (imm tag_bool) !%rax
        ++ pushq !%rax
    | Letin (x, e1, e2) ->
        if !frame_size = next then frame_size := 8 + !frame_size;
        comprec env next depth e1 ++ popq rax
        ++ movq !%rax (ind ~ofs:(-next - 8) rbp)
        ++ comprec (StrMap.add x (-next - 8) env) (next + 8) depth e2
    | IfExpr (cond, e1, e2) ->
        let lbl_else = new_label ".Lelse" in
        let lbl_end = new_label ".Lendif" in
        comprec env next depth cond
        ++ popq rdi
        ++ call_aligned depth "is_true"
        ++ testq !%rax !%rax ++ jz lbl_else ++ comprec env next depth e1
        ++ jmp lbl_end ++ label lbl_else ++ comprec env next depth e2
        ++ label lbl_end
  in
  comprec env next depth expr

let compile_expr_main frame_size e =
  compile_expr ~frame_size ~allow_globals:true StrMap.empty 0 0 e

let compile_expr_main_depth frame_size depth e =
  compile_expr ~frame_size ~allow_globals:true StrMap.empty 0 depth e

let compile_print ~frame_size ~allow_globals env next expr =
  compile_expr ~frame_size ~allow_globals env next 0 expr
  ++ popq rdi ++ call "print_value" ++ call "print_newline"

let rec compile_instr_main frame_size = function
  | Set (x, e) ->
      if not (Hashtbl.mem genv x) then Hashtbl.add genv x ();
      compile_expr_main frame_size e ++ popq rax ++ movq !%rax (lab x)
  | SetIndex (e_list, e_index, e_value) ->
      let lbl_index_ok = new_label ".Lindex_ok" in
      let lbl_index_err = new_label ".Lindex_err" in
      compile_expr_main_depth frame_size 0 e_list
      ++ compile_expr_main_depth frame_size 1 e_index
      ++ compile_expr_main_depth frame_size 2 e_value
      ++ popq rdx ++ popq rdi ++ popq rax
      ++ check_tag rax 0 err_list_assign_label 0
      ++ check_tag rdi tag_int err_list_assign_label 0
      ++ untag_int_rdi ++ untag_ptr_rax
      ++ cmpq (imm 0) !%rdi
      ++ jl lbl_index_err
      ++ movq (ind rax) !%rcx
      ++ cmpq !%rcx !%rdi ++ jge lbl_index_err
      ++ movq !%rdx (ind ~ofs:8 ~index:rdi ~scale:8 rax)
      ++ jmp lbl_index_ok ++ label lbl_index_err
      ++ runtime_error_call 0 err_index_range_label
      ++ label lbl_index_ok
  | Print e -> compile_print ~frame_size ~allow_globals:true StrMap.empty 0 e
  | Expr e -> compile_expr_main frame_size e ++ popq rax
  | If (cond, then_stmts, else_stmts) ->
      let lbl_else = new_label ".Lelse" in
      let lbl_end = new_label ".Lendif" in
      compile_expr_main frame_size cond
      ++ popq rdi ++ call "is_true" ++ testq !%rax !%rax ++ jz lbl_else
      ++ compile_block_main frame_size then_stmts
      ++ jmp lbl_end ++ label lbl_else
      ++ compile_block_main frame_size else_stmts
      ++ label lbl_end
  | While (cond, body) ->
      let lbl_start = new_label ".Lwhile" in
      let lbl_end = new_label ".Lendwhile" in
      label lbl_start
      ++ compile_expr_main frame_size cond
      ++ popq rdi ++ call "is_true" ++ testq !%rax !%rax ++ jz lbl_end
      ++ compile_block_main frame_size body
      ++ jmp lbl_start ++ label lbl_end
  | For (name, e_list, body) ->
      let lbl_start = new_label ".Lfor" in
      let lbl_end = new_label ".Lendfor" in
      let list_ofs = alloc_temp frame_size in
      let len_ofs = alloc_temp frame_size in
      let idx_ofs = alloc_temp frame_size in
      let list_code = compile_expr_main frame_size e_list in
      if not (Hashtbl.mem genv name) then Hashtbl.add genv name ();
      list_code ++ popq rax
      ++ check_tag rax 0 err_for_label 0
      ++ movq !%rax (ind ~ofs:list_ofs rbp)
      ++ untag_ptr_rax
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
      ++ compile_block_main frame_size body
      ++ movq (ind ~ofs:idx_ofs rbp) !%rax
      ++ incq !%rax
      ++ movq !%rax (ind ~ofs:idx_ofs rbp)
      ++ jmp lbl_start ++ label lbl_end
  | Return _ -> failwith "return used outside of a function"

and compile_block_main frame_size stmts =
  List.fold_left
    (fun code stmt -> code ++ compile_instr_main frame_size stmt)
    nop stmts

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
  | SetIndex (e_list, e_index, e_value) ->
      let lbl_index_ok = new_label ".Lindex_ok" in
      let lbl_index_err = new_label ".Lindex_err" in
      let code =
        compile_expr ~frame_size ~allow_globals:false env next 0 e_list
        ++ compile_expr ~frame_size ~allow_globals:false env next 1 e_index
        ++ compile_expr ~frame_size ~allow_globals:false env next 2 e_value
        ++ popq rdx ++ popq rdi ++ popq rax
        ++ check_tag rax 0 err_list_assign_label 0
        ++ check_tag rdi tag_int err_list_assign_label 0
        ++ untag_int_rdi ++ untag_ptr_rax
        ++ cmpq (imm 0) !%rdi
        ++ jl lbl_index_err
        ++ movq (ind rax) !%rcx
        ++ cmpq !%rcx !%rdi ++ jge lbl_index_err
        ++ movq !%rdx (ind ~ofs:8 ~index:rdi ~scale:8 rax)
        ++ jmp lbl_index_ok ++ label lbl_index_err
        ++ runtime_error_call 0 err_index_range_label
        ++ label lbl_index_ok
      in
      (code, env, next)
  | Print e ->
      let code = compile_print ~frame_size ~allow_globals:false env next e in
      (code, env, next)
  | Expr e ->
      let code =
        compile_expr ~frame_size ~allow_globals:false env next 0 e ++ popq rax
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
        ++ popq rdi ++ call "is_true" ++ testq !%rax !%rax ++ jz lbl_else
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
        ++ popq rdi ++ call "is_true" ++ testq !%rax !%rax ++ jz lbl_end
      in
      let body_code, env, next =
        compile_block_fn frame_size ret_label env next body
      in
      ( label lbl_start ++ cond_code ++ body_code ++ jmp lbl_start
        ++ label lbl_end,
        env,
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
      let list_ofs, next2 = alloc_local frame_size next1 in
      let len_ofs, next3 = alloc_local frame_size next2 in
      let idx_ofs, next4 = alloc_local frame_size next3 in
      let lbl_start = new_label ".Lfor" in
      let lbl_end = new_label ".Lendfor" in
      let init_code =
        compile_expr ~frame_size ~allow_globals:false env_before next4 0 e_list
        ++ popq rax
        ++ check_tag rax 0 err_for_label 0
        ++ movq !%rax (ind ~ofs:list_ofs rbp)
        ++ untag_ptr_rax
        ++ movq (ind rax) !%rax
        ++ movq !%rax (ind ~ofs:len_ofs rbp)
        ++ movq (imm 0) (ind ~ofs:idx_ofs rbp)
      in
      let body_code, env, next =
        compile_block_fn frame_size ret_label env_after next4 body
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
      (init_code ++ loop_code, env, next)

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
    List.mapi (fun i param -> (param, 16 + (8 * (nparams - i - 1)))) params
    |> List.fold_left
         (fun acc (param, ofs) -> StrMap.add param ofs acc)
         StrMap.empty
  in
  let body_code, _env, _next =
    compile_block_fn frame_size ret_label env 0 body
  in
  let size = align_frame_size !frame_size in
  label (func_label name)
  ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ subq (imm size) !%rsp
  ++ body_code
  ++ movq (imm tag_none) !%rax
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

let print_newline_text =
  label "print_newline" ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ movq (imm 10) !%rdi
  ++ call "putchar" ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let runtime_error_text =
  label "runtime_error" ++ pushq !%rbp ++ movq !%rsp !%rbp ++ call "puts"
  ++ movq (imm 1) !%rdi
  ++ call "exit" ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let is_true_text =
  let lbl_int = new_label ".Lis_true_int" in
  let lbl_bool = new_label ".Lis_true_bool" in
  let lbl_none = new_label ".Lis_true_none" in
  let lbl_str = new_label ".Lis_true_str" in
  let lbl_list = new_label ".Lis_true_list" in
  let lbl_end = new_label ".Lis_true_end" in
  label "is_true" ++ pushq !%rbp ++ movq !%rsp !%rbp ++ movq !%rdi !%rax
  ++ andq (imm tag_mask) !%rax
  ++ cmpq (imm tag_int) !%rax
  ++ je lbl_int
  ++ cmpq (imm tag_bool) !%rax
  ++ je lbl_bool
  ++ cmpq (imm tag_none) !%rax
  ++ je lbl_none
  ++ cmpq (imm tag_string) !%rax
  ++ je lbl_str
  ++ cmpq (imm 0) !%rax
  ++ je lbl_list
  ++ movq (imm 0) !%rax
  ++ jmp lbl_end ++ label lbl_int ++ movq !%rdi !%rax ++ untag_int_rax
  ++ cmpq (imm 0) !%rax
  ++ setne !%al ++ movzbq !%al rax ++ jmp lbl_end ++ label lbl_bool
  ++ cmpq (imm tag_bool_true) !%rdi
  ++ sete !%al ++ movzbq !%al rax ++ jmp lbl_end ++ label lbl_none
  ++ movq (imm 0) !%rax
  ++ jmp lbl_end ++ label lbl_str ++ movq !%rdi !%rax ++ untag_ptr_rax
  ++ movzbq (ind rax) rax
  ++ cmpq (imm 0) !%rax
  ++ setne !%al ++ movzbq !%al rax ++ jmp lbl_end ++ label lbl_list
  ++ movq !%rdi !%rax ++ untag_ptr_rax
  ++ movq (ind rax) !%rax
  ++ cmpq (imm 0) !%rax
  ++ setne !%al ++ movzbq !%al rax ++ label lbl_end ++ movq !%rbp !%rsp
  ++ popq rbp ++ ret

let rec compare_value_text =
  let lbl_same = new_label ".Lcmp_same" in
  let lbl_type = new_label ".Lcmp_type" in
  let lbl_end = new_label ".Lcmp_end" in
  let lbl_int = new_label ".Lcmp_int" in
  let lbl_bool = new_label ".Lcmp_bool" in
  let lbl_none = new_label ".Lcmp_none" in
  let lbl_str = new_label ".Lcmp_str" in
  let lbl_list = new_label ".Lcmp_list" in
  let lbl_lt = new_label ".Lcmp_lt" in
  let lbl_gt = new_label ".Lcmp_gt" in
  let lbl_loop = new_label ".Lcmp_loop" in
  let lbl_len = new_label ".Lcmp_len" in
  let frame = align_frame_size 40 in
  let list1_ofs = -8 in
  let list2_ofs = -16 in
  let len1_ofs = -24 in
  let len2_ofs = -32 in
  let idx_ofs = -40 in
  label "compare_value" ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ subq (imm frame) !%rsp
  ++ movq !%rdi !%rax
  ++ andq (imm tag_mask) !%rax
  ++ movq !%rsi !%rdx
  ++ andq (imm tag_mask) !%rdx
  ++ cmpq !%rdx !%rax ++ jne lbl_type ++ label lbl_same
  ++ cmpq (imm tag_int) !%rax
  ++ je lbl_int
  ++ cmpq (imm tag_bool) !%rax
  ++ je lbl_bool
  ++ cmpq (imm tag_none) !%rax
  ++ je lbl_none
  ++ cmpq (imm tag_string) !%rax
  ++ je lbl_str
  ++ cmpq (imm 0) !%rax
  ++ je lbl_list
  ++ movq (imm 0) !%rax
  ++ jmp lbl_end ++ label lbl_type ++ movq !%rax !%rcx ++ tag_order rcx
  ++ movq !%rdx !%r8 ++ tag_order r8 ++ cmpq !%r8 !%rcx ++ jl lbl_lt
  ++ jg lbl_gt ++ label lbl_int ++ movq !%rdi !%rax ++ untag_int_rax
  ++ movq !%rsi !%rcx
  ++ sarq (imm 3) !%rcx
  ++ cmpq !%rcx !%rax ++ jl lbl_lt ++ jg lbl_gt
  ++ movq (imm 0) !%rax
  ++ jmp lbl_end ++ label lbl_bool ++ movq !%rdi !%rax ++ cmpq !%rsi !%rax
  ++ jl lbl_lt ++ jg lbl_gt
  ++ movq (imm 0) !%rax
  ++ jmp lbl_end ++ label lbl_none
  ++ movq (imm 0) !%rax
  ++ jmp lbl_end ++ label lbl_str ++ untag_ptr_rdi ++ untag_ptr_rsi
  ++ call "strcmp" ++ movslq !%eax rax ++ jmp lbl_end ++ label lbl_list
  ++ movq !%rdi (ind ~ofs:list1_ofs rbp)
  ++ movq !%rsi (ind ~ofs:list2_ofs rbp)
  ++ movq !%rdi !%rax ++ untag_ptr_rax ++ movq !%rsi !%rdx ++ untag_ptr_rsi
  ++ movq (ind rax) !%rcx
  ++ movq !%rcx (ind ~ofs:len1_ofs rbp)
  ++ movq (ind rdx) !%r8
  ++ movq !%r8 (ind ~ofs:len2_ofs rbp)
  ++ movq (imm 0) (ind ~ofs:idx_ofs rbp)
  ++ label lbl_loop
  ++ movq (ind ~ofs:idx_ofs rbp) !%r9
  ++ movq (ind ~ofs:len1_ofs rbp) !%rcx
  ++ cmpq !%rcx !%r9 ++ jge lbl_len
  ++ movq (ind ~ofs:len2_ofs rbp) !%r8
  ++ cmpq !%r8 !%r9 ++ jge lbl_len
  ++ movq (ind ~ofs:list1_ofs rbp) !%rax
  ++ untag_ptr_rax
  ++ movq (ind ~ofs:list2_ofs rbp) !%rsi
  ++ untag_ptr_rsi
  ++ movq (ind ~ofs:8 ~index:r9 ~scale:8 rax) !%rdi
  ++ movq (ind ~ofs:8 ~index:r9 ~scale:8 rsi) !%rsi
  ++ call "compare_value"
  ++ cmpq (imm 0) !%rax
  ++ jne lbl_end
  ++ movq (ind ~ofs:idx_ofs rbp) !%r9
  ++ incq !%r9
  ++ movq !%r9 (ind ~ofs:idx_ofs rbp)
  ++ jmp lbl_loop ++ label lbl_len
  ++ movq (ind ~ofs:len1_ofs rbp) !%rcx
  ++ movq (ind ~ofs:len2_ofs rbp) !%r8
  ++ cmpq !%r8 !%rcx ++ jl lbl_lt ++ jg lbl_gt
  ++ movq (imm 0) !%rax
  ++ jmp lbl_end ++ label lbl_lt
  ++ movq (imm (-1)) !%rax
  ++ jmp lbl_end ++ label lbl_gt
  ++ movq (imm 1) !%rax
  ++ label lbl_end ++ movq !%rbp !%rsp ++ popq rbp ++ ret

let print_value_text =
  let lbl_int = new_label ".Lprint_int" in
  let lbl_bool = new_label ".Lprint_bool" in
  let lbl_true = new_label ".Lprint_true" in
  let lbl_false = new_label ".Lprint_false" in
  let lbl_none = new_label ".Lprint_none" in
  let lbl_str = new_label ".Lprint_str" in
  let lbl_list = new_label ".Lprint_list" in
  let lbl_loop = new_label ".Lprint_loop" in
  let lbl_done = new_label ".Lprint_done" in
  let lbl_sep = new_label ".Lprint_sep" in
  let lbl_close = new_label ".Lprint_close" in
  let list_ofs = -8 in
  let len_ofs = -16 in
  let idx_ofs = -24 in
  let frame = align_frame_size 24 in
  label "print_value" ++ pushq !%rbp ++ movq !%rsp !%rbp
  ++ subq (imm frame) !%rsp
  ++ movq !%rdi !%rax
  ++ andq (imm tag_mask) !%rax
  ++ cmpq (imm tag_int) !%rax
  ++ je lbl_int
  ++ cmpq (imm tag_bool) !%rax
  ++ je lbl_bool
  ++ cmpq (imm tag_none) !%rax
  ++ je lbl_none
  ++ cmpq (imm tag_string) !%rax
  ++ je lbl_str
  ++ cmpq (imm 0) !%rax
  ++ je lbl_list ++ jmp lbl_done ++ label lbl_int ++ movq !%rdi !%rax
  ++ untag_int_rax ++ movq !%rax !%rdi ++ call "print_int" ++ jmp lbl_done
  ++ label lbl_bool
  ++ cmpq (imm tag_bool_true) !%rdi
  ++ je lbl_true ++ jmp lbl_false ++ label lbl_true
  ++ leaq (lab str_true_label) rdi
  ++ call "print_string" ++ jmp lbl_done ++ label lbl_false
  ++ leaq (lab str_false_label) rdi
  ++ call "print_string" ++ jmp lbl_done ++ label lbl_none
  ++ leaq (lab str_none_label) rdi
  ++ call "print_string" ++ jmp lbl_done ++ label lbl_str ++ movq !%rdi !%rax
  ++ untag_ptr_rax ++ movq !%rax !%rdi ++ call "print_string" ++ jmp lbl_done
  ++ label lbl_list
  ++ movq !%rdi (ind ~ofs:list_ofs rbp)
  ++ movq !%rdi !%rax ++ untag_ptr_rax
  ++ movq (ind rax) !%rax
  ++ movq !%rax (ind ~ofs:len_ofs rbp)
  ++ movq (imm 0) (ind ~ofs:idx_ofs rbp)
  ++ leaq (lab str_lbrack_label) rdi
  ++ call "print_string"
  ++ movq (ind ~ofs:len_ofs rbp) !%rax
  ++ testq !%rax !%rax ++ jz lbl_close ++ label lbl_loop
  ++ movq (ind ~ofs:idx_ofs rbp) !%rax
  ++ testq !%rax !%rax ++ jz lbl_sep
  ++ leaq (lab str_comma_label) rdi
  ++ call "print_string" ++ label lbl_sep
  ++ movq (ind ~ofs:list_ofs rbp) !%rax
  ++ untag_ptr_rax
  ++ movq (ind ~ofs:idx_ofs rbp) !%rdi
  ++ movq (ind ~ofs:8 ~index:rdi ~scale:8 rax) !%rdi
  ++ call "print_value"
  ++ movq (ind ~ofs:idx_ofs rbp) !%rax
  ++ incq !%rax
  ++ movq !%rax (ind ~ofs:idx_ofs rbp)
  ++ movq (ind ~ofs:idx_ofs rbp) !%rax
  ++ cmpq (ind ~ofs:len_ofs rbp) !%rax
  ++ jl lbl_loop ++ label lbl_close
  ++ leaq (lab str_rbrack_label) rdi
  ++ call "print_string" ++ label lbl_done ++ movq !%rbp !%rsp ++ popq rbp
  ++ ret

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

let compile_program (defs, stmts) ofile =
  label_counter := 0;
  Hashtbl.clear genv;
  Hashtbl.clear string_labels;
  string_data := [];

  let fun_texts = List.map compile_function defs in

  let frame_size = ref 0 in
  let main_code = compile_block_main frame_size stmts in
  let main_frame = align_frame_size !frame_size in
  let main_text =
    globl "main" ++ label "main" ++ pushq !%rbp ++ movq !%rsp !%rbp
    ++ subq (imm main_frame) !%rsp
    ++ main_code ++ movq !%rbp !%rsp ++ popq rbp
    ++ movq (imm 0) !%rax
    ++ ret
  in

  let text =
    List.fold_left ( ++ ) nop
      (fun_texts
      @ [
          print_int_text;
          print_string_text;
          print_newline_text;
          runtime_error_text;
          is_true_text;
          compare_value_text;
          print_value_text;
          string_concat_text;
          list_concat_text;
          main_text;
        ])
  in

  let data =
    label fmt_int_label ++ string "%ld" ++ label fmt_str_label ++ string "%s"
    ++ label str_true_label ++ string "True" ++ label str_false_label
    ++ string "False" ++ label str_none_label ++ string "None"
    ++ label str_lbrack_label ++ string "[" ++ label str_rbrack_label
    ++ string "]" ++ label str_comma_label ++ string ", "
    ++ label err_div_zero_label
    ++ string "error: Division by zero"
    ++ label err_unsupported_label
    ++ string "error: Unsupported operand types"
    ++ label err_len_label
    ++ string "error: len expects a list"
    ++ label err_range_label
    ++ string "error: range expects an integer"
    ++ label err_range_neg_label
    ++ string "error: range expects non-negative integer"
    ++ label err_index_label
    ++ string "error: list index expects (list, int)"
    ++ label err_index_range_label
    ++ string "error: index out of range"
    ++ label err_list_assign_label
    ++ string "error: list assignment expects (list, int, value)"
    ++ label err_for_label
    ++ string "error: for expects a list"
    ++ List.fold_left
         (fun acc (lbl, s) ->
           acc ++ inline "\t.p2align 3\n" ++ label lbl ++ string s)
         nop (List.rev !string_data)
    ++ Hashtbl.fold (fun x _ l -> label x ++ dquad [ tag_none ] ++ l) genv nop
    ++ inline "\t.section .note.GNU-stack,\"\",@progbits\n"
  in

  let p = { text; data } in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
