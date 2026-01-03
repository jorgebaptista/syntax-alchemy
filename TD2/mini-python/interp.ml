
open Ast
open Format

(* Exceção levantada para sinalizar um erro durante a interpretação. *)
exception Error of string
let error s = raise (Error s)

(* Os valores de Mini-Python

   - uma diferença notável em relação ao Python: aqui utiliza-se o tipo int,
   enquanto em Python os inteiros têm precisão arbitrária; poder-se-ia usar
   o módulo Big_int de OCaml, mas opta-se aqui pela simplicidade;

   - aquilo a que o Python chama lista é, na realidade, um array redimensionável;
   no fragmento considerado aqui, não há possibilidade de alterar o seu comprimento,
   portanto um simples array de OCaml é suficiente.
*)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Exibição de um valor na saída padrão. *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"

(* Interpretação booleana de um valor

Em Python, qualquer valor pode ser usado como um valor booleano: None, 
a lista vazia, a cadeia de caracteres vazia e o inteiro 0 são considerados 
como False, e qualquer outro valor é considerado como True. *)

let is_false (v: value) = 
    match v with
    | Vnone -> true
    | Vbool false -> true
    | Vint 0 -> true
    | Vstring "" -> true
    | Vlist a when Array.length a = 0 -> true
    | _ -> false

let is_true (v: value) =
    not (is_false v)

(* As funções aqui são apenas globais. *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* A instrução return do Python é interpretada com recurso a uma exceção. *)

exception Return of value

(* As variáveis locais (parâmetros de funções e variáveis introduzidas por 
atribuições) são armazenadas numa tabela de dispersão (hash table) passada 
como argumento às funções seguintes sob o nome ctx. *)

type ctx = (string, value) Hashtbl.t

(* Interpretação de uma expressão (devolve um valor). *)

let rec expr (ctx: ctx) = function
  | Ecst Cnone ->
      Vnone
  | Ecst (Cstring s) ->
      Vstring s
  (* arithmética *)
  | Ecst (Cint n) ->
      Vint n
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match op, v1, v2 with
        | Badd, Vint n1, Vint n2 -> Vint (n1 + n2)
        | Bsub, Vint n1, Vint n2 -> Vint (n1 - n2)
        | Bmul, Vint n1, Vint n2 -> Vint (n1 * n2)
        (* Exceção quando se divide por zero*)
        | Bdiv, Vint _, Vint 0 -> error "Division by zero"
        | Bdiv, Vint n1, Vint n2 -> Vint (n1 / n2)
        (* Exceção quando se divide por zero*)
        | Bmod, Vint _, Vint 0 -> error "Division by zero"
        | Bmod, Vint n1, Vint n2 -> Vint (n1 mod n2)
        | Beq, v1, v2  -> Vbool (v1 = v2)
        | Bneq, v1, v2  -> Vbool (v1 <> v2)
        | Blt, v1, v2  -> Vbool (v1 < v2)
        | Ble, v1, v2  -> Vbool (v1 <= v2)
        | Bgt, v1, v2  -> Vbool (v1 > v2)
        | Bge, v1, v2  -> Vbool (v1 >= v2)
        | Badd, Vstring s1, Vstring s2 ->
            Vstring (s1 ^ s2) 
        | Badd, Vlist l1, Vlist l2 ->
            Vlist (Array.append l1 l2)
        | _ -> error "Unsupported operand types"
      end
  | Eunop (Uneg, e1) ->
      begin match expr ctx e1 with
      | Vint n -> Vint (-n)
      | _ -> error "Unsupported operand types"
      end
  (* booleanos *)
  | Ecst (Cbool b) ->
      Vbool b
  | Ebinop (Band, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_false v1 then v1 else expr ctx e2
  | Ebinop (Bor, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_true v1 then v1 else expr ctx e2
  | Eunop (Unot, e1) ->
      Vbool (is_false (expr ctx e1))
  | Eident id ->
      (match Hashtbl.find_opt ctx id with
      | Some v -> v
      | None -> error ("unbound variable " ^ id))
  (* avalia e1 e devolve o tamanho da lista como Vint. Caso contrário, dá erro*)
  | Ecall ("len", [e1]) ->
      begin match expr ctx e1 with
      | Vlist a -> Vint (Array.length a)
      | _ -> error "len expects a list"
      end
  (* constroi [0, 1, 2, ..., n-1] *)
  | Ecall ("list", [Ecall ("range", [e1])]) ->
      begin match expr ctx e1 with
      | Vint n ->
          if n < 0 then error "range expects non-negative integer";
          Vlist (Array.init n (fun i -> Vint i))
      | _ -> error "range expects an integer"
      end
  | Ecall (f, el) ->
      (* procurar a função *)
      let (params, body) =
        match Hashtbl.find_opt functions f with
        | Some def -> def
        | None -> error ("undefined function " ^ f)
      in

      (* verficiar o número de argumentos *)
      if List.length params <> List.length el then
        error ("wrong number of arguments for " ^ f);

      (* avaliar os argumentos no contexto atual *)
      let args = List.map (expr ctx) el in

      (* criar novo contexto local *)
      let ctx' : ctx = Hashtbl.create 16 in
      List.iter2 (fun x v -> Hashtbl.add ctx' x v) params args;

      (* Executar a função *)
      (try
        stmt ctx' body;
        Vnone (* se não houver return *)
      with
      | Return v -> v )(* valor devolvido pela função *)
    
  (* Avalia cada expressão da lista no ctx atual, cria um array com esses valores e devolve Vlist array *)
  | Elist el ->
      Vlist (Array.of_list (List.map (expr ctx) el))

  (* Avalia a lista e o índice, verifica bounds e devolve o elemento *)
  | Eget (e1, e2) ->
      begin match expr ctx e1, expr ctx e2 with
      | Vlist a, Vint i ->
          if i < 0 || i >= Array.length a then error "index out of range";
          a.(i)
      | _ -> error "list index expects (list, int)"
      end

(* Interpretação de uma instrução; não devolve nenhum valor. *)

and stmt (ctx: ctx) = function
  | Seval e ->
      ignore (expr ctx e)
  | Sprint e ->
      print_value (expr ctx e); printf "@."
  | Sblock bl ->
      block ctx bl
  | Sif (e, s1, s2) ->
      if is_true (expr ctx e)
      then stmt ctx s1
      else stmt ctx s2
  | Sassign (id, e1) ->
      let v = expr ctx e1 in
      Hashtbl.replace ctx id v
  | Sreturn e ->
      raise (Return (expr ctx e))
  (* Avalia a lista uma única vez, percorre os elementos e eme cada iteração atualiza x no ctx e executa o corpo s *)
  | Sfor (x, e, s) ->
      begin match expr ctx e with
      | Vlist a ->
          Array.iter (fun v ->
          Hashtbl.replace ctx x v;  (* x recebe cada elemento *)
          stmt ctx s                 (* executa o corpo *)
        ) a
    | _ -> error "for expects a list"
    end
  (* Isto avalia a lista e o índice, avalia o novo valor e escreve no array a(i) <- ... *)
  | Sset (e1, e2, e3) ->
      begin match expr ctx e1, expr ctx e2 with
      | Vlist a, Vint i ->
          if i < 0 || i >= Array.length a then error "index out of range";
          a.(i) <- expr ctx e3
      | _ -> error "list assignment expects (list, int, value)"
      end

(* Interpretação de um bloco, isto é, de uma sequência de instruções. *)

and block (ctx: ctx) = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

(* Interpretação de um ficheiro
- dl é uma lista de definições de funções (ver Ast.def)
- s é uma instrução, que representa as instruções globais.
 *)

let file ((dl: def list), (s: stmt)) =
  (* Regista todas as funções declaradas no ficheiro *)
  List.iter (fun (fname, params, body) ->
    Hashtbl.replace functions fname (params, body)
  ) dl;

  (* Executa o "programa principal" com um contexto vazio *)
  stmt (Hashtbl.create 16) s



