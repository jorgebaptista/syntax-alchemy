
(* Ficheiro principal do compilador arithc *)

open Format
open Lexing

(* Opção para a compilação, para parar no fim do parsing *)
let parse_only = ref false

(* Nomes dos ficheiros 'source' e 'target' *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

(* As opções do compilador para mostrar a mensagem iniical quando invocamos 'arithc --help' *)
let options =
  ["-parse-only", Arg.Set parse_only,
   "  parsing stage only";
   "-o", Arg.String (set_file ofile),
   "<file>  output file name"]

let usage = "usage: arithc [option] file.exp"

(* localisar um erro mostrando linha e colun *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
  (* Parsing da linha de comando *)
  Arg.parse options (set_file ifile) usage;

  (* Verifica-se que o nome do ficheiro de saída foi forneceido *)
  if !ifile="" then begin eprintf "No file to compile!\n@?"; exit 1 end;

  (* Este ficheiro tem de ter cono extensão .exp *)
  if not (Filename.check_suffix !ifile ".exp") then begin
    eprintf "The input file should end with .exp\n@?";
    Arg.usage options usage;
    exit 1
  end;

  (* Por omissão, o ficheiro target partilha o nome do ficheiro fonte,
   exeptuando a extensão *)
  if !ofile="" then ofile := Filename.chop_suffix !ifile ".exp" ^ ".s";

  (* Abertura do ficheiro em modo leitura *)
  let f = open_in !ifile in

  (* Criação de um buffer de análise léxica *)
  let buf = Lexing.from_channel f in

  try
    (* Parsing: a função  Parser.prog transforma o buffer de análise léxica
       numa árvore de sintaxe abstrata se nenhum erro (léxico ou sintático)
       for detetado
       A função Lexer.token é usada ple a função Parser.prog para obter o
       próximo token. *)
    let p = Parser.prog Lexer.token buf in
    close_in f;

    (* Paramos aqui se só pretendemos a fase de parsing *)
    if !parse_only then exit 0;

     (* Compilamos a árvore de sintaxe abstrata p.
        O código máquina resultante de desta transformação deve ser
	escrito no ficheiro target  ofile. *)
    Compile.compile_program p !ofile
  with
    | Lexer.Lexing_error c ->
	(* Erro léxico. Recuperamos a posição absoluta e convertêmo-la no formato linha-coluna *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Lexical Error: %c@." c;
	exit 1
    | Parser.Error ->
	(* Erro Sintático. Recuperamos a posição absoluta e convertêmo-la no formato linha-coluna *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Syntax Error@.";
	exit 1
    | Compile.VarUndef s->
	(* Erro derivado de um mau uso de variável durante a compilação *)
	eprintf
	  "Compilation Error: The variable %s is not definded@." s;
	exit 1





