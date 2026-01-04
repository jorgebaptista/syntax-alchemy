
(* Analisador léxico para Arith *)

{
  open Lexing
  open Parser

  exception Lexing_error of char

  let kwd_tbl = [
    "let", LET;
    "in", IN;
    "set", SET;
    "print", PRINT;
    "true", TRUE;
    "false", FALSE;
    "if", IF;
    "then", THEN;
    "else", ELSE;
    "while", WHILE;
    "do", DO;
    "done", DONE;
    "def", DEF;
    "return", RETURN;
    "len", LEN;
    "and", AND;
    "or", OR;
    "not", NOT;
  ]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let integer = ['0'-'9']+
let space = [' ' '\t']

rule token = parse
  | '\n'    { new_line lexbuf; token lexbuf }
  | "#" [^'\n']* '\n' { new_line lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | "=="    { EQEQ }
  | "!="    { NEQ }
  | "<="    { LE }
  | ">="    { GE }
  | '<'     { LT }
  | '>'     { GT }
  | '='     { EQ }
  | '('     { LP }
  | ')'     { RP }
  | '['     { LSQ }
  | ']'     { RSQ }
  | ','     { COMMA }
  | integer as s { CST (int_of_string s) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }
