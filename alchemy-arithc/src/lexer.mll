
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
    "True", TRUE;
    "false", FALSE;
    "False", FALSE;
    "none", NONE;
    "None", NONE;
    "if", IF;
    "then", THEN;
    "else", ELSE;
    "while", WHILE;
    "do", DO;
    "done", DONE;
    "def", DEF;
    "return", RETURN;
    "list", LIST;
    "range", RANGE;
    "len", LEN;
    "for", FOR;
    "and", AND;
    "or", OR;
    "not", NOT;
  ]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let string_buffer = Buffer.create 1024

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let integer = ['0'-'9']+
let space = [' ' '\t']

rule token = parse
  | '\n'    { new_line lexbuf; token lexbuf }
  | '\r'    { token lexbuf }  (* Skip carriage return for Windows compatibility *)
  | "#" [^'\n']* '\n' { new_line lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | "//"    { DIV }
  | '/'     { DIV }
  | '%'     { MOD }
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
  | '"'     { STRING (string lexbuf) }
  | integer as s { CST (int_of_string s) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }

and string = parse
  | '"'     {
      let s = Buffer.contents string_buffer in
      Buffer.reset string_buffer;
      s
    }
  | "\\n"   { Buffer.add_char string_buffer '\n'; string lexbuf }
  | "\\\""  { Buffer.add_char string_buffer '"'; string lexbuf }
  | "\\\\"  { Buffer.add_char string_buffer '\\'; string lexbuf }
  | _ as c  { Buffer.add_char string_buffer c; string lexbuf }
  | eof     { raise (Lexing_error '"') }
