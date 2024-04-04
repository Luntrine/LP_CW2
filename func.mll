{ (* Tokens in the func language. *)
  type token =
    | METHOD | ENDMETHOD 
    | BEGIN | END 
    | VAR | VARS 
    | IF | THEN | ELSE | ENDIF 
    | WHILE | ENDWHILE 
    | READ | WRITE | RETURN
    | PLUS | MINUS | TIMES | DIVIDE
    | LESS | LESSEQ | EQ | NEQ 
    | LBRA | RBRA
    | COMMA | SEMI
    | ID of string | INT of int 
    | ASSIGN
    | EOF
  exception LEX of string
}

(* These are all to help parse the tokens for IDs, white space and Integers. *)
let white = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let nondigit = ['a'-'z' 'A'-'Z' '_']
let integer = digit+
let identifier = nondigit (nondigit | digit)*

(* This is to parse the func code into a list of tokens to be operated on. *)
rule token = parse
  | white {token lexbuf}
  | "method" {METHOD} | "endmethod" {ENDMETHOD}
  | "begin" {BEGIN} | "end" {END}
  | "var" {VAR} | "vars" {VARS}
  | "if" {IF} | "then" {THEN} | "else" {ELSE} | "endif" {ENDIF}
  | "while" {WHILE} | "endwhile" {ENDWHILE}
  | "read" {READ} | "write" {WRITE} | "return" {RETURN}
  | "+" {PLUS} | "-" {MINUS} | "*" {TIMES} | "/" {DIVIDE}
  | "lessEq" {LESSEQ} | "less" {LESS} | "nEq" {NEQ} | "eq" {EQ}
  | "(" {LBRA} | ")" {RBRA} 
  | "," {COMMA} | ";" {SEMI}
  | identifier {ID(Lexing.lexeme lexbuf)} | integer {INT(int_of_string (Lexing.lexeme lexbuf))}
  | ":=" {ASSIGN}
  | eof {EOF}
  | _ {raise (LEX ("Unexpected char: " ^ Lexing.lexeme lexbuf))}