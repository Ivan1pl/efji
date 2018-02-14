{
open Source
open Parser

let identifier buf =
  let s = Lexing.lexeme buf in
  match Reserved.lookup s with
  | Some t -> t
  | None -> IDENTIFIER (Syntax.ident s)

let literal buf =
  INT_LITERAL (int_of_string (Lexing.lexeme buf))

exception Unterminated_comment
}

(* CHAPTER 3: Lexical Structure *)

(* 3.4 Line Terminators *)

let LF = '\n'  (* newline *)
let CR = '\r'  (* return *)

let LineTerminator = LF | CR | CR LF
let InputCharacter = [^ '\r' '\n']

(* 3.5 Input Elements and Tokens *)

let SUB = '\026' (* control-Z *) (* decimal *)

(* 3.6 White Space *)

let SP = ' '     (* space *)
let HT = '\t'    (* horizontal tab *)
let FF = '\012'  (* form feed *) (* decimal *)

let WhiteSpace = SP | HT | FF (* | LineTerminator -- handled separately *)

(* 3.8 Identifiers *)

let Letter = ['A'-'Z' 'a'-'z' '_' '$']
let Digit = ['0'-'9']
let Identifier = Letter (Letter | Digit)*

(* 3.10.1 Integer Literals *)

let IntegerTypeSuffix = ['l' 'L']

let DecimalIntegerLiteral = ('0' | ['1'-'9'] Digit*) IntegerTypeSuffix?

let HexDigit = ['0'-'9' 'a'-'f' 'A'-'F']
let HexIntegerLiteral = '0' ['x' 'X'] HexDigit+ IntegerTypeSuffix?

let OctalDigit = ['0'-'7']
let OctalIntegerLiteral = '0' OctalDigit+ IntegerTypeSuffix?

let IntegerLiteral =
  DecimalIntegerLiteral
| HexIntegerLiteral
| OctalIntegerLiteral

(* 3.10 Literals *)

let Literal =
  IntegerLiteral

rule token = parse
| WhiteSpace
    { token lexbuf }
| LineTerminator
    { next_line lexbuf; token lexbuf }
| Identifier
    { identifier lexbuf }
| Literal
    { literal lexbuf }

(* 3.11 Separators *)
| '('  { LP }
| ')'  { RP }
| '{'  { LC }
| '}'  { RC }
| ';'  { SM }
| ','  { CM }
| '.'  { DOT }
| '?'  { QM }
| ':'  { CL }

(* 3.12 Operators *)
| "="  { EQ }
| ">"  { GT }
| "<"  { LT }
| "!"  { NOT }
| "=="  { EQ_EQ }
| "<="  { LE }
| ">="  { GE }
| "!="  { NOT_EQ }
| "&&"  { AND_AND }
| "||"  { OR_OR }
| "+"  { PLUS }
| "-"  { MINUS }
| "*"  { TIMES }
| "/"  { DIV }
| "%"  { MOD }

| SUB? eof { EOF }
