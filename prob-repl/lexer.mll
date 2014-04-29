(* some of it this comes from some ocaml parsing tutorial *)

{
  open Parser
}
let bindigit = ['0'-'1']
let octdigit = ['0'-'7']
let digit = ['0'-'9']
let hexdigit = ['0'-'9''a'-'f']

rule token = parse
  | "fun"       { FUN }
  | "return"    { RETURN }
  | "to"        { TO }
  | "assert"    { ASSERT }
  | "observe"   { OBSERVE }
  | "type"      { TYPE }
  | "unit"      { TUNIT }
  | "bool"      { TBOOL }
  | "int"       { TINT }
  | "real"      { TREAL }
  | "string"    { TSTRING }
  | "record"    { TRECORD }
  | "=="        { EQUAL }
  | "!="        { NEQUAL }
  | "."         { DOT }
  | ","         { COMMA }
  | "true"      { TRUE }
  | "false"     { FALSE }

  | "#ui"       { UI }
  | "#include"  { INCLUDE }
  | "#define"   { DEFINE }

  | "#reset"     { UIRESET }
  | "#view"      { UIVIEW }
  | "#exit"      { UIEXIT }

  | "uniform"   { UNIFORM }
  | "rand"      { RAND }
  | "rand_int"  { RANDINT }
  | "rand_real" { RANDREAL }
  | "rand_bool" { RANDBOOL }

  | "not"       { LNEG }
  | "and"       { LAND }
  | "&&"        { LAND }
  | "or"        { LOR }
  | "||"        { LOR }

  | "&"         { AAND }
  | "|"         { AOR }
  | "^"         { AXOR }
  | "!"         { ANEG }

  | "->"        { RA }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | "/"         { DIVIDES }
  | "<="        { LEQ }
  | ">="        { GEQ }
  | "<"         { LESSER }
  | ">"         { GREATER }
  | ";"         { SEMICOLON }
  | ":"         { COLON }
  | "skip"      { SKIP }
  | "{"         { LB }
  | "}"         { RB }
  | "("		{ LP }
  | ")"		{ RP }
  | "["         { LSB }
  | "]"         { RSB }
  | "="         { EQUALS }
  | "endif"     { ENDIF }
  | "endpif"    { ENDPIF }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "while"     { WHILE }
  | "endwhile"  { ENDWHILE }
  | "do"        { DO }
  | "pif"       { PIF }
  | "0b" bindigit+ as num { INT (int_of_string num) }
  | "0" octdigit+ as num { INT (int_of_string num) }
  | "0x" hexdigit+ as num { INT (int_of_string num) }
  | "-"? digit+ "." digit+ as num
      { FLOAT (float_of_string num) }
  | "-"? digit+ as num
      { INT (int_of_string num) }
  | "\"" (['a'-'z''A'-'Z''0'-'9''_''.'' ''/']* as s) "\""
      { STRING s }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']* as id
      { IDENT id }
  | ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as id
      { LABEL id }
  | [' ' '\t']	{ token lexbuf }
  | ['\n']	{ Lexing.new_line lexbuf; token lexbuf }
  | eof		{ TEOF }
  | '(''*'      { comment lexbuf; token lexbuf }
  | '/''*'      { comment lexbuf; token lexbuf }
  | _		{ failwith ("Unrecognized symbol: "^(Lexing.lexeme lexbuf)) }
and comment = parse
    | '*'')'	{ () }
    | '*''/'	{ () }
    | '(''*'	{ comment lexbuf; comment lexbuf }
    | '\n'	{ Lexing.new_line lexbuf; comment lexbuf }
    | _ 		{ comment lexbuf }
