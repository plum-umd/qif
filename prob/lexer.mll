(* some of it this comes from some ocaml parsing tutorial *)

{
  open Parser
}
let digit = ['0'-'9']
rule token = parse
  | "bool"      { TBOOL }
  | "int" (digit+ as size) { TINT (int_of_string size) }
  | "int"       { TINTDEF }
  | "="         { ASSIGN }
  | "about"     { ABOUT }
  | "output"    { OUTPUT }
  | "to"        { TO }
  | "."         { DOT }
  | ","         { COMMA }
  | "querydef"  { QUERYDEF }
  | "policy"    { POLICY }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "uniform"   { UNIFORM }
  | "#define"   { DEFINE }
  | "#include"  { INCLUDE }
  | "in"        { IN }
  | "secret"    { SECRET }
  | "belief"    { BELIEF }
  | "query"     { QUERY }
  | "and"       { LAND }
  | "or"        { LOR }
  | "->"        { RA }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | "/"         { DIVIDES }
  | "<="        { LEQ }
  | ">="        { GEQ }
  | "<"         { LESSER }
  | ">"         { GREATER }
  | "=="        { EQUAL }
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
  | "-"? digit+ "." digit+ as num
      { FLOAT (float_of_string num) }
  | "-"? digit+ as num
      { INT (int_of_string num) }
  | "\"" (['A'-'Z''a'-'z']['a'-'z''A'-'Z''0'-'9''_''.']* as s) "\""
      { STRING s }
  | ['A'-'Z''a'-'z']['a'-'z''A'-'Z''0'-'9''_']* as id
      { VAR id }
  | [' ' '\t']	{ token lexbuf }
  | ['\n']	{ Lexing.new_line lexbuf; token lexbuf }
  | eof		{ TEOF }
  | '(''*'      { comment lexbuf; token lexbuf }
  | _		{ failwith ("Unrecognized symbol: "^(Lexing.lexeme lexbuf)) }
and comment = parse
    '*'')'	{ () }
  | '(''*'	{ comment lexbuf; comment lexbuf }
  | '\n'	{ Lexing.new_line lexbuf; comment lexbuf }
  | _ 		{ comment lexbuf }
