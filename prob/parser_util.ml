open Printf

exception Parse_error;;

(* http://stackoverflow.com/questions/1933101/ocamlyacc-parse-error-what-token *)
let parse_buf_exn input lexbuf =
  try
    input Lexer.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let fname = !Globals.currently_parsing in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
	printf "syntax error in %s on line %d character %d token %s\n"
	  fname
	  line
	  cnum
	  tok;
	raise Parse_error
    end

let parse_file filename input =
  Globals.currently_parsing := filename;
  let chan = open_in (Globals.file_relative filename) in
  let lexbuf = Lexing.from_channel chan in
  let ret = parse_buf_exn input lexbuf in
    close_in chan;
    ret
;;

let parse_channel chan input =
  let lexbuf = Lexing.from_channel chan in
    parse_buf_exn input lexbuf
;;

let parse infile =
  if (infile = "-") then
    parse_channel stdin
  else
    parse_file infile
