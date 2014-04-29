open Printf
open ExtString

exception Parse_error of string * int * int * string;;
exception Parse_message of string;;

let currently_parsing = ref ""

let string_of_parse_error e = match e with
  | Parse_message msg -> msg
  | Parse_error (fname, line, cnum, msg) ->
    sprintf "syntax error in %s on line %d character %d"
      fname line cnum
  | e -> raise e

(* http://stackoverflow.com/questions/1933101/ocamlyacc-parse-error-what-token *)
let parse_buf_exn input lexbuf =
  try
    input Lexer.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let fname = !currently_parsing in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      raise (Parse_error
               (fname,
                line,
                cnum,
                (sprintf "syntax error in %s on line %d character %d token %s:\n"
	           fname
	           line
	           cnum
	           tok)))
    end

let parse_string instring input =
  let lexbuf = Lexing.from_string instring in
  try parse_buf_exn input lexbuf
  with Parse_error (fname, lnum, cnum, msg) ->
    let lines = String.nsplit instring "\n" in
    let line = (if List.length lines = 0 then "" else List.nth lines (lnum-1)) in
    raise (Parse_message
             begin
               (sprintf "parse error in %s\n%s\n" fname line) ^
                 (List.fold_left (fun a _ -> " " ^ a) "" (Util.list_range 0 (cnum-1))) ^
                 "^\n"
             end)
;;

let parse_file filename input =
  currently_parsing := filename;
  let data = Std.input_file filename in
  parse_string data input
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
