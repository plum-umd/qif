open Lang
open Printf
open Util
open Parserutil
open ExtList
open Eval

let () = Printexc.record_backtrace true;;

module RL = Core_extended.Readline

module PD = Ppldomainbox;;
module E = Evalabstract.ABSTRACT_EVAL_MAKER(PD);;

let () =
  let infile = ref "-" in
  let seed = ref (-1) in
  Arg.parse [
    ("--seed",
     Arg.Set_int seed,
     "set the random seed, default 0")
  ] (function s -> infile := s) "";
  if !seed >= 0 then Random.init(!seed) else Random.self_init ();
  if !infile = "-" then begin
    let looping = ref true in
    let build_s = ref " " in
    
    let astate = ref 
      (E.eval_stmt (new E.absstate_empty)
         (fst (Preprocess.preprocess (PSInclude ("library.p2", Lang.PSStmt Lang.SSkip))))) in

    while !looping do
      let cmd = RL.input_line
        ~prompt: (if !build_s = " " then "> " else "... ")
        () in
      begin match cmd with
        | Some s ->
          begin
            build_s := !build_s ^ s;
            try
              let apstmt = parse_string !build_s Parser.pstmt_eof in
              let (astmt, _) = Preprocess.preprocess apstmt in

              astate := E.eval_stmt !astate astmt;
              
              printf "final state=\n%s\n" (string_indent " " (!astate#to_string));

              build_s := " "
            with
              | Parse_message msg ->
                if not (!build_s = " ") then (printf "%s%!" msg)
              | Preprocess.Preprocessor_error msg -> begin 
                printf "%s%!" msg;
                build_s := " "
              end
              | e -> 
                printf "%s\n" (Printexc.to_string e);
                Printexc.print_backtrace stdout;
                raise e
          end
        | None ->
          begin
            if !build_s = " " then begin
              looping := false
            end else begin
              printf "abort\n"; flush stdout;
              build_s := " "
            end
          end
      end
    done
  end else begin 
    let apstmt = begin
      try parse !infile Parser.pstmt_eof
      with Parse_error (_, _, _, s) ->
        print_string s; failwith "parse error"
    end in

    let (astmt, _) = Preprocess.preprocess apstmt in

    printf "initial stmt=\n%s\n\n" (string_indent " " (Lang.string_of_stmt astmt));

    let astate = new E.absstate_empty in
    let fs = E.eval_stmt astate astmt in

    printf "final state=\n%s\n" (string_indent " " (fs#to_string));
  end
