open Lang
open Printf
open Util
open Parserutil
open ExtList
open Eval

(*

open CamomileLibraryDyn.Camomile
open React
open Lwt
open LTerm_style
open LTerm_text
open LTerm_geom
module RL = LTerm_read_line;;

class read_line ~term ~history = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method completion = ()

  initializer self#set_prompt (S.const (eval [B_fg lred; S"> "]))
end;;
*)

let () = Printexc.record_backtrace true;;

module RL = Core_extended.Readline

(*
  let rec loop term history =
  match_lwt
  try_lwt
  lwt command = (new read_line ~term ~history:(LTerm_history.contents history))#run in
  return (Some command)
  with Sys.Break ->
  return None
  with
  | Some command ->
  lwt status =
  try_lwt
  Lwt_process.exec (Lwt_process.shell command)
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
  lwt () = LTerm.printls (eval [B_fg lred; S "command not found"]) in
  return (Unix.WEXITED 127)
  in
  LTerm_history.add history command;
  loop
  term
  history
  | None ->
  loop term history
  ;;
*)

module E = Evalconcrete

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
    
    let astate =
      E.eval_stmt
        (new Stateconcrete.state_empty)
        (fst (Preprocess.preprocess (PSInclude ("library.p2", Lang.PSStmt Lang.SSkip)))) in 

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

              let fs = E.eval_stmt astate astmt in
              
              printf "final state=\n%s\n" (string_indent " " (fs#to_string));

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

    let astate = new Stateconcrete.state_empty in
    let fs = E.eval_stmt astate astmt in

    printf "final state=\n%s\n" (string_indent " " (fs#to_string));
    
    let dump = Library.dump_state_values astate in
    List.iter
      (fun (id, s) -> printf "%s = %s\n" id (Lang.string_of_value s))
      dump
  end
(*
  with
    | e ->
    (*    printf "chdir to %s\n%!" Util.original_dir;
          Unix.chdir Util.original_dir;*)
      printf "%s\n" (Printexc.to_string e);
      Printexc.print_backtrace stdout;
      raise e;;
*)
