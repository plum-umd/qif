open Printf
open Lang
open Parserutil
open Parser
open Ipc
open Netlog
open Eval

module RL = Core_extended.Readline

let command_loop ~root_sref =
(*  let heap = Netmcore_ref.heap root_sref in
    let root = Netmcore_ref.deref_ro root_sref in
    let pool = Netmcore_heap.pool heap in*)

  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> close_in stdin));
  
  let looping = ref true in
  let got_shutdown = ref false in
  let build_s = ref " " in

  let receiver = Ipc.make_receiver_cmd ~root_sref: root_sref in
  let sender_to_master = Ipc.make_sender_to_master ~root_sref: root_sref in

  begin
    try begin
      while !looping do
        let cmd = RL.input_line
          ~prompt: (if !build_s = " " then "> " else "... ")
          ()
        in
        begin match cmd with
        | Some s ->
          begin
            build_s := !build_s ^ s;
            try
              let apstmt = parse_string !build_s Parser.pstmt_eof in
              let (astmt, uicmds) = Preprocess.preprocess apstmt in
              if (List.length uicmds > 0) then begin
                sender_to_master#send (UiCmdsReady uicmds)
              end;
              begin match astmt with
                | SSkip -> ()
                | _ -> let ss = Lang.string_of_stmt astmt in
                       printf "%s\n%!" ss;
                       sender_to_master#send (CmdStmtReady astmt)
              end;
              build_s := " "
            with
              | Parse_message msg ->
                if not (!build_s = " ") then (printf "%s%!" msg)
              | Preprocess.Preprocessor_error msg -> begin 
                printf "%s%!" msg;
                build_s := " "
              end
              | e -> raise e
          end 
        | None ->
          begin
            if !build_s = " " then begin
              looping := false
            end else begin
              printf "abort\n%!";
              build_s := " "
            end
          end
        end;
          
        receiver#process
          (fun msg -> match msg with
            CmdShutdown _ ->
              logf `Info "commander got shotdown message";
              got_shutdown := true;
              looping := false);
      done
    end with Unix.Unix_error _ -> begin
      got_shutdown := true
    end end;

  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> ()));

  if not !got_shutdown then begin
    printf "\n"; flush stdout;
    sender_to_master#send (UiAction (UiQuit true));
    receiver#wait (fun msg -> match msg with
    | CmdShutdown _ -> ()
    (*  | _ -> failwith "shutdown expected"*)
    )
  end
;;

