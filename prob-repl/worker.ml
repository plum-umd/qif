open Printf
open Ipc
open Std
open Netlog
open Eval

module EC = Evalconcrete;;

module PD = Ppldomainbox;;
module EA = Evalabstract.ABSTRACT_EVAL_MAKER(PD);;

let view = Array.of_list [Lang.EIdent "x";
                          Lang.EIdent "y";
                          Lang.EIdent "z"];;

let get_library fname =
  try fst (Preprocess.preprocess (Lang.PSInclude (fname, Lang.PSStmt Lang.SSkip)))
  with e -> failwith (sprintf "cannot load library %s: %s" fname (Printexc.to_string e))
;;

let init_sample lib = EC.eval_stmt (new EC.state_empty) lib;;

let num_samples = ref (Config.initial_workload / Config.num_cores);;

let work_loop ~num ~root_sref =
  let receiver = Ipc.make_receiver_worker ~root_sref: root_sref ~num: num in
  (*  let sender = Ipc.make_sender_to_master ~root_sref: root_sref in*)

  let root = Netmcore_ref.deref_ro root_sref in
  (*let heap = Netmcore_ref.heap root_sref in*)

  let sender_to_ui = new Ipc.sender_to_ui root.box_to_ui_addr in

  let load = ref 0 in
  let completed = ref 0 in
  let accepted = ref 0 in
  let rejected = ref 0 in

  (*let set_info () =
    Netmcore_heap.modify heap (fun m ->
    root.workers_samples_load.(num) <- Netmcore_heap.add m !load;
    root.workers_samples_completed.(num) <- Netmcore_heap.add m !completed;
    root.workers_samples_accepted.(num) <- Netmcore_heap.add m !accepted;
    root.workers_samples_rejected.(num) <- Netmcore_heap.add m !rejected;
    ); () in
  *)

  let work = ref true in

  let lib = get_library "library.p2" in

  let samples = ref [] in

  let is_linear = ref true in
  let absstate = ref (EA.eval_stmt (new EA.absstate_empty) lib) in

  let history_stmt = ref Lang.SSkip in

  (*
    let eval_wrap astate astmt =
    try `Evalgood (EC.eval_stmt astate astmt)
    with
    | Eval_error s ->
    logf `Err "eval error: %s" s;
    `Evalerror
    | Lang.Type_error s ->
    logf `Err "type error: %s" s;
    `Evalerror
    | Assertion_failed -> 
    `Evalbad in
  *)

  let send_dump adump =
    sender_to_ui#send (UiDumpReady {worker_num= num;
                                    worker_load= !load;
                                    worker_completed= !completed;
                                    worker_accepted= !accepted;
                                    worker_rejected= !rejected;
                                    work_dump= adump;
                                    work_weight= 1.0
                                   }) in

    let send_samples () = 
      List.iter (fun astate -> send_dump (astate#dump_view view)) !samples in

    let send_abstract_dump () =
      let dumpl = !absstate#dump_view view in
      (*logf `Info "sending abstract dumps";*)
      List.iter (fun shape ->
        (* logf `Info "shape:"; List.iter (fun face -> logf `Info "
           face:"; List.iter (fun coord -> logf `Info " coord: %s"
           (String.concat ", " (List.map string_of_float
           (Array.to_list coord))) ) face) shape; *)
        sender_to_ui#send (UiAbsDumpReady {abswork_dump = shape})) dumpl in

    let resend_all_dumps () =
      send_samples ();
      send_abstract_dump () in

    while !work do
      receiver#wait
        ~fmsg: begin fun msg -> match msg with
          | WorkerResendSamples _ ->
            send_samples ();
            if !is_linear && num = 0 then send_abstract_dump ()

          | WorkerSetView (f1, f2, f3) ->

            view.(0) <- f1;
            view.(1) <- f2;
            view.(2) <- f3

          | WorkerShutdown _ -> work := false

          | WorkerReset (anum, splitrate) ->
            is_linear := true;
            Evalabstract.split_rate := splitrate;
            absstate := (EA.eval_stmt (new EA.absstate_empty) lib);
            
            history_stmt := Lang.SSkip;
            num_samples := anum;
            
            samples := List.map
              (fun _ -> init_sample lib)
              (Util.list_range 0 !num_samples)
        end

        ~fjob: begin fun job -> match job with
          | WorkerEval astmt ->

            let new_history_stmt = Lang.SSeq (!history_stmt, astmt) in
            
            load := !num_samples;

            accepted := 0;
            rejected := 0;
            completed := 0;

            if num = 0 && !is_linear then begin
              try
                absstate := EA.eval_stmt (!absstate)#copy astmt;
                begin match new_history_stmt with
                  | Lang.SSkip -> ()
                  | _ -> send_abstract_dump ()
                end
              with
                | Librarylinear.Non_linear _ -> is_linear := false
                | Lang.Type_error s ->
                  if num = 0 then logf `Err "type error: %s" s;
                  resend_all_dumps()
                | Eval_error s ->
                  if num = 0 then logf `Err "eval error: %s" s;
                  resend_all_dumps()
                | Undefined_variable s ->
                  if num = 0 then logf `Err "undefined variable: %s" s;
                  resend_all_dumps()
            end;
            
            (*set_info ();*)

            try begin
              let temp_samples = ref (
                List.flatten
                  (List.map
                     (fun astate ->
                       let temp = begin
                         try 
                           let temp = astate#copy in
                           (*let temp = astate in*)
                           completed := !completed + 1;
                           ignore (Evalconcrete.eval_stmt temp astmt);
                           accepted := !accepted + 1;

                           begin match new_history_stmt with
                             | Lang.SSkip -> ()
                             | _ -> send_dump (temp#dump_view view)
                           end; 

                           [temp]
                         with Assertion_failed ->
                           rejected := !rejected + 1;
                           []
                       end in
                       (* set_info (); *)
                       temp
                     ) !samples)) in
              
              while List.length !temp_samples < !num_samples do
                let astate = init_sample lib in begin
                  try
                    completed := !completed + 1;
                    ignore (Evalconcrete.eval_stmt astate new_history_stmt);
                    accepted := !accepted + 1;
                    begin match new_history_stmt with
                      | Lang.SSkip -> ()
                      | _ -> send_dump (astate#dump_view view) 
                    end; 
                    temp_samples := astate :: !temp_samples
                  with Assertion_failed -> rejected := !rejected + 1
                end;
              (*set_info ()*)
              done;

              history_stmt := new_history_stmt;
              samples := !temp_samples;

            end with
              | Lang.Type_error s ->
                if num = 0 && not !is_linear then logf `Err "type error: %s" s;
                resend_all_dumps()
              | Eval_error s ->
                if num = 0 && not !is_linear then logf `Err "eval error: %s" s;
                resend_all_dumps()
              | Undefined_variable s ->
                if num = 0 && not !is_linear then logf `Err "undefined variable: %s" s;
                resend_all_dumps()
        end
    done;

    receiver#close ()

;;
