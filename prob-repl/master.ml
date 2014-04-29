open Printf
open Util
open Ipc
open Netlog
open Std

let worker_fork, worker_join = 
  Netmcore_process.def_process
    (fun (num, pool, root_descr) ->
      let root_sref = Netmcore_ref.sref_of_descr pool root_descr in
      (*      let heap = Netmcore_ref.heap root_sref in
              let root = Netmcore_ref.deref_ro root_sref in*)
      
      (*let `Process pid = Netmcore.self_process_id () in*)

      Random.self_init ();

      Worker.work_loop ~num: num ~root_sref: root_sref;
    )
;;

let cmd_fork, cmd_join =
  Netmcore_process.def_process
    (fun (pool, root_descr) ->
      let root_sref = Netmcore_ref.sref_of_descr pool root_descr in
      (*      let heap = Netmcore_ref.heap root_sref in
              let root = Netmcore_ref.deref_ro root_sref in*)
      
      let `Process id = Netmcore.self_process_id () in
      let pid = Unix.getpid () in

      logf `Info "cmd is %d/%d" id pid;

      Cmd.command_loop ~root_sref: root_sref;
    )
;;

let foreach_worker root f = 
  List.iter (fun num ->
    let q_jobs = root.jobs.(num) in
    f q_jobs
  ) Config.list_cores;;

let process_debug_actions msg = match msg with
  | DebugLog (l, s) -> logf l "%s" s
  | _ -> failwith
    ("master got unexpected message: " ^ (dump msg))

let master_loop ~receiver ~sender_to_workers ~sender_to_ui ~sender_to_cmd =
  let looping = ref true in
  let current_stmt = ref None in

  while !looping do
    receiver#wait (fun msg -> 
      match msg with
        | UiAction (UiQuit _) -> looping := false
        | UiViewChanged _ ->
          sender_to_workers#send_all ~msg: (WorkerResendSamples true)
        | UiCleared _ ->
          begin match !current_stmt with
            | Some astmt -> begin 
              sender_to_workers#add_job_forall
                ~job: (WorkerEval astmt);
              current_stmt := None
            end
            | None -> sender_to_workers#send_all ~msg: (WorkerResendSamples true)
          end 
        | WorkerEvalDone adump ->
          sender_to_ui#send (UiDumpReady adump)
        | CmdStmtReady astmt ->
(*          logf `Info "stmt ready";*)
          sender_to_ui#send (UiClear true);
          current_stmt := Some astmt
        | UiCmdsReady alist ->
          List.iter (fun acmd -> match acmd with
            | Lang.UiReset (numsamples, splitrate)  ->
(*              logf `Info "sending reset";*)
              Evalabstract.split_rate := splitrate;
              (*logf `Info "split rate = %d" !Evalabstract.split_rate;
                logf `Info "number of samples = %d" numsamples;*)
              sender_to_ui#send (UiCmds [acmd]);
              sender_to_workers#send_all ~msg: (WorkerReset ((numsamples / Config.num_cores), splitrate))
            | Lang.UiView (f1, f2, f3) ->
(*              logf `Info "sending view";*)
              sender_to_ui#send (UiCmds [acmd]);
              sender_to_workers#send_all ~msg: (WorkerSetView (f1, f2, f3))
            | Lang.UiExit -> looping := false
          ) alist;
        | _ -> process_debug_actions msg
    )

  done;;

let master_fork, master_join =
  Netmcore_process.def_process
    (fun pool ->
      (* starting workers *)
      let root_sref: shared Netmcore_ref.sref = 
        Netmcore_ref.sref pool
          {workers_samples_load = Array.create Config.num_cores 0;
           workers_samples_completed = Array.create Config.num_cores 0;
           workers_samples_accepted = Array.create Config.num_cores 0;
           workers_samples_rejected = Array.create Config.num_cores 0;
           wakeup_workers = [||];
           wakeup_cmd = Netmcore_sem.dummy ();
           wakeup_master = "";
           msgs_to_master = None;
           msgs_to_cmd = None;
           jobs = [||];
           msgs_to_workers = [||];
           pid_cmd = 0;
           num_samples = 100;
           box_to_ui_addr = ""
          } in

      let heap = Netmcore_ref.heap root_sref in
      let root_descr = Netmcore_ref.descr_of_sref root_sref in
      let (root: shared) = Netmcore_ref.deref_ro root_sref in

      let receiver_from_ui = new Ipc.receiver_from_ui in

      let receiver = Ipc.make_receiver_master
        ~receiver_from_ui: receiver_from_ui
        ~root_sref: root_sref in

      (* starting the ui process *)
      (*let fd_ui_read, fd_master_write = Unix.pipe () in
        let fd_master_read, fd_ui_write = Unix.pipe () in*)

      let ui_pid = Netsys_posix.spawn
        ~pg: Netsys_posix.Pg_keep
        ~fd_actions: []
(*        ~fd_actions: [Netsys_posix.Fda_dup2 (fd_ui_write, Unix.stdout);
                      Netsys_posix.Fda_dup2 (fd_ui_read, Unix.stdin);
                      Netsys_posix.Fda_close fd_ui_read;
                      Netsys_posix.Fda_close fd_ui_write;
                      Netsys_posix.Fda_close fd_master_read;
                      Netsys_posix.Fda_close fd_master_write;
                     ]*)
        "repl"
        (Array.of_list ["";
                        "--uiproc";
                        "--box_addr"; (receiver_from_ui#get_addr ());
                        "--sem_addr"; root.wakeup_master;
                       ]) in

      (*Unix.close fd_ui_read;
        Unix.close fd_ui_write;*)

      (*
        let ch_from_ui = Unix.in_channel_of_descr fd_master_read in
      *)

      let box_to_ui_addr = ref "" in
      receiver_from_ui#wait (fun msg ->
        match msg with
        | UiBoxReady addr -> box_to_ui_addr := addr
        | _ -> process_debug_actions msg
      );

      Netmcore_heap.modify heap (fun m ->
        root.box_to_ui_addr <- Netmcore_heap.add m !box_to_ui_addr);

      let sender_to_ui = new Ipc.sender_to_ui !box_to_ui_addr in
      let sender_to_workers = Ipc.make_sender_to_workers ~root_sref: root_sref in

      let workers = 
        List.map
          (fun num ->
            Netmcore_process.start
              ~inherit_resources: (`Resources [pool])
              worker_fork (num, pool, root_descr)
          )
          Config.list_cores
      in

      (* starting command reader *)
      let proc_cmd =
        Netmcore_process.start
          ~inherit_resources: (`Resources [pool])
          cmd_fork (pool, root_descr) in

      let sender_to_cmd = Ipc.make_sender_to_cmd
        ~root_sref: root_sref in

      (* master loop *)
      master_loop
        ~receiver: receiver
        ~sender_to_ui: sender_to_ui
        ~sender_to_workers: sender_to_workers
        ~sender_to_cmd: sender_to_cmd;

      logf `Notice "shutting down";

      (* exiting *)
      sender_to_cmd#send (CmdShutdown true);
      sender_to_ui#send (UiShutdown true);
      sender_to_workers#send_all (WorkerShutdown true);

      ignore (Netmcore_process.join cmd_join proc_cmd);

      List.iter
        (fun process_id ->
          let `Process pid = process_id in
          let result = Netmcore_process.join worker_join process_id in
          match result with
          | Some res -> ()
          | None -> logf `Err "worker %d failed" pid)
        workers;
      
      ignore (Unix.waitpid [] ui_pid);

      receiver#close()
    )
;;

let run () =
  logf `Notice "starting, number of cores = %d" Config.num_cores;
  let pool = Netmcore_mempool.create_mempool Config.shared_pool_size in

  begin try let (total, free, cont) = Netmcore_mempool.stats pool in
            logf `Info "shared pool: %d of %d free" free total
    with _ -> failwith "failed to allocate shared pool"
  end;

  (*let shm_name = (Netmcore_mempool.shm_name pool) in*)

  Netmcore.startup
    ~socket_directory:"."
    ~first_process:
    (fun () -> Netmcore_process.start
      ~inherit_resources: (`Resources [pool])
      master_fork pool)
    ();

  Netmcore.release pool

