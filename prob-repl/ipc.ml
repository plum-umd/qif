open Util
open Printf
open Netlog
open Option
open ExtList

type domain =
| Boxes
| Octagons
| Polygons

type state_dump = (string * Lang.value) list
type view_dump = float array

type absview_dump = float * float * (float array list list)

type abssample_done_message =
    {abswork_dump: absview_dump}
;;

type sample_done_message = 
    {worker_num: int;
     worker_load: int;
     worker_completed: int;
     worker_accepted: int;
     worker_rejected: int;
     work_dump: view_dump;
     work_weight: float
    }
;;

type work_task =
| WorkerEval of Lang.stmt
(*
  | WorkerEval_samples of Lang.stmt * int
  | WorkerEval_abstract of Lang.stmt * domain
*)

type msg_to_worker =
  | WorkerResendSamples of bool
  | WorkerShutdown of bool
  | WorkerReset of int * int
  | WorkerSetView of Lang.exp * Lang.exp * Lang.exp

type ui_action =
  | UiQuit of bool

type msg_to_ui =
(* note that ipc method here (ocamlbox) doesn't like variants with no data; 
   hence the pointless bools
*)
  | UiDumpReady of sample_done_message
  | UiAbsDumpReady of abssample_done_message
  | UiClear of bool
  | UiShutdown of bool
  | UiCmds of Lang.uicmd list

type msg_to_master =
  | DebugLog of Netlog.level * string
  | UiCmdsReady of Lang.uicmd list
  | UiAction of ui_action
  | UiBoxReady of string
  | UiCleared of bool
  | UiViewChanged of bool
  | CmdPidReady of int
  | WorkerEvalDone of sample_done_message
  | CmdStmtReady of Lang.stmt

type msg_to_cmd =
| CmdShutdown of bool

type shared = {
  mutable workers_samples_load: int array;
  mutable workers_samples_completed: int array;
  mutable workers_samples_accepted: int array;
  mutable workers_samples_rejected: int array;
  mutable wakeup_workers: Netmcore_sem.semaphore array;
  mutable wakeup_cmd: Netmcore_sem.semaphore;
  mutable wakeup_master: string;
  mutable msgs_to_workers: (msg_to_worker, string) Netmcore_queue.squeue_descr array;
  mutable jobs: (work_task, string) Netmcore_queue.squeue_descr array;
  mutable msgs_to_master: (msg_to_master, string) Netmcore_queue.squeue_descr option;
  mutable msgs_to_cmd: (msg_to_cmd, string) Netmcore_queue.squeue_descr option;
  mutable pid_cmd: int;
  mutable num_samples: int;
  mutable box_to_ui_addr: string
}

let num_cores = cpu_count ();;
  
let make_sender_to_cmd ~root_sref =
  let heap = Netmcore_ref.heap root_sref in
  let root = Netmcore_ref.deref_ro root_sref in
  let pool = Netmcore_heap.pool heap in

  let queue = Netmcore_queue.create pool "" in
  let queue_descr = Netmcore_queue.descr_of_squeue queue in

  Netmcore_heap.modify heap
    (fun m -> root.msgs_to_cmd <- Netmcore_heap.add m (Some queue_descr));

  Netmcore_heap.modify heap
    (fun m -> root.wakeup_cmd <- Netmcore_heap.add m (Netmcore_sem.create m 0));

object(self)
  method wake () =
    Netmcore_sem.post root.wakeup_cmd;
    Unix.kill root.pid_cmd Sys.sigint
  method send ~msg =
    Netmcore_queue.push msg queue;
    self#wake ()
  method close () = ()
end
;; 

let make_receiver_cmd ~root_sref =
  let heap = Netmcore_ref.heap root_sref in
  let root = Netmcore_ref.deref_ro root_sref in
  let pool = Netmcore_heap.pool heap in

  Netmcore_heap.modify heap (fun m ->
    root.pid_cmd <- Netmcore_heap.add m (Unix.getpid ()));

  let queue = Netmcore_queue.squeue_of_descr pool (get root.msgs_to_cmd) in

 object (self)
    method sleep () = Netmcore_sem.wait root.wakeup_cmd Netsys_posix.SEM_WAIT_BLOCK

    method process f =
      while not (Netmcore_queue.is_empty queue) do        
        f (Netmcore_queue.pop_c queue)
      done
    method wait f =
      let loop = ref true in
      while !loop do
        self#sleep ();
        while not (Netmcore_queue.is_empty queue) do
          f (Netmcore_queue.pop_c queue);
          loop := false
        done
      done
    method close () = ()
  end;;

let make_receiver_master ~receiver_from_ui ~root_sref =
  let heap = Netmcore_ref.heap root_sref in
  let root = Netmcore_ref.deref_ro root_sref in
  let pool = Netmcore_heap.pool heap in

  let (wakeup_sem, wakeup_sem_addr) = Netsys_posix.sem_create (sprintf "/wakeup_master_%d_" (Unix.getpid ())) 0 in

  Netmcore_heap.modify heap (fun m ->
    root.wakeup_master <- Netmcore_heap.add m wakeup_sem_addr);
  
  let queue = Netmcore_queue.create pool "" in
  let queue_descr = Netmcore_queue.descr_of_squeue queue in

  Netmcore_heap.modify heap (fun m ->
    root.msgs_to_master <- Netmcore_heap.add m (Some queue_descr));
  
object (self)
  method sleep () =
    Netsys_posix.sem_wait wakeup_sem Netsys_posix.SEM_WAIT_BLOCK
  method process f =
    receiver_from_ui#process f;
    while not (Netmcore_queue.is_empty queue) do
      let amsg = (Netmcore_queue.pop_c queue) in f amsg
    done
  method wait f =
    self#sleep ();
    self#process f;
    
  method close () =
    receiver_from_ui#close ()
end
;; 

let make_sender_to_master ~root_sref =
  let heap = Netmcore_ref.heap root_sref in
  let root = Netmcore_ref.deref_ro root_sref in
  let pool = Netmcore_heap.pool heap in

  let queue = Netmcore_queue.squeue_of_descr pool (get root.msgs_to_master) in

  let wakeup_sem = Netsys_posix.sem_open root.wakeup_master [] 0 0 in

object(self)
  method wake () = Netsys_posix.sem_post wakeup_sem
  method send ~msg = 
    Netmcore_queue.push msg queue;
    self#wake ()
  method close () = ()
end
;;

let make_receiver_worker ~root_sref ~num =
  let heap = Netmcore_ref.heap root_sref in
  let root = Netmcore_ref.deref_ro root_sref in
  let pool = Netmcore_heap.pool heap in

  let sem = root.wakeup_workers.(num) in 
  let q_msg = (Netmcore_queue.squeue_of_descr pool root.msgs_to_workers.(num)) in
  let q_job = (Netmcore_queue.squeue_of_descr pool root.jobs.(num)) in object (self)

    method process ~fmsg ~fjob =
      while not (Netmcore_queue.is_empty q_job) do
        let ajob = (Netmcore_queue.pop_c q_job) in fjob ajob
      done;
      while not (Netmcore_queue.is_empty q_msg) do
        let amsg = (Netmcore_queue.pop_c q_msg) in fmsg amsg
      done

    method wait ~fmsg ~fjob =
      self#sleep ();
      self#process ~fmsg: fmsg ~fjob: fjob

    method sleep () = Netmcore_sem.wait sem Netsys_posix.SEM_WAIT_BLOCK

    method close () = ()
  end;;

let make_sender_to_workers ~root_sref =
  let heap = Netmcore_ref.heap root_sref in
  let root = Netmcore_ref.deref_ro root_sref in
  let pool = Netmcore_heap.pool heap in

  let (queues_jobs, queues_msgs, sems) = 
    Netmcore_heap.modify heap (fun m ->
      let queues_jobs = 
        (List.map
           (fun i -> (Netmcore_queue.create pool ""))
           (list_range 0 num_cores)) in
      let queues_msgs =
        (List.map
           (fun i -> (Netmcore_queue.create pool ""))
           (list_range 0 num_cores)) in

      let sems =
        (List.map
           (fun i -> (Netmcore_sem.create m 0))
           (list_range 0 num_cores)) in

      root.jobs <-
        Netmcore_heap.add m
        (Array.of_list (List.map Netmcore_queue.descr_of_squeue queues_jobs));

      root.msgs_to_workers <-
        Netmcore_heap.add m
        (Array.of_list (List.map Netmcore_queue.descr_of_squeue queues_msgs));

      root.wakeup_workers <- Netmcore_heap.add m (Array.of_list sems);
      
      (queues_jobs, queues_msgs, sems)) in object

      method add_job ~job ~num =
        let q_job = List.nth queues_jobs num in
        let sem = List.nth sems num in
        Netmcore_queue.push job q_job;
        Netmcore_sem.post sem
      method add_job_forall ~job = 
        List.iter (fun num ->
          let q_job = List.nth queues_jobs num in
          let sem = List.nth sems num in
          Netmcore_queue.push job q_job;
          Netmcore_sem.post sem
        ) (list_range 0 num_cores)
      method send_all ~msg = 
        List.iter (fun num ->
          let q_msg = List.nth queues_msgs num in
          let sem = List.nth sems num in
          Netmcore_queue.push msg q_msg;
          Netmcore_sem.post sem
        ) (list_range 0 num_cores)
      method send ~msg ~num =
        let q_msg = List.nth queues_msgs num in
        let sem = List.nth sems num in
        Netmcore_queue.push msg q_msg;
        Netmcore_sem.post sem
      method close () = () (* free up the conditions and such *)
    end
;;

class ['a] box_sender box_addr =
  let (box: 'a Netcamlbox.camlbox_sender) = Netcamlbox.camlbox_sender box_addr in object 
    method send (msg: 'a) =
      Netcamlbox.camlbox_send box msg;
    method close () = ()
  end;;

class sender_to_ui x = object inherit [msg_to_ui] box_sender x end;;
class sender_ui_to_master x = object inherit [msg_to_master] box_sender x end;;

let make_sender_ui_to_master ~master_box_addr ~wakeup_sem_addr =
  let wakeup_sem = Netsys_posix.sem_open wakeup_sem_addr [] 0 0 in
  
  let temp = new box_sender master_box_addr in object (self)
    method send msg =
      temp#send msg;
      Netsys_posix.sem_post wakeup_sem
    method close = temp#close
  end;;      

class ['a] box_receiver name = 
  let pid = Unix.getpid () in
  let box_addr = sprintf "%s_%d" name pid in
  let (box: 'a Netcamlbox.camlbox) =
    Netcamlbox.create_camlbox box_addr Config.shared_box_size Config.shared_box_msg_size in object (self)
      method get_addr () = box_addr
      method wait f =
        let message_ids = 
            Netcamlbox.camlbox_wait box in
        List.iter (fun msg_id -> 
          let msg = Netcamlbox.camlbox_get_copy box msg_id in
          Netcamlbox.camlbox_delete box msg_id;
          f msg;
        ) message_ids
      method process f = 
        if Netcamlbox.camlbox_bmessages box > 0 then self#wait f
      method close () =
        Netcamlbox.unlink_camlbox box_addr
    end;;

class receiver_ui = object inherit [msg_to_ui] box_receiver "box_to_ui" end;;  
class receiver_from_ui = object inherit [msg_to_master] box_receiver "box_to_master" end;;

let rec repeat_task atask num =
  if num = 0 then []
  else atask :: (repeat_task atask (num - 1))
;;

let make_task_split tasklist = 
  let share = int_of_float ((float_of_int (List.length tasklist)) /. (float_of_int num_cores)) in
  let remain = ref tasklist in 
  let current_core = ref 0 in
  let ret = ref [] in
  while (List.length !remain) >= share do
    let (ashare, arest) = List.split_nth share !remain in
    ret := (!current_core, ashare) :: !ret;
    remain := arest;
    current_core := (!current_core + 1) mod num_cores;
  done;
  if (List.length !remain) > 0 then
    ret := (!current_core, !remain) :: !ret;
  !ret

let rec split_tasks ~sender_to_workers tasklist = 
  List.iter (fun (acore, tasklist) ->
    List.iter (fun atask -> sender_to_workers#add_job ~job: atask ~num: acore) tasklist)
    (make_task_split tasklist)
;;
