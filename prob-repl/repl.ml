open Netmcore
open Netmcore_ref
open Netmcore_queue
open Printf
open Util
open Ipc

let () =
  let is_gl_proc = ref false in
  let master_box_addr = ref "" in
  let wakeup_sem_addr = ref "" in
  let infile = ref "" in

  Arg.parse [
    ("--uiproc",
     Arg.Set is_gl_proc,
     "INTERNAL; DO NOT USE");
    ("--box_addr",
     Arg.Set_string master_box_addr,
     "INTERNAL; DO NOT USE");
    ("--sem_addr",
     Arg.Set_string wakeup_sem_addr,
     "INTERNAL; DO NOT USE");
  ] (fun s -> infile := s) "";

  if !is_gl_proc then begin
    Ui.run
      ~master_box_addr: !master_box_addr
      ~wakeup_sem_addr: !wakeup_sem_addr
  end else begin
    printf "ppl = %s\n%!" (Pplutil.ppl_version ());
    Master.run ()
  end
