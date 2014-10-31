open Sys
open Benchmark
open Printf
open Filename
open Unix
open Util

let currently_parsing = ref "";;

let precision = ref 0;;
let simplifier = ref 0;;

let use_latte_minmax = ref false;;
let split_uniforms_factor = ref 1;;

let renderlatex = ref false;;

let simplify_steps = ref 0;;

let use_dsa = ref false;;
let output_verbose = ref false;;
let output_debug   = ref false;;
let output_bench   = ref false;;
let output_bench_latte = ref false;;

let current_executable = Sys.argv.(0);;
let current_dir = Filename.dirname current_executable;;
let original_dir = Unix.getcwd ();;

let file_relative f = (* relative to starting envionment *)
  if Filename.is_relative f then
    original_dir ^ Filename.dir_sep ^ f
  else
    f
;;

let file_abs f = (* assuming f is specified relative to new environment *)
  if Filename.is_relative f then
    (Unix.getcwd ()) ^ Filename.dir_sep ^ f
  else 
    f

let do_ifverbose f = 
  if !output_verbose then f ();;
let do_ifdebug f =
  if !output_debug then f ();;
let do_ifbench f =
  if !output_bench then f ();;
let do_ifbench_latte f =
  if !output_bench_latte then f ();;

(* latte timing recording *)
let chan_bench_latte = ref Pervasives.stdout;;
let set_bench_latte s =
  chan_bench_latte := (open_out (file_relative s));;
let timer_bench_latte = new timer;;
let bench_latte_out_header () =
  do_ifbench_latte (fun () ->
		      fprintf !chan_bench_latte "type,dimensions,constraints,time (s),time (real s)\n";
		      flush !chan_bench_latte);;
let bench_latte_start () =
  do_ifbench_latte (fun () -> ignore (timer_bench_latte#mark));;
let bench_latte_end k d c =
  do_ifbench_latte (fun () ->
		      let (t, tr) = timer_bench_latte#mark in
			fprintf !chan_bench_latte "%s,%d,%d,%f,%f\n" k d c t tr;
			flush !chan_bench_latte);;
let bench_latte_close () =
  do_ifbench_latte (fun () -> close_out !chan_bench_latte);;
(* end of latte timing recording *)

let max_complexity = ref 0;;
let seen_complexity a = if a > !max_complexity then max_complexity := a;;

let b = new bench;;

let new_record = b#new_record;;
let inc_record = b#inc_record;;
let inc_val_record = b#inc_val_record;;
let max_record = b#max_record;;
let set_record = b#set_record;;
let new_timer = b#new_timer;;
let start_timer = b#start_timer;;
let stop_timer = b#stop_timer;;
let mark_epoch = fun() -> b#mark_epoch;;
let next_epoch = fun() -> b#next_epoch;;
let print_header = fun () -> b#print_header;;
let print_epoch = fun () -> b#print_epoch;;
let set_bench s = b#set_bench (file_relative s);;
let close_bench () = b#close;;

let timer_count     = "latte count";;
let timer_maximize  = "latte maximize";;
let timer_simplify  = "simplify";;
let timer_query     = "query";;
let record_vertices = "vertices";;

new_timer timer_count;;
new_timer timer_maximize;;
new_timer timer_simplify;;
new_timer timer_query;;
new_record record_vertices;;

let _memoize_common name =
  let record_hit = "memoize " ^ name ^ " hit" in
  let record_fault = "memoize " ^ name ^ " fault" in  
  let h = Hashtbl.create 256 in
    new_record record_hit "0";
    new_record record_fault "0";
    ((fun () -> inc_record record_hit),
     (fun () -> inc_record record_fault),
     h)
;;

let memoize_named1 name f =
  let bench_hit, bench_fault, h = _memoize_common name in
    fun k ->
    try
      let temp = Hashtbl.find h k in
	bench_hit ();
	temp
    with
	Not_found ->
	  (let temp = f k
	   in
	     Hashtbl.replace h k temp;
	     bench_fault ();
	     temp)
;;

let memoize_named2 name f =
  curry2 (memoize_named1 name (uncurry2 f))
;;

Unix.chdir current_dir;;
