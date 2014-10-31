open Gmp
open Gmp_util
open Globals
open Util
open Printf
open Unix
open Ppl_ocaml
open Ppl_util
open Maths
open Geo

let latte_bin = Globals.file_abs "install/latte-for-tea-too-1.2-mk-0.9.3/dest/bin/count"
let latte_bin_max = Globals.file_abs "install/latte-for-tea-too-1.2-mk-0.9.3/dest/bin/maximize"
let latte_bin_max_b = Globals.file_abs "install/latte-for-tea-too-1.2-mk-0.9.3/dest/bin/maximize bbs"
let latte_tmp = Globals.file_abs "latte_tmp"

let latte_workdir = (latte_tmp ^ "/" ^ (string_of_int (Unix.getpid ())));;

(*let latte_env = Array.create 1 ("PWD=" ^ latte_workdir);;*)
(*let latte_env = Array.create 0 "";;*)
let latte_env = Unix.environment ();;

if Sys.file_exists latte_workdir then
  ignore (Unix.system ("rm -Rf " ^ latte_workdir));;
Unix.mkdir latte_workdir 0o700;;
Unix.chdir latte_workdir;;

type latte = string
type lspec = (Z.t list) list

(* let rand_tmp_name = fun () -> "model_" ^ (string_of_int (Random.bits ()));; *)
let rand_tmp_name = fun () -> "model_" ^ (string_of_int (Unix.getpid ()));;
let rand_tmp_name2 = fun () -> "model_max_" ^ (string_of_int (Unix.getpid ()));;

let rec_count_total = "latte count constraints total";;
let rec_count_max   = "latte count constraints max";;
let rec_max_total   = "latte maximize constraints total";;
let rec_max_max     = "latte maximize constraints max";;
let rec_latte_total = "latte constraints total";;
let rec_latte_max   = "latte constraints max";;

Globals.new_record rec_count_total "0";;
Globals.new_record rec_count_max   "0";;
Globals.new_record rec_max_total   "0";;
Globals.new_record rec_max_max     "0";;
Globals.new_record rec_latte_total "0";;
Globals.new_record rec_latte_max   "0";;

let _latte_size =
  let regex_first = Str.regexp "^\\([0-9]+?\\) \\([0-9]+?\\)" in
    fun alatte ->  
      if not (Str.string_match regex_first alatte 0) then
	(
	  printf "latte=\"%s\n" alatte;
	  raise (General_error "couldn't find num of constraints/dimensions in latte input")
	);
      let temp_conses = (Str.matched_group 1 alatte) in
      let temp_dims = (Str.matched_group 2 alatte) in
	(int_of_string temp_conses, (int_of_string temp_dims) - 1)
;;

exception Count_return of Z.t;;

let _count_models =
  let rexp_count = Str.regexp "The number of lattice points is \\([0-9]+\\)\\." in
  let rexp_num = Str.regexp "[0-9]+" in

    fun (alatte: latte) ->
      (* latte seems to have 2 different means of reporting the count, at times
	 it reports just the count to stdout but other times the count only shows up
	 at the end of the stderr along with some text
	 
	 actually looks like there is a third output means, this one outputs
	 two lines to stdout, the second of which is the actual count
      *)
      let (ns, ds) = _latte_size alatte in
	ifbench (
	  Globals.inc_val_record rec_count_total ns;
	  Globals.inc_val_record rec_latte_total ns;
	  Globals.max_record rec_count_max ns;
	  Globals.max_record rec_latte_max ns
	);
	Globals.bench_latte_start ();

	let filename = latte_workdir ^ "/" ^ (rand_tmp_name ()) in
	  write_file filename alatte;
	  ifdebug
	    (printf "counting models (latte) ... ";
	     flush Pervasives.stdout);
	  ifbench (Globals.start_timer Globals.timer_count);

	  let (data_in, data_err) = exec_and_read_all (latte_bin ^ " " ^ filename) latte_env in

	    ifdebug (printf "done "; flush Pervasives.stdout);
	    ifbench (Globals.stop_timer Globals.timer_count;
		     Globals.bench_latte_end "count" ds ns);

	    let lines_in = string_split data_in "\n" in

	      try
		if List.length lines_in = 0 then
		  (
		    if (string_search rexp_count data_err) then
		      let matched = Str.matched_group 1 data_err in
			raise (Count_return (Z.from_string matched))
		  );
		
		let temp = (List.hd (List.rev lines_in)) in
		  if Str.string_match rexp_num temp 0 then
		    raise (Count_return (Z.from_string temp));
		  
		  printf "filename = %s\n" filename;
		  printf "data_in = \"%s\"\n" data_in;
		  printf "----\n\n\n\ndata_err = \"%s\"\n" data_err;
		  raise (General_error "new latte count output")
	      with | Count_return r -> r

    (*
    let (chan_in, chan_out, chan_err) =
      open_process_full
	(latte_bin ^ " " ^ filename)
	latte_env in
      try 
	let line = input_line chan_in in
	let (lline, alllines_temp) = _chan_last_line chan_in in
	  (* alllines := alllines_temp;*)
	let cline = (if (String.compare "" lline) == 0 then line else lline) in
	  ignore (close_process_full (chan_in, chan_out, chan_err));
	  let temp = Z.from_string cline in
	    if (String.compare (Z.to_string temp) (cline) != 0) then
	      raise (General_error ("count_models: bad latte output received: [" ^ cline ^ "] vs [" ^ (Z.to_string temp) ^ "]"))
	    else
	      temp
      with
	| End_of_file -> 
	    let (line, alllines_err) = _chan_last_line chan_err in
	      ignore (close_process_full (chan_in, chan_out, chan_err));
	      let rexp = Str.regexp "The number of lattice points is \\([0-9]+\\)\\." in
		if Str.string_match rexp line 0 then
		  let matched = Str.matched_group 1 line in
		    Z.from_string matched
		else
		  (printf "latte failed for %s :\n" filename;
		   print_string alatte;
		   printf "--- all stdout lines ---\n";
		   print_string !alllines;
		   printf "\n--- all stderr lines ---\n";
		   print_string alllines_err;
		   print_string "\n--- end all lines ---\n";
		   raise (General_error "couldn't understand latte output")
		  )) in
    ret
    *)
;;

let count_models =
  Globals.memoize_named1
    "count"
    _count_models
;;

let _maximize_models_bin =
  let rexp1 = Str.regexp ".*^The optimal value is: \\([0-9]+\\)\\." in
  let rexp2 = Str.regexp ".*^The optimal value: \\([0-9]+\\)" in

    fun lbin (alatte: latte) (le: latte) ->
      (* when maximizing, latte seems to output a whole bunch of things to stdout, among the lines is
	 
	 The optimal value is: X
      *)
      let (ns, ds) = _latte_size alatte in
	Globals.bench_latte_start ();
	ifbench (
	  Globals.inc_val_record rec_max_total ns;
	  Globals.inc_val_record rec_latte_total ns;
	  Globals.max_record rec_max_max ns;
	  Globals.max_record rec_latte_max ns
  );
  (*  let ret = ( *)
  let filename = latte_workdir ^ "/" ^ (rand_tmp_name2 ()) in
  let filename_cost = filename ^ ".cost" in
    write_file filename alatte;
    write_file filename_cost le;

      ifdebug (printf "maximizing (latte) ...";
	       flush Pervasives.stdout);
      ifbench (Globals.start_timer Globals.timer_maximize);

    let (data_in, data_err) = exec_and_read_all (lbin ^ " " ^ filename) latte_env in

    ifdebug (printf "done "; flush Pervasives.stdout);
      ifbench (Globals.stop_timer Globals.timer_maximize;
	       Globals.bench_latte_end "maximize" ds ns);
    
      if (string_search rexp1 data_err) || (string_search rexp2 data_err) then (
	let matched = Str.matched_group 1 data_err in
	  Z.from_string matched)
      else (
	  printf "data_in = \"%s\"\n" data_in;
	  printf "----\n\n\n\ndata_err = \"%s\"\n" data_err;
	  raise (General_error "new latte maximize output")
	)
;;

let _maximize_models (alatte: latte) (le: latte) : Z.t =
  let a1 = _maximize_models_bin latte_bin_max alatte le in a1;;

(*  let a2 = _maximize_models_bin latte_bin_max_b alatte le in a2;;
    this one keeps giving bad answers
*)

    (*    if ((Random.float 1.0) >= 0.5) &&
	  (a2 >=! zone)
	  then 
	  (printf "!!!!!!!!!!!!!!!!!\n";
	  a2 -! (Z.from_int 42))
	  else
	  a2
    *)

(*
  if not (Z.equal a1 a2) then raise
  (General_error ("maximize returned different answers, my pid is "
  ^ (string_of_int (Unix.getpid ()))));
  let filename = latte_workdir ^ "/" ^ (rand_tmp_name2 ()) in
  let filename_cost = filename ^ ".cost" in
  Unix.unlink filename;
  Unix.unlink filename_cost;
  a1*)

let maximize_models =
  Globals.memoize_named2
    "maximize"
    _maximize_models
;;

let latte_of_poly p =
  let cs = constraints_rid_equals (ppl_Polyhedron_get_minimized_constraints p) in
  let num_cs = List.length cs in
  let num_dim = ppl_Polyhedron_space_dimension p in
  let lspec = List.map (halfspace_of_constraint num_dim) cs in
    (string_of_int num_cs) ^
      " " ^ (string_of_int (num_dim + 1)) ^
      "\n" ^ (string_of_halfspaces lspec) ^ "\n"
;;

let latte_of_le dim le = 
  (string_of_int 1)
  ^ " "
  ^ (string_of_int dim)
  ^ "\n"
  ^ (string_of_halfspaces [Array.of_list (List.tl (Array.to_list (halfspace_of_le dim le)))])
  ^ "\n"

let poly_size p = 
  if (ppl_Polyhedron_space_dimension p) = 0 then zone else
    if poly_is_nonempty p then
      (let latte = (latte_of_poly p) in
       let result = count_models latte in
	 (*printf "--------- latte ------------\n";
	   printf "%s\n" latte;
	   printf "--------- size = %s --------\n" (Z.to_string result); *)
	 result
      )
    else zzero;;

let poly_maximize p le = 
  let dim = (ppl_Polyhedron_space_dimension p) in
    maximize_models (latte_of_poly p) (latte_of_le dim le)
