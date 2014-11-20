open State
open Lang
open Printf
open Pdefs
open Esys
open Policy
open Util
open Preeval
open Globals
open Parser_util

open Maths
open Gmp

let add_policy_records aexp =
  List.iter
    (fun p ->
       Globals.new_record (policy_record_label p) "")
    aexp.policies;;

module type EXP_SYSTEM = sig
  val pmock: Pdefs.tpmocksetup -> unit
end;;

module MAKE_EVALS (ESYS: EVAL_SYSTEM) = struct
  module PSYS = MAKE_PSYSTEM(ESYS)

  let rec pmock_queries queries querydefs ps = match queries with
    | [] -> ()
    | (queryname, querystmt) :: t ->
	ifbench Globals.start_timer Globals.timer_query;

	let querytuple = List.assoc queryname querydefs  in
	let (inlist, outlist, progstmt) = querytuple in

	let secretvars = ESYS.psrep_vars ps.PSYS.belief in
	let sa_progstmt = (sa_of_stmt progstmt (List.append secretvars inlist) outlist) in
	let sa_progstmt = (if !Globals.use_dsa then sa_progstmt else progstmt) in

	  printf "-------------------------------------------------\n";
	  printf "query %s from %s to %s\n"
	    queryname
	    (String.concat " " (List.map Lang.varid_to_string inlist))
	    (String.concat " " (List.map Lang.varid_to_string outlist));
	  print_stmt progstmt; printf "\n";

	  ifverbose
	    (printf "query (single assignment):\n"; print_stmt sa_progstmt; printf "\n");

	  let ans = PSYS.policysystem_answer ps querytuple querystmt in
	  let res = ans.PSYS.result in
	  let ps = PSYS.policysystem_answered ps ans.PSYS.update in

	    (match res with
	       | RTrueValue (vals) ->
		   (printf "*** query was accepted\n") 
		     (* , "results: %s\n" (String.concat " " (List.map (fun (k,v) -> k ^ "=" ^ (string_of_int v)) vals))) *)
	       | RReject (reason) ->
		   (printf "*** query was rejected due to: %s\n" reason;
		    printf "*** belief will not be updated as a result of this query\n"))
	    ;

	    ifbench (
	      Globals.stop_timer Globals.timer_query;
	      Globals.mark_epoch ();
	      Globals.print_epoch ();
	      Globals.next_epoch ()
	    );

	    pmock_queries t querydefs ps

  let pmock asetup = 
    Printexc.record_backtrace true;
(*      let vars = pmock_all_vars asetup in*)
      let secretstmt = Preeval.preeval asetup.secret in
      let beliefstmt = Preeval.preeval asetup.belief in

      (* let (inlist, outlist, progstmt) = asetup.expprog in *)

      let querydefs = List.map
	(fun (astring, (l1, l2, apstmt)) -> (astring, (l1, l2, Preeval.preeval apstmt)))
	asetup.querydefs in
      let queries   = List.map
	(fun (astring, apstmt) -> (astring, Preeval.preeval apstmt))
	asetup.queries in
      let policies  = asetup.policies in
	
      let (ignored, secretstate) = Evalstate.eval secretstmt (new state_empty) in
	
      let secretvars = Lang.all_vars beliefstmt in
	
      let sa_beliefstmt = (sa_of_stmt beliefstmt [] secretvars) in
      let sa_beliefstmt = (if !Globals.use_dsa then sa_beliefstmt else beliefstmt) in

      let startdist = ESYS.peval_start sa_beliefstmt in
	
      (*let secretdist = ESYS.psrep_point (ESYS.srep_point secretstate) in*)	
      (*let startrelent = ESYS.psrep_relative_entropy startdist secretdist in*)
      
	printf "secret:\n\t"; secretstate#print; printf "\n";
	ifverbose (printf "initial belief generator:\n"; print_stmt beliefstmt; printf "\n");
	ifverbose (printf "initial belief generator (single assignment):\n"; print_stmt sa_beliefstmt; printf "\n");
	printf "initial belief:\n"; ESYS.print_psrep startdist;
	(*printf "relative entropy (initial -> secret): %f\n" startrelent; *)

	let ps = {PSYS.policies =
	    (List.map
	       (fun p -> policy_new (policy_record_label p) p.name p.varlist p.param) policies);
		  PSYS.belief = startdist;
		  PSYS.valcache = secretstate} in
	  
	  pmock_queries queries querydefs ps
  (*with 
      | e ->
	  printf "%s\n" (Printexc.to_string e);
	  Printexc.print_backtrace stdout*)

end
;;

(*module EVALS_S = MAKE_EVALS(ESYS_S);;*)
module EVALS_PPSS_POLY = MAKE_EVALS(ESYS_PPSS_POLY);;
module EVALS_PPSS_BOX  = MAKE_EVALS(ESYS_PPSS_BOX);;
module EVALS_PPSS_OCTA = MAKE_EVALS(ESYS_PPSS_OCTA);;
module EVALS_PPSS_OCTALATTE = MAKE_EVALS(ESYS_PPSS_OCTALATTE);;

let main () =
  let infile = ref "-" in
  let opt_esys = ref 3 in
  let opt_pmock = ref 1 in
  let seed = ref 0 in
    Arg.parse [
      ("--use-latte-minmax",
       Arg.Set Globals.use_latte_minmax,
       "use latte for maximization, constant 1 for minimization");
      ("--use-dsa",
       Arg.Set Globals.use_dsa,
       "convert to dynamic single assignment");
      ("--precision",
       Arg.Set_int Globals.precision,
       "set the precision");
      ("--split-factor",
       Arg.Set_int Globals.split_uniforms_factor,
       "set the uniforms split factor, default = 1");
      ("--pmock",
       Arg.Unit (fun () -> opt_pmock := 1),
       "run the mock policy");
      ("--domain",
       Arg.String (fun (s) ->
		     opt_esys := 
		       (match s with
			  | "octalatte" -> 4
			  | "octa" -> 2
			  | "poly" -> 3
			  | "box"  -> 1
			  | "list" -> 0
			  | _ -> raise (General_error ("unknown domain: " ^ s)))),
       "base domain: list, box, octalatte, poly");
      ("--bench",
       Arg.String (fun s ->
		     if s <> "--" then Globals.set_bench s;
		     Globals.output_bench := true
		  ),
       "write out timing information, use -- to designate stdout");
      ("--bench-latte",
       Arg.String (fun s ->
		     if s <> "--" then Globals.set_bench_latte s;
		     Globals.output_bench_latte := true
		  ),
       "write out latte timing information, use -- to designate stdout");
      ("--verbose",
       Arg.Set Globals.output_verbose,
       "verbose output");
      ("--debug",
       Arg.Set Globals.output_debug,
       "debug output");
      ("--simplify",
       Arg.String (fun (s) ->
		     Globals.simplifier := 
		       (match s with
			  | "simple" -> 1
			  | "halfs"  -> 0
			  | "random" -> 3
			  | "slack"  -> 2
			  | _ -> raise (General_error ("unknown simplifier: " ^ s)))),
       "precision simplifier: simple, halfs, random, slack");
      ("--seed",
       Arg.Set_int seed,
       "set the random seed, default 0")
	] (function s -> infile := s) "";
    (*if !opt_esys <> 3 then Globals.split_uniforms := true;*)
    Random.init(!seed);
    ifdebug Printexc.record_backtrace true;
    try
      let aexperiment = parse !infile Parser.pmock in
	ifbench (add_policy_records aexperiment;
		 Globals.print_header ());
	Globals.bench_latte_out_header ();
	let module E =
	  (val (match !opt_esys with
		  | 0 -> (*printf "running with lists\n";
		      (module EVALS_S: EXP_SYSTEM)*)
		      raise (General_error "list-based peval not implemented")
		  | 1 -> (module EVALS_PPSS_BOX: EXP_SYSTEM)
		  | 2 -> (module EVALS_PPSS_OCTA: EXP_SYSTEM)
		  | 3 -> (module EVALS_PPSS_POLY: EXP_SYSTEM)
		  | 4 -> (module EVALS_PPSS_OCTALATTE: EXP_SYSTEM)
		  | _ -> raise Not_expected): EXP_SYSTEM) in
	  E.pmock aexperiment;
	  ifdebug (printf "maximum complexity encountered = %d\n" !Globals.max_complexity);
	  ifbench (Globals.close_bench ());
	  Globals.bench_latte_close ();
(*	  printf "simplification steps: %d\n" !Globals.simplify_steps;
	  printf "no errors\n"*)
    with 
      | e ->
	  Unix.chdir Globals.original_dir;
	  ifdebug (printf "%s\n" (Printexc.to_string e);
		   Printexc.print_backtrace stdout);
	  raise e
;;

main();;
