open Lang
open Printf
open Util
open Preeval

open Maths
open Gmp
open Gmp_util
open Parser_util
open Preeval
open Smcdefs
open Evalstate
open State
open Esys

let translate_to_perspective astmt (evalvals: (agent, State.state) Hashtbl.t) (whichagents: agent list) =
  transform_stmt
    (fun astmt -> raise Unchanged)
    (fun alexp -> raise Unchanged)
    (fun aaexp -> match aaexp with
       | AEVar ((owner, name)) ->
	   if List.mem owner whichagents then AEInt ((Hashtbl.find evalvals owner)#get (owner, name))
	   else aaexp
       | _ -> raise Unchanged)
    astmt

let translate_to_owner (astmt: Lang.stmt) (whichagent: agent) =
  transform_stmt
    (fun astmt -> 
       match astmt with
	 | SDefine ((owner, name), dt) -> SDefine ((whichagent, name), dt)
	 | SUniform ((owner, name), a, b) -> SUniform ((whichagent, name), a, b)
	 | SAssign ((owner, name), aexp) -> SAssign ((whichagent, name), aexp)
	 | _ -> raise Unchanged)
    (fun alexp -> raise Unchanged)
    (fun aaexp -> match aaexp with
       | AEVar ((owner, name)) -> AEVar ((whichagent, name))
       | _ -> raise Unchanged
    )    
    astmt

module type EXP_SYSTEM = sig
  val domock: Smcdefs.tsmcmocksetup -> unit
  val domock_nosmc: Smcdefs.tsmcmocksetup -> unit
end;;

module ST = State.STATE_HASHABLE;;

module CMAKER (E: EVAL_SYSTEM) = struct
  let max_belief abelief overvars outputvars =
    ifdebug (printf "checking belief\n";
	     E.print_psrep abelief);

    let outstates = E.psrep_enum_on_vars abelief outputvars in
      list_max_q_fun pair_first
	(List.map
	   (fun s -> 
	      ifdebug (printf "possible output = "; s#print; printf "\n");
	      
	      let temp = E.psrep_given_state abelief s in
	      let revised = (E.psrep_on_vars (E.psrep_given_state abelief s) overvars) in
		
	      let m = E.psrep_max_belief revised in
		ifdebug (printf "revision possibility:\n";
			 E.print_psrep revised;
			 printf "possible revision max belief = %s\n" (Q.to_string m));
		(m, s)
	   ) outstates)
  ;;

  let tryquery
      (invars, outvars, (querypstmt: pstmt))
      (inputpstmt: pstmt)
      agents
      secretstates
      (beliefreps: (Lang.agent * (Lang.agent list) * E.psrep) list)
      policies =
    let querystmt = preeval querypstmt in
    let output_agents = get_output_agents querystmt in
    let inputstmt = preeval inputpstmt in
    let (_, inputstate) = Evalstate.eval inputstmt (new state_empty) in
      inputstate#project invars;

    List.iter
      (fun (anagent, otheragents, cbelief) ->
	 let outvars = Lang.find_outputs_for querystmt anagent in 
	   if List.length outvars = 0 then ()
	   else
	     (	      
	       let omniagents = list_subtract agents otheragents in
	       let querystmt = predefine_as_state querystmt inputstate invars in
	       let querystmt =
		 translate_to_perspective
		   querystmt
		   secretstates
		   omniagents in

		 printf "consider belief %s has about %s:\n" anagent (agent_list_to_string otheragents);
		 E.print_psrep cbelief;

		 printf "translated query:\n";
		 print_stmt querystmt;
		 printf "\n";

		 let outbelief = E.peval querystmt cbelief in
		   List.iter
		     (fun ap ->
			let whoagent = ap.agent in
			let varlist = ap.varlist in
			  if (not (List.mem whoagent otheragents)) ||
			    (not (List.mem anagent ap.aboutagents)) then ()
			      (*printf "this agent does not care about this belief, skipping\n"*)
			  else 
			    (printf "consider policy %s has over %s, param %s\n"
			       whoagent
			       (varid_list_to_string varlist)
			       (Q.to_string ap.param);
			     let (max_prob, max_output) = max_belief outbelief varlist outvars in

			     let otheragent = whoagent in
			     let max_secretstate = Hashtbl.find secretstates anagent in

			     let temp = sprintf "agent %s's policy: agent %s's max belief = %s when agent %s's secret is %s, and output is %s"
			       otheragent
			       anagent
			       (Q.to_string max_prob)
			       anagent
			       max_secretstate#to_string
			       max_output#to_string in

			       printf "%s\n" temp;
			       debug temp

			    )) policies))
      beliefreps

  let domock (asetup: Smcdefs.tsmcmocksetup) =
    let secrets = asetup.secrets in
    let beliefs = asetup.beliefs in
    let policies = asetup.policies in
    let querydefs = asetup.querydefs in
    let queries = asetup.queries in
    let secretstates = Hashtbl.create (List.length secrets) in
      List.iter
	(fun (anagent, apstmt) ->
	   let astmt = (preeval apstmt) in
	   let astmt = translate_to_owner astmt anagent in
	   let (_, astate) = Evalstate.eval astmt (new state_empty) in
	     Hashtbl.replace secretstates anagent astate
	) secrets;
      
      let beliefreps =
	List.fold_left 
	  (fun acc (ofagents, aboutagents, apstmt) ->
	     List.append
	       acc
	       (List.map
		  (fun ofagent ->
		     (ofagent, aboutagents, E.peval_start (preeval apstmt)))
		  ofagents))
	[] beliefs in
      let agents = hash_keys secretstates in
	List.iter
	  (fun (queryname, inputpstmt)
	     -> ignore
	       (tryquery
		  (List.assoc queryname querydefs)
		  inputpstmt
		  agents
		  secretstates
		  beliefreps
		  policies
	       ))
	  queries

  let tryquery_nosmc
      (invars, outvars, (querypstmt: pstmt))
      (inputpstmt: pstmt)
      agents
      secretstates
      (beliefreps: (Lang.agent * (Lang.agent list) * E.psrep * 'a) list)
      policies =
    let querystmt = preeval querypstmt in
    let output_agents = get_output_agents querystmt in
    let inputstmt = preeval inputpstmt in
    let (_, inputstate) = Evalstate.eval inputstmt (new state_empty) in
      inputstate#project invars;

      let agents = hash_keys secretstates in

      let truequery = predefine_as_state querystmt inputstate invars in

      let truequery = 
	List.fold_left
	  (fun astmt anagent ->
	     let secretstate = Hashtbl.find secretstates anagent in
	       (*printf "predefining %s\n" secretstate#to_string;*)
	       predefine_as_state astmt secretstate secretstate#vars)
	  truequery agents in

	(*
	printf "true query view:\n";
	Lang.print_stmt truequery;
	printf "\n";

	printf "input %s\n" inputstate#to_string;
	*)



      let (_, trueout) = Evalstate.eval truequery (new state_empty) in

	(*printf "true output: %s\n" trueout#to_string;*)

	List.iter
	  (fun (anagent, otheragents, cbelief, beliefsets) ->
	     (*printf "agent %s is now considering the query\n" anagent;*)

	     let agents_vars = (Hashtbl.find secretstates anagent)#vars in

	     let otheragents = hash_keys beliefsets in
	       List.iter (fun otheragent ->
			    let outvars = Lang.find_outputs_for querystmt otheragent in 
			      (*printf "agent %s is now considering agent %s's possible views on the situation\n" anagent otheragent;*)
			      
			      let beliefset = Hashtbl.find beliefsets otheragent in

			      let newbeliefset = 
				List.map (fun (otheragent_secretstate, otheragent_belief) ->
					    let querystmt = predefine_as_state querystmt otheragent_secretstate (otheragent_secretstate#vars) in
					    let querystmt = predefine_as_state querystmt inputstate invars in

					      (*
					      printf "consider possible %s's secret %s, his query would be:\n"
						otheragent otheragent_secretstate#to_string;
					      Lang.print_stmt querystmt;
					      printf "\nhis belief is:\n";
					      E.print_psrep otheragent_belief;
						printf "\n\n";
					      *)

					      let newbelief = E.peval querystmt otheragent_belief in

						(*
						printf "his non-revised post-belief is:\n";
						  E.print_psrep newbelief;
						*)
						
						let (maxbelief, maxbelief_state) = max_belief newbelief agents_vars outvars in
						  printf "maxbelief here is %s when %s\n" (Q.to_string maxbelief) maxbelief_state#to_string;

						  flush stdout;

						  debug(sprintf
							  "%s-%s\t%s\t%s\t%s\t%s\t%f\t%s"
							  anagent
							  otheragent
							  anagent
							  otheragent
							  otheragent_secretstate#to_string
							  (Q.to_string maxbelief)
							  (Q.to_float maxbelief)
							  maxbelief_state#to_string);

						(otheragent_secretstate, newbelief, maxbelief, maxbelief_state)
					) beliefset in

			      let (max_secretstate, max_newbelief, max_prob, max_output) = list_max_q_fun (fun (a, b, c, d) -> c) newbeliefset in
			      let (min_secretstate, min_newbelief, min_prob, min_output) = list_min_q_fun (fun (a, b, c, d) -> c) newbeliefset in
				
			      let temp = sprintf "agent %s's policy: agent %s's largest max belief = %s when agent %s's secret is %s, and output is %s"
				anagent
				otheragent
				(Q.to_string max_prob)
				otheragent
				max_secretstate#to_string
				max_output#to_string in
				
				printf "%s\n" temp;
				(*debug temp;*)

				let temp = sprintf "agent %s's policy: agent %s's smallest max belief = %s when agent %s's secret is %s, and output is %s"
				  anagent
				  otheragent
				  (Q.to_string min_prob)
				  otheragent
				  min_secretstate#to_string
				  min_output#to_string in
				
				  printf "%s\n" temp;
				  (*debug temp;*)

			      let newbeliefset = List.map (fun (a, b, c, d) -> (a, b)) newbeliefset in
				Hashtbl.replace beliefsets otheragent newbeliefset
			 ) otheragents)
	  beliefreps

		   (*
		     let querystmt = predefine_as_state querystmt inputstate invars in
		     let omniagents = list_subtract agents otheragents in

		     let querystmt =
		     translate_to_perspective
		     querystmt
		     secretstates
		     omniagents in

		     printf "consider belief %s has about %s:\n" anagent (agent_list_to_string otheragents);
		     E.print_psrep cbelief;

		     printf "translated query:\n";
		     print_stmt querystmt;
		     printf "\n";

		     let outbelief = E.peval querystmt cbelief in
		     List.iter
		     (fun ap ->
		     let whoagent = ap.agent in
		     let varlist = ap.varlist in
		     if (not (List.mem whoagent otheragents)) ||
		     (not (List.mem anagent ap.aboutagents)) then ()
  (*printf "this agent does not care about this belief, skipping\n"*)
		     else 
		     (printf "consider policy %s has over %s, param %s\n"
		     whoagent
		     (varid_list_to_string varlist)
		     (Q.to_string ap.param);
		     let maxbelief = max_belief outbelief varlist outvars in
			       printf "max belief = %f\n" maxbelief)
		     ) policies)

      beliefreps*)

  let domock_nosmc (asetup: Smcdefs.tsmcmocksetup) =
    let secrets = asetup.secrets in
    let beliefs = asetup.beliefs in
    let policies = asetup.policies in
    let querydefs = asetup.querydefs in
    let queries = asetup.queries in
    let secretstates = Hashtbl.create (List.length secrets) in
      List.iter
	(fun (anagent, apstmt) ->
	   let astmt = (preeval apstmt) in
	   let astmt = translate_to_owner astmt anagent in
	   let (_, astate) = Evalstate.eval astmt (new state_empty) in
	     (*printf "agent %s's true secret will be derived from\n" anagent;
	     Lang.print_stmt astmt;
	     printf "\n";*)
	     Hashtbl.replace secretstates anagent astate
	) secrets;

      let agents = hash_keys secretstates in

	debug(String.concat "\t" ["policy_on";
				  "who_checks";
				  "over_whom";
				  "secret_value";
				  "max_belief";
				  "max_belief_float";
				  "on_output"]);

      let beliefhash = Hashtbl.create (List.length agents) in

      let beliefreps =
	List.fold_left 
	  (fun acc (ofagents, aboutagents, apstmt) ->
	     List.append
	       acc
	       (List.map
		  (fun ofagent ->
		     let belief = E.peval_start (preeval apstmt) in
		       Hashtbl.replace beliefhash ofagent belief;
		       (ofagent, aboutagents, belief))
		  ofagents))
	[] beliefs in

      let beliefreps =
	List.map (fun (ofagent, aboutagents, belief) ->
		    let vars = E.psrep_vars belief in
		    let beliefsets = Hashtbl.create ((List.length agents) - 1) in
		      List.iter (fun about_agent -> 
				   let about_agent_belief = Hashtbl.find beliefhash about_agent in
				   let about_agent_vars = List.filter (varid_belongs_to about_agent) vars in
				   let about_agent_vars_vals = E.psrep_enum_on_vars belief about_agent_vars in
				     Hashtbl.replace beliefsets about_agent
				       (List.map (fun vals -> 
						    (*printf "agent %s beliefs agent %s can have actual values %s\n"
						      ofagent
						      about_agent
						      vals#to_string;*)
						    (vals, E.psrep_copy about_agent_belief))
					  about_agent_vars_vals)) aboutagents;
		      (ofagent, aboutagents, belief, beliefsets)) beliefreps in

	List.iter
	  (fun (queryname, inputpstmt)
	     -> ignore
	       (tryquery_nosmc
		  (List.assoc queryname querydefs)
		  inputpstmt
		  agents
		  secretstates
		  beliefreps
		  policies
	       ))
	  queries
end;;

module CM_BOX = CMAKER(ESYS_PPSS_BOX)
module CM_OCTA = CMAKER(ESYS_PPSS_OCTA)
module CM_OCTALATTE = CMAKER(ESYS_PPSS_OCTALATTE)
module CM_POLY = CMAKER(ESYS_PPSS_POLY)

let main () =
  let nosmc = ref false in
  let infile = ref "-" in
  let domain = ref 0 in
  let opt_esys = ref 3 in
  let seed = ref 0 in
    Arg.parse [
      ("--nosmc",
       Arg.Set nosmc,
       "simulate the non-scm method");
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
      ("--verbose",
       Arg.Set Globals.output_verbose,
       "verbose output");
      ("--debug",
       Arg.Set Globals.output_debug,
       "debug output");      
      ("--seed",
       Arg.Set_int seed,
       "set the random seed, default 0");
    ] (function s -> infile := s) "";
    Random.init(!seed);
    ifdebug Printexc.record_backtrace true;
    try
      let mock = (parse !infile Parser.smcmock) in 
      let module E =
	(val (match !opt_esys with
		| 0 -> (*printf "running with lists\n";
			 (module EVALS_S: EXP_SYSTEM)*)
		    raise (General_error "list-based peval not implemented")
		| 1 -> (module CM_BOX: EXP_SYSTEM)
		| 2 -> (module CM_OCTA: EXP_SYSTEM)
		| 3 -> (module CM_POLY: EXP_SYSTEM)
		| 4 -> (module CM_OCTALATTE: EXP_SYSTEM)
		| _ -> raise Not_expected): EXP_SYSTEM) in
	if !nosmc then E.domock_nosmc mock else E.domock mock
    with 
      | e ->
	  printf "an error occured\n";
	  Unix.chdir Globals.original_dir;
	  ifdebug (printf "%s\n" (Printexc.to_string e);
		   Printexc.print_backtrace stdout)
;;

main();;

