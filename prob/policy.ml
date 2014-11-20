open State
open Util
open Esys
open Lang
open Printf
open Gmp
open Gmp.Q.Infixes
open Pdefs
open Preeval

type policytype =
  | PMinRelEnt
  | PMaxProbOut
  | PMaxProbAll
  | PMaxProb
;;

type policy = {plabel: string;
	       ptype: policytype;
	       pvars: Lang.varid list;
	       pparam: Q.t};;

let policytype_of_string pname =
  match pname with
    | "max_prob_output" -> PMaxProbOut
    | "max_prob_all"    -> PMaxProbAll
    | "max_prob"        -> PMaxProb
    | "min_rel_entropy" -> PMinRelEnt
    | _ -> raise (General_error ("unknown polict type " ^ pname))
;;

let policy_new plabel pname vars param = {plabel = plabel;
					  ptype = policytype_of_string pname;
					  pvars = vars;
					  pparam = param}


let string_of_policytype pt = match pt with
  | PMinRelEnt -> "min_rel_entropy (minimum relative entropy)"
  | PMaxProbOut -> "max_prob_output (output-based min-entropy)"
  | PMaxProbAll -> "max_prob_all (min-entropy)"
  | PMaxProb -> "max_prob"
;;

let string_of_policy p =
  let string_ptype = string_of_policytype p.ptype in
  let string_vars = String.concat " " (List.map Lang.varid_to_string p.pvars) in
    "policy type " ^ string_ptype ^ " of vars " ^ string_vars ^ " with param " ^ (Q.to_string p.pparam)
;;

type queryresult =
  | RTrueValue of (string * int) list
  | RReject of string

module MAKE_PSYSTEM (ESYS: EVAL_SYSTEM) = struct
  type policysystem =
      {policies: policy list;
       belief: ESYS.psrep;
       valcache: state}

  type policysystemupdate =
      {newbelief: ESYS.psrep}

  type policysystemresult =
      {result: queryresult;
       update: policysystemupdate}

  let policysystem_of_list plist (belief: ESYS.psrep) (secret: state) =
    let pols = List.map (fun p ->
			   policy_new (policy_record_label p) p.name p.varlist p.param) plist in
      {policies = pols;
       belief = belief;
       valcache = secret}

  let find_max_belief distout outputs pvars =
    (* let onvars = ESYS.psrep_on_vars distout outputs in *)
    let outstates = ESYS.psrep_enum_on_vars distout outputs in
      list_max (List.map
		  (fun s -> 
		     ifverbose (printf "-- possible output = "; s#print; printf "\n";
				flush stdout);
		     (let revised =
			(ESYS.psrep_on_vars
			   (ESYS.psrep_given_state distout s)
			   pvars) in
			(

			  let m = ESYS.psrep_max_belief revised in
			    ifverbose (
			      printf "revised belief for this output:\n";
			      ESYS.print_psrep revised;
			      printf "max belief for this output= %s\n" (Q.to_string m);
			      flush stdout);
			    m
			)
		     ))
		  outstates)

  let policy_eval
      (p: policy)
      (distout: ESYS.psrep)
      (distbelief: ESYS.psrep)
      (distactual: ESYS.psrep)
      (outputs: Lang.varid list)
      : bool = 
    (* TODO: the below needs to be fixed to include variables that are not mentioned in the policy but are used to probabilistically build up
       the ones mentioned *)
      match p.ptype with
	| PMinRelEnt -> 
	    let bred = ESYS.psrep_on_vars distbelief p.pvars in 
	    let ared = ESYS.psrep_on_vars distactual p.pvars in
	    let relent = ESYS.psrep_relative_entropy bred ared in
	      if relent < (Q.to_float p.pparam) then
		(printf "*** relative entropy was %f but needed at least %s ***\n" relent (Q.to_string p.pparam); false)
	      else true
	| PMaxProbOut ->
	    let max_prob = find_max_belief distout outputs p.pvars in
	      printf "-- overall max_belief = %s = %f\n" (Q.to_string max_prob) (Q.to_float max_prob);
	      ifbench Globals.set_record p.plabel (string_of_float (Q.to_float max_prob));
	      (max_prob < p.pparam)
	    (*
	    let onvars = ESYS.psrep_on_vars distout outputs in
	    let outstates = ESYS.psrep_enum onvars in
	      (try
		 (List.iter
		    (fun s -> 
		       (let revised =
			  (ESYS.psrep_on_vars
			     (ESYS.psrep_given_state distout s)
			     p.pvars) in
			let maxprob = (ESYS.psrep_max_belief revised) in
			  s#print; printf "\n";
			  ESYS.print_psrep revised;
			  printf "max belief: %f\n" maxprob;
			  if (maxprob > p.pparam) then raise Loop_exit else ()
		       ))
		    outstates);
		 true
	       with
		 | Loop_exit -> false)
	    *)
	| PMaxProbAll -> true (* find max prob over all states in belief *)
	| PMaxProb -> true (* sample the probability of the secret in the belief here *)
	
  let policysystem_answered (ps: policysystem) (up: policysystemupdate): policysystem =
    {policies = ps.policies;
     belief = up.newbelief;
     valcache = ps.valcache}

  let rec policysystem_check_policies
      (policies: policy list)
      (distout: ESYS.psrep) (* output distribution *)
      (distbelief: ESYS.psrep) (* revised distribution *)
      (distactual: ESYS.psrep)
      (outputs: Lang.varid list)
      : string option =
    match policies with
      | [] -> None
      | p :: r ->
	  if not (policy_eval p distout distbelief distactual outputs) then
	    let string_p = string_of_policy p in
	      Some ("policy not satisfied: " ^ string_p)
	  else
	    policysystem_check_policies r distout distbelief distactual outputs

  let policysystem_answer (ps: policysystem) (query: (Lang.varid list * Lang.varid list * Lang.stmt)) (queryinput_stmt: Lang.stmt) :  policysystemresult =
    (* todo: simplify some of this query preparation, factor out to someplace else, also done repeatedly in prob.ml *)

    printf "\nstart belief:\n"; ESYS.print_psrep ps.belief;

    let (inlist, outlist, querystmt) = query in
    let secretvars = ESYS.psrep_vars ps.belief in
      
    let sa_querystmt = if !Globals.use_dsa then 
      (sa_of_stmt querystmt (List.append secretvars inlist) outlist)
    else
      querystmt in

    let (ignored, inputstate_temp) = Evalstate.eval queryinput_stmt (new state_empty) in
    let inputstate = inputstate_temp#copy in
      inputstate#project inlist;
      let inputstate_full = inputstate#copy in
      let sa_querystmt = Preeval.predefine_as_state sa_querystmt inputstate inlist in
	ifdebug (printf "predefined is \n";
		 Lang.print_stmt_pretty sa_querystmt "";
		 printf "--- end of predefined ---\n");
	
	inputstate_full#merge ps.valcache;

	printf "\nquery inputs:\n\t"; inputstate#print; printf "\n";

	(*let inputdist = ESYS.psrep_set_all ps.belief inputstate in *) (* inputs are now substituted above using Preeval.predefine *)
	let inputdist = ps.belief in

	let secretdist = ESYS.psrep_point (ESYS.srep_point ps.valcache) in

	  printf "\ninput belief:\n"; ESYS.print_psrep inputdist;

	  let outputdist = ESYS.peval sa_querystmt inputdist in

	    printf "\nend belief:\n"; ESYS.print_psrep outputdist;

	    let (ignored, outputstate_temp) = Evalstate.eval sa_querystmt
	      inputstate_full in

	    let outputstate = outputstate_temp#copy in
	      outputstate#project outlist;

	      (*
	       let startrelent = ESYS.psrep_relative_entropy inputdist secretdist in *)
	       
	      let enddist =
		ESYS.psrep_on_vars
		  (ESYS.psrep_given_state outputdist outputstate)
		  secretvars in
	    
	      let ps_updater = {newbelief = enddist} in 

	      (* let ps_updater = {newbelief = outputdist} in *)
	      (*
		let endrelent = ESYS.psrep_relative_entropy enddist secretdist in
	      *)
		
		printf "\ninput state:\n\t"; inputstate_full#print; printf "\n";

		printf "\noutput view:\n\t"; outputstate#print; printf "\n";
		printf "\nrevised belief\n"; ESYS.print_psrep enddist;
	      
	      (* printf "relative entropy (start -> secret): %f\n" startrelent;
		 printf "relative entropy (revised -> secret): %f\n" endrelent;
		 printf "bits learned: %f\n" (startrelent -. endrelent); *)

	      printf "\n### checking policies ###\n";
	      (*flush stdout;*)
	      
	      (*      match policysystem_check_policies ps.policies outputdist enddist secretdist outlist with *)
	      match policysystem_check_policies ps.policies outputdist outputdist secretdist outlist with
		  (*	| None -> {result = RTrueValue (outputstate#canon);*)
		| None -> {result = RTrueValue ([]);
			   update = ps_updater}
		| Some (s) -> {result = RReject (s);
			       update = {newbelief = ps.belief}}
end
;;

(*module PSYS_S = MAKE_PSYSTEM (ESYS_S);;*)
(*module PSYS_SSP = MAKE_PSYSTEM (ESYS_SSP);;*)
