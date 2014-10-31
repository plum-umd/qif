open State
open Printf
open Util
open Stateset
open Pstateset
open Gmp
open Gmp.Q.Infixes
open Gmp.Z.Infixes
open Globals

open Gmp_util

module MakePPowerset (* create a powerset pstateset from a pstateset *)
  (SSM: STATESET_TYPE)
  (PSSM: PSTATESET_TYPE with type stateset = SSM.stateset
			and type splitter = SSM.splitter
  )
  : (PSTATESET_TYPE with type stateset = SSM.stateset
		    and type splitter = SSM.splitter
    ) =

struct
  module SS = SSM
  module PSS = PSSM

  type stateset = SS.stateset
  type splitter = SS.splitter

  type base_pstateset = PSSM.pstateset

  type pstateset = base_pstateset list

  let copy pss = List.map PSS.copy pss

  let make_empty () = []
    
  let make_point s = [PSS.make_point s]
    
  let make_new sl = [PSS.make_new sl]
    
  let print pss =
    printf "PPowerset\n";
    List.iter PSS.print pss
      
  let size pss =
    List.fold_left
      (fun accum apss -> accum +! PSS.size apss)
      zzero
      pss
    
  let rec _make_uniform_to_precision vname lower upper prec share =
    let points = (upper -! lower) +! zone in

      if (points <! ztwo) || (prec <= 1) then
	[PSS.prob_scale (PSS.make_uniform vname lower upper) share]
      else
	let mid = (upper +! lower) /! ztwo in
	let temp1 = _make_uniform_to_precision vname lower mid (prec - 1)
	  (share */ ((Q.from_z (mid -! lower +! zone)) // (Q.from_z points))) in
	let temp2 = _make_uniform_to_precision vname (mid +! zone) upper (prec - 1)
	  (share */ ((Q.from_z (upper -! (mid +! zone) +! zone)) // (Q.from_z points))) in
	  List.append temp1 temp2

  let make_uniform vname lower upper =
    if !Globals.split_uniforms_factor > 1 then
      _make_uniform_to_precision vname lower upper !Globals.split_uniforms_factor qone
    else
      [PSS.make_uniform vname lower upper]

  let _purge_useless pss =
    List.filter (fun apss -> PSS.is_possible apss) pss

  let addvar pss varid =
    List.map (fun apss -> PSS.addvar apss varid) pss

  let transform pss astmt = 
    List.map (fun apss -> PSS.transform apss astmt) pss
    
  let intersect pss ss =
    _purge_useless (List.map (fun apss -> PSS.intersect apss ss) pss)
    
  let exclude pss1 pss2 = raise Not_implemented
    
  let is_empty pss = List.for_all PSSM.is_empty pss

  let slack pss = raise Not_implemented

  let _slack_of_plus apss1 apss2 =
    printf "checking slack\n";
    flush stdout;
    let apss = PSS.abstract_plus apss1 apss2 in
      (apss, PSS.slack apss)

  let _simplify_to_precision_simple pss maxpolies = 
    if (maxpolies = 0) || (List.length pss <= maxpolies) then pss else
      (let temp = ref pss in
	 while ((List.length !temp) > maxpolies) do
	   let p1 = List.hd !temp in
	   let p2 = List.hd (List.tl !temp) in
	     temp := PSS.abstract_plus p1 p2 :: List.tl (List.tl !temp)
	 done;
	 !temp)

  let _simplify_to_precision_random pss maxpolies = 
    if (maxpolies = 0) || (List.length pss <= maxpolies) then pss else
      let ret = ref pss in
	while ((List.length !ret) > maxpolies) do
	  Globals.simplify_steps := !Globals.simplify_steps + 1;
	  let temp = Array.to_list (array_permute (Array.of_list !ret)) in	
	  let p1 = List.nth temp 0 in
	  let p2 = List.nth temp 1 in
	  let p3 = PSS.abstract_plus p1 p2 in
	    ret := p3 :: List.tl (List.tl temp)
	done;
	!ret

  let _simplify_to_precision_complex pss maxpolies = 
    if (maxpolies = 0) || (List.length pss <= maxpolies) then pss else
      (let temp = ref pss in
	 while ((List.length !temp) > maxpolies) do
	   let opts = list_pairs_and_rest !temp in
	     printf "slack options = %d\n" (List.length opts); flush stdout;
	   let opts_goodness = List.map (fun (p1, p2, rest) -> (_slack_of_plus p1 p2, rest)) opts in
	   let ((bapps, bslack), brest) = list_ultimate_nonempty
	     (fun
		((aapss1, aslack1), arest1)
		((aapss2, aslack2), arest2) -> aslack1 </ aslack2)
	     opts_goodness
	   in
	     temp := bapps :: brest
	 done;
	 !temp)

  let _simplify_to_precision_halfs pss maxpolies =
    let ret = (
      if maxpolies = 1 then
	_simplify_to_precision_simple pss maxpolies
      else
	let have = List.length pss in
	  if (maxpolies = 0) || (have <= maxpolies) then pss else
	    let split1 = have / 2 in
	      (*let split2 = have - split1 in*)
	    let make1 = maxpolies / 2 in
	    let make2 = maxpolies - make1 in
	    let (half1, half2) = list_split_into split1 pss in
	      List.append
		(_simplify_to_precision_simple half1 make1)
		(_simplify_to_precision_simple half2 make2)
    ) in
      ret

  let _simplify_to_precision_select () =
    match !Globals.simplifier with
      | 0 -> _simplify_to_precision_halfs
      | 1 -> _simplify_to_precision_simple
      | 2 -> _simplify_to_precision_complex
      | 3 -> _simplify_to_precision_random
      | _ -> raise (General_error "unknown simplifier selected")

  let _simplify_to_precision pss maxpolies =
    Globals.start_timer Globals.timer_simplify;
    let ret = (_simplify_to_precision_select ()) pss maxpolies in
      Globals.stop_timer Globals.timer_simplify;
      ret

  let _simplify pss =
    _simplify_to_precision pss !Globals.precision

  let make_splitter pss alexp = PSS.make_splitter (List.hd pss) alexp
    
  let split_many_with_splitter pss vars splitter =
    let (ins, outs) = (List.fold_left (fun (accin, accout) apss ->
					 let (ins, outs) = PSS.split_many_with_splitter apss vars splitter in
					   (List.append accin ins, List.append accout outs))
			 ([], []) pss) in
      ([_purge_useless ins], [_purge_useless outs])

  let split_many pss alexp =
    let vars = Lang.collect_vars_lexp alexp in
      if List.length pss = 0 then ([], []) else
	(let splitter = PSS.make_splitter (List.hd pss) alexp in
	   split_many_with_splitter pss vars splitter)

  let split pss alexp =
    if (List.length pss) = 0 then ([], []) else
      (
	let (ins, outs) = split_many pss alexp in
	  (_simplify_to_precision (List.hd ins) !Globals.precision,
	   _simplify_to_precision (List.hd outs) !Globals.precision))

  let set_all pss sil = List.map (fun apss -> PSS.set_all apss sil) pss
    
  let project pss vl = List.map (fun apss -> PSS.project apss vl) pss
    
  let vars pss = match pss with
    | [] -> []
    | apss :: rest -> PSS.vars apss

  let stateset_hull pss = SS.statesets_union_list_nocomp (List.map (fun apss -> PSS.stateset_hull apss) pss)
  let stateset_hull_on_vars pss vars = SS.statesets_union_on_vars_list_nocomp (List.map (fun apss -> PSS.stateset_hull apss) pss) vars
    
  let enum pss = SS.stateset_enum (stateset_hull pss)
  let enum_on_vars pss vars = 
    SS.stateset_enum (stateset_hull_on_vars pss vars)

  let relative_entropy pss1 pss2 = 0.0

  let max_prob pss = raise (General_error "max_prob not implemented")
    
  let prob_max_in_min_out pss s =
    List.fold_left (fun (inmass, outmass) apss ->
		      let (morein, moreout) = PSS.prob_max_in_min_out apss s in
			(inmass +/ morein, outmass +/ moreout))
      (qzero, qzero) pss

  let prob_scale pss s = List.map (fun apss -> PSS.prob_scale apss s) pss

  let make_point_of_stateset ss = [PSS.make_point_of_stateset ss]

  let abstract_plus pss1 pss2 =
    Globals.seen_complexity ((List.length pss1) + (List.length pss2));
    let maxpolies = !Globals.precision in
      if (maxpolies = 0) then List.append pss1 pss2 else
	if (maxpolies = 1) then
	  _simplify_to_precision (List.append pss1 pss2) maxpolies
	else if !Globals.simplifier = 3 then
	  _simplify_to_precision (List.append pss1 pss2) maxpolies
	else
	(
	  let num1 = List.length pss1 in
	  let num2 = List.length pss2 in
	  (*let (num1, num2, pss1, pss2) = 
	    if num1 < num2 then (num1, num2, pss1, pss2) else (num2, num1, pss2, pss1) in*)
	  let halfpolies = maxpolies / 2 in
	  let remove1 = max 0 (num1 - halfpolies) in
	  let remove2 = max 0 ((num1 - remove1 + num2) - maxpolies) in
	    printf "splitting %d,%d into %d,%d\n" num1 num2 (num1 - remove1) (num2 - remove2);
	  let ret = 
	    List.append
	      (_simplify_to_precision pss1 (num1 - remove1))
	      (_simplify_to_precision pss2 (num2 - remove2)) in
	    if (num1 + num2 >= maxpolies && List.length ret < maxpolies) ||
	      (num1 + num2 < maxpolies && List.length ret < num1 + num2) then
	      raise (General_error "not using up all polies");
	    ret
	)

  let is_possible pss =
    List.exists PSS.is_possible pss

  let prob_max_norm pss s =
    if not (is_possible pss) then qzero
    else 
      let (massin, massout) = prob_max_in_min_out pss s in
	massin // massout

  let min_mass pss = list_sum_general qzero Q.add (List.map (fun apss -> PSS.min_mass apss) pss)

  let prob_max_min pss = raise Not_implemented

  let max_belief pss =
    if not (is_possible pss) then qzero else (
      let total_mass = min_mass pss in
	if total_mass =/ qzero then qone
	else (
	  let probs = List.map (fun apss ->
				  (PSS.stateset_hull apss,
				   PSS.prob_max_min apss)) pss in

	  (*let prob_lists = SS.statesets_approx_intersections probs in  *)

	  let prob_lists = SS.statesets_exact_intersections probs in

	  let prob_maxes = List.map
	    (fun plist ->
	       let (massin, massout) =
		 (List.fold_left (fun (accin, accout) (amassin, amassout) -> (accin +/ amassin, accout +/ amassout))
		    (Q.zero, Q.zero) plist) in
		 (massin // total_mass)) prob_lists in
	  let ret = list_max_q prob_maxes in
	    if ret >/ qone then qone else ret
	))
	      
	(*	     (if massout =/ qzero then 
		     (if massin >/ qzero then qone else qzero) *)

  let prod pss1 pss2 =
    _simplify
      (List.fold_left
	 (fun accum apss1 ->
	    List.append accum
	      (List.map (fun apss2 -> PSS.prod apss1 apss2)
		 pss2))
	 [] pss1)

end

