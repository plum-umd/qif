open State
open Stateset
open Lang
open Gmp
open Gmp_util
open Printf
open Util
open Pstateset

open Gmp.Q.Infixes
open Gmp.Z.Infixes

module MakePStateset(* create pstateset from a stateset *)
  (SSM: STATESET_TYPE) : 
  (PSTATESET_TYPE with type stateset = SSM.stateset
		  and type splitter = SSM.splitter
  ) = struct

    module SS = SSM

    type stateset = SS.stateset
    type splitter = SS.splitter

    type estimator = {
      pmin: Q.t;
      pmax: Q.t;
      smin: Z.t;
      smax: Z.t;
      mmin: Q.t;
      mmax: Q.t
    }
	
    let estimator_empty = {
      pmin = qzero;
      pmax = qzero;
      smin = zzero;
      smax = zzero;
      mmin = qzero;
      mmax = qzero
    }
	
    let estimator_one = {
      pmin = qone;
      pmax = qone;
      smin = zone;
      smax = zone;
      mmin = qone;
      mmax = qone
    }

    type pstateset = {
      ss: SS.stateset;
      est: estimator
    }

    let estimator_prob_scale est scalar = {
      pmin = est.pmin */ scalar;
      pmax = est.pmax */ scalar;
      smin = est.smin;
      smax = est.smax;
      mmin = est.mmin */ scalar;
      mmax = est.mmax */ scalar
    }

    let make_empty () = 
      (* need to give it estimator_one as this is used for initialization and once
	 a new variable is added, via prod, there needs to be full mass estimated for
	 the math to work out (maybe) *)
      let temp = {ss = SS.stateset_empty ();
		  est = estimator_one} in
	temp

    let make_point s = {ss = SS.stateset_point s;
			est = estimator_one}

      (* todo: change spec of psrep_point in esys to take in a state instead of srep *)
    let make_point_of_stateset ss = {ss = ss;
				     est = estimator_one}

    let make_new sl = {ss = SS.stateset_new sl;
		       est = estimator_one}

    let make_uniform vname lower upper =
      let range = (upper -! lower) +! zone in
      let rangeq = Q.from_z range in
	{ss = SS.stateset_uniform vname lower upper;
	 est = {pmin = qone // rangeq;
		pmax = qone // rangeq;
		smin = range;
		smax = range;
		mmin = qone;
		mmax = qone
	       }}

    let print pss =
      printf "PStateset (p=[%s,%s],s=[%s,%s],m=[%s,%s]): "
	(Q.to_string pss.est.pmin)
	(Q.to_string pss.est.pmax)
	(Z.to_string pss.est.smin)
	(Z.to_string pss.est.smax)
	(Q.to_string pss.est.mmin)
	(Q.to_string pss.est.mmax);
      SS.print_stateset pss.ss;
      printf "\n"

    let size pss = SS.stateset_size pss.ss
	
    let prod pss1 pss2 = 
      (* todo: make sure variables are disjoint *)
      
      let ss = SS.stateset_prod pss1.ss pss2.ss in
	{ss = ss;
	 est = {
	   (* todo: do these make sense *)
	   (* todo: add write up to paper *)
	   pmin = pss1.est.pmin */ pss2.est.pmin;
	   pmax = pss1.est.pmax */ pss2.est.pmax;
	   smin = pss1.est.smin *! pss2.est.smin;
	   smax = pss1.est.smax *! pss2.est.smax;
	   mmin = pss1.est.mmin */ pss2.est.mmin;
	   mmax = pss1.est.mmax */ pss2.est.mmax
	 }}

    let addvar pss varid =
      {ss = SS.stateset_addvar pss.ss varid;
       est = pss.est}

    let transform pss astmt = {ss = SS.stateset_transform pss.ss astmt;
			       est = pss.est}

    let _assert_check pss =
      let printer () =
	printf "\n=begin=======================\n";
	print pss;
	printf "\n=end=========================\n";
	flush stdout in
	if (pss.est.smax >! (SS.stateset_size pss.ss)) then
	  (printer ();
	   raise (General_error "got smax > size"))
	else if (pss.est.smin <! zzero) then
	  (printer ();
	   raise (General_error "got smin less than zero"))
	else if (pss.est.pmin </ qzero) then
	  (printer ();
	   raise (General_error "got pmin less than zero"))
	else if (pss.est.smax <! pss.est.smin) then
	  (printer ();
	   raise (General_error "got smax < smin"))
	else if (pss.est.pmax </ pss.est.pmin) then
	  (printer ();
	   raise (General_error "got pmax < pmin"))
	else ()

    let slack pss =
      let size = SS.stateset_size pss.ss in
      let halfsize = size /! (Z.from_int 2) in
      let est = pss.est in
      let smin = Z.max zzero (est.smin -! halfsize) in
      let smax = Z.min halfsize est.smax in
      let minprob_maxdenom = 
	qmin
	  (est.pmax */ (Q.from_z smax))
	  (est.mmax -/ (est.pmin */ (Q.from_z smin))) in
      let maxprob_mindenom = 
	qmax
	  (est.pmin */ (Q.from_z smin))
	  (est.mmin -/ (est.pmax */ (Q.from_z smax))) in
      let minprob = 
	(if minprob_maxdenom <=/ qzero then qone else 
	   est.pmin // minprob_maxdenom) in
      let maxprob =
	(if maxprob_mindenom <=/ qzero then qone else
	   est.pmax // maxprob_mindenom) in
	maxprob -/ minprob

    (* definition 11 *)
    let intersect pss ss =
      let ssinter = SS.stateset_intersect pss.ss ss in
      let sizeinter = SS.stateset_size ssinter in
      let sizesidepss = (SS.stateset_size pss.ss) -! sizeinter in (* !!! todo imporant: this needs to be an overestimate of what lies outside *)
	if (not (Z.equal (sizeinter +! sizesidepss)  (SS.stateset_size pss.ss))) then
	  raise (General_error "intersection assertion failed")
	else (
      let est = pss.est in
      let newpmin = est.pmin in
      let newpmax = est.pmax in
      let newsmin = Z.max (est.smin -! sizesidepss) zzero in
      let newsmax = Z.min est.smax sizeinter in
      let temp =
	{ss = ssinter;
	 est = {pmin = newpmin;
		pmax = newpmax;
		smin = newsmin;
		smax = newsmax;
		mmin =
	     qmax
	       (newpmin */ (Q.from_z newsmin))
	       (est.mmin -/ (est.pmax */ (Q.from_z (Z.min est.smax sizesidepss))));
		mmax =
	     qmin
	       (newpmax */ (Q.from_z newsmax))
	       (est.mmax -/ (est.pmin */ (Q.from_z (Z.max zzero (est.smin -! sizeinter)))))
	       }} in
	(_assert_check temp;
	 temp))

    let exclude pss1 pss2 = raise Not_implemented
 
    let is_empty pss = SS.stateset_is_empty pss.ss

    let copy pss =
      {ss = SS.stateset_copy pss.ss;
       est = pss.est}

    (* definition 9 *)
    let abstract_plus pss1 pss2 =
      let pss1 = copy pss1 in
      let pss2 = copy pss2 in
	if ((SS.stateset_size pss1.ss) =! zzero) then
	  pss2
	else if ((SS.stateset_size pss2.ss) =! zzero) then
	  pss1 else (
(*	    printf "\nplusing:\n";
	    print pss1;
	    printf "\n-- and --\n";
	    print pss2;
	    printf "\n";*)
	    let ssinter = SS.stateset_intersect pss1.ss pss2.ss in
	    let n3 = SS.stateset_size ssinter in
	    let n1 = (SS.stateset_size pss1.ss) -! n3 in
	    let n2 = (SS.stateset_size pss2.ss) -! n3 in
	    let overpessi = Z.max
	      (
		(Z.max (pss1.est.smin -! n1) zzero) +!   (* the maxing here shouldn't be necessary *)
		  (Z.max (pss2.est.smin -! n2) zzero) -! n3)
	      zzero in
	      (*
		(((pss1.est.smin -! n1) +! (pss2.est.smin -! n2)) -! n3)
		zzero in *)
	
      let overopti = (Z.min (Z.min pss1.est.smax pss2.est.smax) n3) in
      let ssunion = SS.stateset_union pss1.ss pss2.ss in
      let temp = 
	{ss = ssunion;
	 est = {
	   pmin =
	     if overpessi =! (SS.stateset_size ssunion) then
	       pss1.est.pmin +/ pss2.est.pmin
	     else
	       qmin pss1.est.pmin pss2.est.pmin;
	   pmax =
	     if overpessi >! zzero then
	       pss1.est.pmax +/ pss2.est.pmax
	     else
	       qmax pss1.est.pmax pss2.est.pmax;
	   smin = Z.max zzero ((pss1.est.smin +! pss2.est.smin) -! overopti);
	   smax = Z.min (SS.stateset_size ssunion) ((pss1.est.smax +! pss2.est.smax) -! overpessi);
	   mmin = pss1.est.mmin +/ pss2.est.mmin;
	   mmax = pss1.est.mmax +/ pss2.est.mmax
	 }
	} in
	(*printf "\nresult (intersection = %s):\n" (Z.to_string (SS.stateset_size ssinter));
	print temp;
	printf "\n";*)
	_assert_check temp;
	temp)
      (* todo implemented correctly *)

    let is_possible pss =
      (not (Z.is_zero (SS.stateset_size pss.ss))) &&
	(pss.est.pmax >/ qzero) &&
	(pss.est.mmax >/ qzero) &&
	(pss.est.smax >! zzero)

    let _intersect_of_split pss ssinter sizeinter sizesidepss = (* !!! untested *)
      let est = pss.est in
      let newpmin = est.pmin in
      let newpmax = est.pmax in
      let newsmin = Z.max (est.smin -! sizesidepss) zzero in
      let newsmax = Z.min est.smax sizeinter in
      let temp =
	{ss = ssinter;
	 est = {pmin = newpmin;
		pmax = newpmax;
		smin = newsmin;
		smax = newsmax;
		mmin =
	     qmax
	       (newpmin */ (Q.from_z newsmin))
	       (est.mmin -/ (est.pmax */ (Q.from_z (Z.min est.smax sizesidepss))));
		mmax =
	     qmin
	       (newpmax */ (Q.from_z newsmax))
	       (est.mmax -/ (est.pmin */ (Q.from_z (Z.max zzero (est.smin -! sizeinter)))))
	       }} in
	(_assert_check temp;
	 temp)

    let split_many_with_splitter pss vars splitter = (* !!! untested *)
      let (lssin, lssout) = SS.stateset_split pss.ss vars splitter in
      let inter_temp = (fun flag -> (fun ss ->
				       let ssinter = SS.stateset_intersect pss.ss ss in
				       let sizeinter = SS.stateset_size ssinter in
					 (flag, pss, ssinter, sizeinter))
		       ) in
      let sumin = (fun l -> List.fold_left (fun total (_, _, _, a) -> total +! a) zzero l) in
      let filter_flag = (fun which_flag -> fun (flag,  _) -> which_flag == flag) in
      let extract_ss = (fun (flag, ss) -> ss) in
      let pieces = List.append
	(List.map (inter_temp 0) lssin)
	(List.map (inter_temp 1) lssout) in
      let rec pieces_intersector =
	(fun (rest, size_so_far, ret) ->
	   match rest with
	     | [] -> ret
	     | (flag, ss, ssinter, sizeinter) :: t ->
		 (pieces_intersector
		    (t,
		     size_so_far +! sizeinter,
		     (flag,
		      _intersect_of_split ss ssinter sizeinter (size_so_far +! (sumin t))) :: ret))) in
      let pieces2 = pieces_intersector (pieces, zzero, []) in
	((List.map extract_ss (List.filter (filter_flag 0) pieces2)),
	 (List.map extract_ss (List.filter (filter_flag 1) pieces2)))

    let split_many pss alexp =
      let vars = Lang.collect_vars_lexp alexp in
      let splitter = SS.stateset_make_splitter pss.ss alexp in
	split_many_with_splitter pss vars splitter

    let split (pss: pstateset) (alexp: lexp): (pstateset * pstateset) = 
      let (lssin, lssout) = split_many pss alexp in
      let accfun = (fun accum apss -> abstract_plus accum apss) in
	(List.fold_left accfun (make_empty ()) lssin,
	 List.fold_left accfun (make_empty ()) lssout)

    (* note: the below function should only be used for setting a set of variables
       that is disjoint from the ones already defined *)
    let set_all pss sil = 
      {ss = SS.stateset_set_all pss.ss sil;
       est = pss.est}

    let make_splitter pss alexp = SS.stateset_make_splitter pss.ss alexp

    let project_single pss v =
      
      (*
	printf "projecting out %s\n" (Lang.varid_to_string v); flush stdout; 
	printf "\n------------------\n";
	print pss; printf "\n"; 
      *)

      let all_vars = SS.stateset_vars pss.ss in

      let (hmin, hmax) = SS.stateset_min_max_height pss.ss v in

      let remain_vars = list_subtract all_vars [v] in
      let new_ss = SS.stateset_on_vars pss.ss remain_vars in
      let original_size = SS.stateset_size pss.ss in
      let new_size = SS.stateset_size new_ss in
      let est = pss.est in
      let new_est = {
	pmin = est.pmin */ (Q.from_z (Z.max zone ((hmin -! original_size) +! est.smin)));
	pmax = est.pmax */ (Q.from_z (Z.min hmax est.smax));
	smin = qceil ((Q.from_z est.smin) // (Q.from_z hmax)); (* todo: check this *)
	smax = Z.min new_size est.smax;
	mmin = est.mmin;
	mmax = est.mmax
      } in
      let temp = 
	{ss = new_ss;
	 est = new_est} in

	(*
	printf "\n------------------\n";
	  print temp; printf "\n";
	  printf "\nh = [%s,%s]\n" (Z.to_string hmin) (Z.to_string hmax);
	  flush stdout; 
	*)

	_assert_check temp;
	temp

    let project pss vl =
      let all_vars = SS.stateset_vars pss.ss in
      let removed_vars = list_subtract all_vars vl in

	List.fold_left (fun apss vname -> project_single apss vname) pss removed_vars

      (*
      (*      printf "projecting to "; List.iter (fun v -> print_string v; print_string " ") vl; printf "\n";
	      print pss;*)
      (* todo: do this correctly *)

      let all_vars = SS.stateset_vars pss.ss in
      let removed_vars = list_subtract all_vars vl in
      let ss_pancake = SS.stateset_on_vars pss.ss removed_vars in
      let hmax = Z.max zone (SS.stateset_size ss_pancake) in (* todo: important optimize *)
      let hmin = zone in (* todo: critical optimize *) 
(*      let hmin = hmax in *)
      let ss = SS.stateset_on_vars pss.ss vl in
      let projsize = SS.stateset_size ss in
      let psize = SS.stateset_size pss.ss in
      let est = pss.est in
	(*	printf "pancake:";
		SS.print_stateset ss_pancake;
		printf "\n";
		printf "hmax = %s, hmin = %s\n" (Z.to_string hmax) (Z.to_string hmin);*)
      let newest = {
	pmin = est.pmin */ (Q.from_z (Z.max zone ((hmin -! psize) +! est.smin)));
	pmax = est.pmax */ (Q.from_z (Z.min hmax est.smax));
	smin = qceil ((Q.from_z est.smin) // (Q.from_z hmax)); (* todo: check this *)
	smax = Z.min projsize est.smax;
	mmin = est.mmin;
	mmax = est.mmax
      } in
	{ss = ss;
	 est = newest}
      *)
	  
    let vars pss = SS.stateset_vars pss.ss

    let enum pss = SS.stateset_enum pss.ss

    let enum_on_vars pss vars = SS.stateset_enum (SS.stateset_on_vars_nocomp pss.ss vars)

    let relative_entropy pss1 pss2 = 0.0 (* todo: implement *)

    let _opt_estimate_max_in_min_out est =
(*      (est.pmax, est.pmin */ (Q.from_z est.smin)) (* !!! todo: do the trick to get a better prob bound *) *)
      (est.pmax, est.mmin)

    let min_mass pss = pss.est.mmin

    let prob_max_in_min_out pss s = 
      let ss = SS.stateset_point s in
      let ssinter = SS.stateset_intersect pss.ss ss in
	if SS.stateset_is_empty ssinter then
	  (qzero, pss.est.mmin) 
	else
	  _opt_estimate_max_in_min_out pss.est

    let prob_max_min pss = 
      _opt_estimate_max_in_min_out pss.est

    let prob_scale pss scalar = {ss = pss.ss;
				 est = estimator_prob_scale pss.est scalar}

    let _hit_max pss = pss.est.mmin =/ qzero

    let prob_max_norm pss s =
      if not (is_possible pss) then qzero
      else if _hit_max pss then qone
      else (
	let (massin, massout) = prob_max_in_min_out pss s in
	  massin // massout)

    let max_belief pss =
      if not (is_possible pss) then qzero
      else if _hit_max pss then qone
      else (
	let (massin, massout) = _opt_estimate_max_in_min_out pss.est in
	  massin // massout)

    let stateset_hull pss = pss.ss
      (*
    let normalize pss =
      let est = pss.est in
      let mmin = est.mmin in
      let mmax = est.mmax in
      let newest =
	{pmin = est.pmin // mmax;
	 pmax = est.pmax // mmin;
	 smin = est.smin;
	 smax = est.smax;
	 mmin = est.mmin // mmax;
	 mmax = est.mmax // mmin}
      *)

  end;;
