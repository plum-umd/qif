open Globals
open Printf
open State
open Stateset
open Eval
open Util
open Lang
open Logical
open Latte

open Ppl_ocaml
open Ppl_util
open Ppldomain

open Gmp
open Gmp_util

open Gmp.Z.Infixes

module MakeStateset (P: PPLDOMAIN_TYPE): (STATESET_TYPE) =
struct
  type region = P.region
  type splitter = polyhedron

  type stateset = {
    bound: region;
    size: Z.t;
    dim: int;
    varmap: varmap
  }

  let stateset_vars ss = Bimap.keys ss.varmap

  let stateset_empty () = {
    bound = P.make_empty 0;
    size = zzero;
    dim = 0;
    varmap = Bimap.create 10
  }

  let _var_lookup ss varid = Bimap.find ss.varmap varid

  let stateset_new varlist = {
    bound = P.make_new (List.length varlist);
    size = zone; (* ambiguous here, really we are representing 0 dimensions at first *)
    dim = (List.length varlist);
    varmap = build_varmap varlist
  }
   
  let stateset_point astate = 
    let varlist = List.map Util.pair_first (astate#canon) in
    let aregion = P.make_point
      (list_zip
	 (list_range 0 ((List.length varlist) - 1))
	 (List.map
	    (fun varid -> astate#get varid)
	    varlist)) in
    let vmap = build_varmap varlist in
      {bound = aregion;
       size = zone;
       dim = (List.length varlist);
       varmap = vmap}

  let stateset_size aset = aset.size

  let stateset_is_empty aset1 = Z.is_zero (stateset_size aset1) 
  let stateset_is_nonempty aset1 = not (Z.is_zero (stateset_size aset1))

  let _stateset_of_region p vmap =
    {bound = p;
     dim = Bimap.length vmap;
     size = P.region_size p;
     varmap = vmap}

  let _stateset_of_region_nocomp p vmap =
    {bound = p;
     dim = 0;
     size = zzero;
     varmap = vmap}

  let _print_region p =
    P.print_region p

  let _print_region_with_map p vmap =
    P.print_region_with_map p vmap

  let print_stateset (aset: stateset) : unit =
    printf "Stateset of %s states, %d dimensions"
      (Z.to_string (stateset_size aset))
      (P.get_dimensions aset.bound);
    printf "\n\t\t\t";
    _print_region_with_map aset.bound (Bimap.get_bmap aset.varmap)

  let stateset_uniform varid blower bupper =
    {bound = P.make_range blower bupper;
     dim = 1;
     size = Z.add (Z.sub bupper blower) zone;
     varmap = build_varmap [varid]}
  ;;

  let stateset_copy (ss: stateset) : stateset =
    {bound = P.copy_region ss.bound;
     dim = ss.dim;
     size = ss.size;
     varmap = Bimap.copy ss.varmap}

  let _stateset_extend_and_bound_dimensions ss1 (vars: Lang.varid list) =
    let dims = ref ss1.dim in
      List.iter
	(fun avar ->
	   (if not (Bimap.mem ss1.varmap avar) then
	      raise (General_error ("conditioned on undefined variable " ^ (Lang.varid_to_string avar))))
	) vars;
      {bound = ss1.bound;
       dim = ! dims;
       size = P.region_size ss1.bound;
       varmap = ss1.varmap}

  let _stateset_extend_dimensions ss1 vars =
    let dims = ref ss1.dim in
      List.iter
	(fun avar ->
	   if not (Bimap.mem ss1.varmap avar) then
	     (Bimap.add ss1.varmap avar (!dims);
	      P.add_dimensions ss1.bound 1;
	      dims := (!dims) + 1
	     )
	)
	vars;
      {bound = ss1.bound;
       dim = !dims;
       size = ss1.size; (* technically might be unbounded, but assumed to not stay long in this state *)
       varmap = ss1.varmap}

  let _stateset_redimension ss1 ss2 =
    let vars1 = Bimap.keys ss1.varmap in
    let vars2 = Bimap.keys ss2.varmap in
    let ss1 = _stateset_extend_dimensions ss1 vars2 in
    let ss2 = _stateset_extend_dimensions ss2 vars1 in
    let amap = List.map
      (fun varid ->
	 let varnum2 = Bimap.find ss2.varmap varid in
	 let temp = (Bimap.find ss1.varmap varid, varnum2) in
	   Bimap.add ss1.varmap varid varnum2; temp)
      (Bimap.keys ss1.varmap) in
      P.map_dimensions ss1.bound amap;
      (ss1, ss2)
      
  let _check_consistency ss =
    if (not (P.get_dimensions ss.bound = ss.dim)) ||
      (not (ss.dim = Bimap.length ss.varmap)) then
	(print_stateset ss; printf "\n";
	 printf "space_dimension = %d\n" (P.get_dimensions ss.bound);
	 printf "stateset dim = %d\n" ss.dim;
	 raise (General_error "inconsistant stateset")) else
	  ss

  let stateset_on_vars_nocomp ss vl =
    let ss = stateset_copy ss in
    let dim = ref 0 in
    let newvarmap = Bimap.create 8 in
    let amap = 
      List.map
	(fun varid ->
	   let (map_from, map_to) = ((Bimap.find ss.varmap varid), !dim) in
	     Bimap.add newvarmap varid map_to;
	     let temp = (map_from, map_to) in
	       dim := !dim + 1; temp) vl in
    let ss = stateset_copy ss in
      P.map_dimensions ss.bound amap;
      P.remove_higher_dimensions ss.bound !dim;
      Bimap.filter ss.varmap (fun k v -> List.mem k vl);
      {bound = ss.bound;
       dim = !dim;
       size = zzero;
       varmap = newvarmap}

  let stateset_on_vars ss vl =
    let ss = stateset_copy ss in
    let dim = ref 0 in
    let newvarmap = Bimap.create 8 in
    let amap = 
      List.map
	(fun varid ->
	   let (map_from, map_to) = ((Bimap.find ss.varmap varid), !dim) in
	     Bimap.add newvarmap varid map_to;
	     let temp = (map_from, map_to) in
	       dim := !dim + 1; temp) vl in
    let ss = stateset_copy ss in
      P.map_dimensions ss.bound amap;
      P.remove_higher_dimensions ss.bound !dim;
      Bimap.filter ss.varmap (fun k v -> List.mem k vl);
      {bound = ss.bound;
       dim = !dim;
       size = P.region_size ss.bound;
       varmap = newvarmap}

  let stateset_min_max_height ss v = (* (qone, qone) *)
    if (ss.size =! zone) then (zone, zone) else
      (* note: latte uses a different output format when trying to maximize over a region having only 1 point *)
      P.region_min_max_height ss.bound (_var_lookup ss v)

  (* todo: make a version of this that doesn't bother with anything but report size of intersection *)
  let stateset_intersect (ss1: stateset) (ss2: stateset) : stateset =
    let ss1 = stateset_copy ss1 in
    let ss2 = stateset_copy ss2 in
    let (ss1, ss2) = _stateset_redimension ss1 ss2 in
      P.intersect_regions_assign ss1.bound ss2.bound;
      _check_consistency 
	{bound = ss1.bound;
	 dim = ss1.dim;
	 size = P.region_size ss1.bound;
	 varmap = ss1.varmap}

  let _is_possible dims clist = not (List.mem (LETERM.term_false) clist)

  let _filter_false dims lclist =
    List.filter (fun clist -> (_is_possible dims clist)) lclist

  let _constraints_lists_reduce dims lclist =
    (_filter_false dims (List.map (constraints_reduce dims) lclist))

  let _disjunct_negations dims clist =
    _constraints_lists_reduce dims 
      (List.tl (list_prod_list (List.map (fun t -> (t :: (LETERM.term_negate t))) clist)))

  let _disjoint_negations dims tlist =
    List.map 
      (fun clist -> (constraints_reduce dims (lists_append clist)))
      (list_prod_list
	 (List.map (_disjunct_negations dims) tlist))

  let rec _disjoint_dnf_lists dims llist =
    match llist with
      | [] -> []
      | al :: [] -> [al]
      | al :: rest ->
	  List.append
	    (_filter_false dims
	       (List.map
		  (fun clist -> (constraints_reduce dims (List.append al clist)))
		  (_disjoint_negations dims rest)))
	    (_disjoint_dnf_lists dims rest)

  let _partition_regions_poly dims vmap alexp =
    let alogical = (logical_of_lexp vmap alexp) in
    let lin  = _disjoint_dnf_lists dims (_constraints_lists_reduce dims (LE.dnf_lists_of_logical alogical)) in
    let lout = _disjoint_dnf_lists dims (_constraints_lists_reduce dims (LE.dnf_lists_of_logical (LE.LNot alogical))) in
      (List.map (poly_of_constraints dims) lin,
       List.map (poly_of_constraints dims) lout)

  let _region_of_ss ss = ss.bound

  let stateset_split ss1 vars (sspins, sspouts) = 
    let ss1 = stateset_copy ss1 in
      (* let vars = Lang.collect_vars_lexp alexp in *)
    let ss1 = _stateset_extend_and_bound_dimensions ss1 vars in (* todo: why is this done? *)

    (*let (pins, pouts) = _partition_regions ss1.dim ss1.varmap alexp in *)

    (*
      let pins = List.map _region_of_ss sspins in
    let pouts = List.map _region_of_ss sspouts in
    *)

    let intersect_me =
      (fun region ->
	 let region = P.intersect_region_poly ss1.bound region in
	   {bound = region;
	    dim = ss1.dim;
	    size = P.region_size region;
	    varmap = ss1.varmap}) in
      (List.filter stateset_is_nonempty (List.map intersect_me sspins),
       List.filter stateset_is_nonempty (List.map intersect_me sspouts))

  let stateset_make_splitter ss1 alexp =
    let ss1 = stateset_copy ss1 in
    let vars = Lang.collect_vars_lexp alexp in 
    let ss1 = _stateset_extend_and_bound_dimensions ss1 vars in 

      (*    let ssmaker = (fun x -> _stateset_of_region_nocomp x ss1.varmap) in*)

    let (pins, pouts) = _partition_regions_poly ss1.dim ss1.varmap alexp in

      (*(List.map ssmaker pins,
       List.map ssmaker pouts) *)

	(pins, pouts)

  (* !!! untested !!! *)
  let stateset_addvar aset varid =
    if Bimap.mem aset.varmap varid then raise (General_error ("variable " ^ (varid_to_string varid) ^ " already defined"));
    let vmap = Bimap.copy aset.varmap in
    let newregion = P.copy_region aset.bound in
      Bimap.add vmap varid (aset.dim);
      P.add_dimensions newregion 1;
      P.add_constraint newregion (Equal
				    (Variable (aset.dim),
				     Coefficient zzero));
      {bound = newregion;
       size = aset.size;
       varmap = vmap;
       dim = aset.dim + 1}

  let stateset_transform aset astmt =
    let vmap = Bimap.copy aset.varmap in
    let (tvarname, tlexp) = trans_of_stmt vmap astmt in
    let dims = aset.dim in
    let newregion = P.copy_region aset.bound in
    let (tvar, dims) =
      if Bimap.mem vmap tvarname then
	(Bimap.find vmap tvarname, dims)
      else
	(Bimap.add vmap tvarname dims; 
	 P.add_dimensions newregion 1;
	 (dims, dims + 1)) in
      P.affine_image newregion tvar tlexp;
      {bound = newregion;
       size = P.region_size newregion;
       varmap = vmap;
       dim = dims}

  let _stateset_intersect_partition_polies (p1: region) (p2: region) =
    let (inter, part_p2_polies) = P.partition_regions p1 p2 in 
    let (inter, part_p1_polies) = P.partition_regions p2 p1 in
      (part_p1_polies,
       part_p2_polies,
       inter)

  let stateset_intersect_partition aset1 aset2 =
    let (part1, part2, inter) = _stateset_intersect_partition_polies aset1.bound aset2.bound in
    let ss_of_region = (fun p -> _stateset_of_region p aset1.varmap) in
      (List.map ss_of_region part1,
       List.map ss_of_region part2,
       ss_of_region inter)

  let statesets_union_on_vars_list_nocomp ssl vars = (* assumes identical and same ordered dimensions *)
    (*    printf "unioning statesets:\n";
	  List.iter (fun s -> printf "element:"; print_stateset s; printf "\n") ssl;*)
    let ssl = List.map (fun s -> stateset_on_vars_nocomp s vars) ssl in
    let ssfirst = (List.hd ssl) in
    let p = P.copy_region ssfirst.bound in
      List.iter (fun p2 -> P.union_regions_assign p p2) (List.map (fun ss -> ss.bound) (List.tl ssl));
      {bound = p;
       dim = ssfirst.dim;
       varmap = ssfirst.varmap;
       size = zzero}

  let statesets_union_list_nocomp ssl = (* assumes identical and same ordered dimensions *)
    let ssfirst = (List.hd ssl) in
    let p = P.copy_region ssfirst.bound in
      List.iter (fun p2 -> P.union_regions_assign p p2) (List.map (fun ss -> ss.bound) (List.tl ssl));
      {bound = p;
       dim = ssfirst.dim;
       varmap = ssfirst.varmap;
       size = zzero}

  let _statesets_hull ss1 ss2 =
    let ss1 = stateset_copy ss1 in (* todo: is this copy required here? *)
    let ss2 = stateset_copy ss2 in
    let (ss1, ss2) = _stateset_redimension ss1 ss2 in
    let dim = ss1.dim in
      P.union_regions_assign ss1.bound ss2.bound;
      let temp = 
	{bound = ss1.bound;
	 dim = dim;
	 size = P.region_size ss1.bound;
	 varmap = ss1.varmap} in
	temp

  let _statesets_intersect_partition_hull dim regionl =
    let ptemp = P.make_empty dim in
      List.iter
	(fun p -> P.union_regions_assign ptemp p) regionl;
      ptemp

  exception Break_loop

  let statesets_exact_intersections (ssl: (stateset * 'a) list): (('a list) list) =
    (* Takes a set of statesets, with each have some associated but
       unused data value, and produces a list (one for each disjoint
       region) of sets of data values that came from the input regions
       that they overlap. You can specify the data value as the region
       as well which will give you a list of (a set of regions) where
       each element of the list represents a disjoint region. *)
    
    let queue = ref (Queue.create ()) in
    let queue_copy = ref (Queue.create ()) in
    let queue_done = (Queue.create ()) in
      List.iter (fun (i, note) -> Queue.add (i.bound, [note]) !queue) ssl;
      while (not (Queue.is_empty !queue)) do
	let expandedref = ref false in
	let (p1temp, notes1temp) = Queue.pop !queue in
	let p1ref = ref p1temp in
	let notes1ref = ref notes1temp in
	  while (not (Queue.is_empty !queue)) do
	    (*printf "queue = %d, queue copy = %d, queue done = %d\n"
	      (Queue.length !queue)
	      (Queue.length !queue_copy)
	      (Queue.length queue_done);
	    printf "p1ref = \n"; P.print_region !p1ref;
	    printf "queue=\n";
	    List.iter (fun (p, n) -> printf "(%d): " (List.length n); P.print_region p; printf "\n") (list_of_queue !queue);
	    flush Pervasives.stdout;*)

	    let (p2, notes2) = Queue.pop !queue in
	      if P.regions_are_disjoint !p1ref p2 then
		Queue.add (p2, notes2) !queue_copy
	      else
		(let (inp1, inp2, inboth) = _stateset_intersect_partition_polies !p1ref p2 in
		   List.iter
		     (fun p -> Queue.add (p, !notes1ref) !queue_copy
		     ) inp1;
		   List.iter
		     (fun p -> Queue.add (p, notes2) !queue_copy
		     ) inp2;
		   p1ref := inboth;
		   notes1ref := List.append !notes1ref notes2;
		   expandedref := true)
	  done;
	  Queue.add !notes1ref queue_done;
	  (*printf "done (contains pieces from %d polies):\n" (List.length !notes1ref);
	  P.print_region !p1ref;*)
	  let queue_temp = !queue in
	    queue := !queue_copy;
	    queue_copy := queue_temp
      done;
      list_of_queue queue_done

  let _union_and_return r1 r2 =
    let r1 = P.copy_region r1 in
      P.union_regions_assign r1 r2;
      r1

  let statesets_approx_intersections (ssl: (stateset * 'a) list) =
    let queue = ref (Queue.create ()) in
    let queue_copy = ref (Queue.create ()) in
    let queue_done = (Queue.create ()) in
      List.iter (fun (i, note) -> Queue.add (i.bound, [note]) !queue) ssl;
      while (not (Queue.is_empty !queue)) do
	let expandedref = ref false in
	let (p1temp, notes1temp) = Queue.pop !queue in
	let p1ref = ref p1temp in
	let notes1ref = ref notes1temp in
	  while (not (Queue.is_empty !queue)) do
	    let (p2, notes2) = Queue.pop !queue in
	      if P.regions_are_disjoint !p1ref p2 then
		Queue.add (p2, notes2) !queue_copy
	      else
		(p1ref := _union_and_return !p1ref p2;
		 notes1ref := List.append !notes1ref notes2;
		 expandedref := true)
	  done;
	  (if !expandedref then
	     Queue.add (!p1ref, !notes1ref) !queue_copy
	   else 
	     Queue.add !notes1ref queue_done);
	  let queue_temp = !queue in
	    queue := !queue_copy;
	    queue_copy := queue_temp
      done;
      list_of_queue queue_done

  let statesets_intersect_partition (ssl1: (stateset * 'a) list) (ssl2: (stateset * 'a) list) :
      ((stateset * 'a) list) * ((stateset * 'a) list) * ((stateset * 'a * 'a) list) =
    (* Partition the set of input statesets A and B into three
       disjoint pieces: 1. those overlapping A, 2. those overlapping B,
       3. those overlapping both . *)

    match (ssl1, ssl2) with
      | ([], _) -> ([], ssl2, [])
      | (_, []) -> (ssl1, [], [])
      | (_, _) ->
    let tri_of_pair (ss, x) = (ss.bound, x, ss) in
    let region_of_pair (ss, x) = ss.bound in 
    let pl1 = List.map tri_of_pair ssl1 in
    let pl2 = List.map tri_of_pair ssl2 in
    let list_ref_non_empty lr = not (List.length !lr = 0) in
    let list_ref_pop lr = (let ret = List.hd !lr in
			     lr := List.tl !lr; ret) in
    let list_ref_push lr a = (lr := a :: !lr) in
    let list_ref_append lr l = (lr := List.append !lr l) in

    let vmap = (pair_first (List.hd ssl1)).varmap in
    let dim = (pair_first (List.hd ssl1)).dim in
      
    let hull1 = _statesets_intersect_partition_hull dim (List.map region_of_pair ssl1) in
    let hull2 = _statesets_intersect_partition_hull dim (List.map region_of_pair ssl2) in

      if P.regions_are_disjoint hull1 hull2 then
	(ssl1, ssl2, [])
      else
	let retA = ref [] in
	let retB = ref [] in
	let retC = ref [] in
	let ppA = ref pl1 in
	let ppB = ref pl2 in
	  while (list_ref_non_empty ppA) do
	    try 
	      let a = list_ref_pop ppA in
	      let ppBp = ref [] in
		while (list_ref_non_empty ppB) do
		  let b = list_ref_pop ppB in
		  let (al, bl, c) = _stateset_intersect_partition_polies (triple_first a) (triple_first b) in
		    if (P.region_is_nonempty c) then
		      (
			list_ref_push retC (c, triple_second a, triple_second b, triple_third a, triple_third b);
			list_ref_append ppA (List.map (fun x -> (x, triple_second a, triple_third a)) al);
			ppB := List.append
			  (List.append !ppB !ppBp)
			  (List.map (fun x -> (x, triple_second b, triple_third b)) bl);
			raise Break_loop
		      )
		    else
		      (
			list_ref_push ppBp b
		      )
		done;
		list_ref_push retA a;
		ppB := !ppBp
	    with Break_loop -> ()
	  done;
	  retB := !ppB;
	  let make_ret (p, x, sp) = (_stateset_of_region p vmap, x) in
	  let make_ret2 (p, x, y, sp1, sp2) = (_stateset_of_region p vmap, x, y) in
	    (List.map make_ret !retA,
	     List.map make_ret !retB,
	     List.map make_ret2 !retC)

  let stateset_prod ss1 ss2 =
    (* todo: make sure variables are disjoint *)
    let newregion = P.copy_region ss1.bound in
    let newvarmap = Bimap.copy ss1.varmap in
      P.product_regions_assign newregion ss2.bound;
      Bimap.iter
	(fun varid varval ->
	   Bimap.add newvarmap varid (varval + ss1.dim)
	)
	ss2.varmap;
      {bound = newregion;
       size = Z.mul ss1.size ss2.size (* todo: is this correct? *);
       dim = ss1.dim + ss2.dim;
       varmap = newvarmap}
;;

  let stateset_set_all ss sl =
    let new_region = P.copy_region ss.bound in
    let new_vmap = Bimap.copy ss.varmap in
    let new_vals = ref [] in
    let dims = ref ss.dim in
      List.iter
	(fun (avar, aval) ->
	   if not (Bimap.mem new_vmap avar) then
	     (Bimap.add new_vmap avar (!dims);
	      (*ppl_Region_add_space_dimensions_and_embed new_region 1;
		ppl_Region_add_constraint new_region
		(Equal (Variable (!dims), Coefficient (Z.of_int aval))); *)
	      new_vals := (!dims, Z.of_int aval) :: !new_vals;
	      dims := (!dims) + 1)
	   else
	     raise (General_error "attempting to set a variable that already exists in stateset")
	)
	sl;
      P.add_dimensions_and_set new_region !new_vals;
      {bound = new_region;
       dim = ! dims;
       size = ss.size;
       varmap = new_vmap}

  let stateset_given_state (ss: stateset) (astate: state) : stateset =
    let ssp = stateset_point astate in
      _check_consistency (stateset_intersect ssp ss)

  let stateset_enum ss : (state list) =
    (*let ssov = stateset_on_vars ss vl in *)
    let ssov = ss in
    let vecs = P.enum_region ssov.bound in
    let ids = List.sort compare (Bimap.vals (ssov.varmap)) in
    let names = List.map (fun index -> (Hashtbl.find (Bimap.get_bmap ssov.varmap) index)) ids in
      List.map
	(fun vec ->
	   let empty = new state_empty in
	     (empty#set_list (list_zip names vec);
	      empty)
	)
	vecs

  let stateset_union ss1 ss2 = _statesets_hull ss1 ss2

  let stateset_is_disjoint ss1 ss2 = P.regions_are_disjoint ss1.bound ss2.bound
end;;
