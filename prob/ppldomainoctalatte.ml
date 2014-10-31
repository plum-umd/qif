open State
open Ppl_ocaml
open Ppl_util
open Lang
open Latte
open Ppldomain
open Gmp
open Gmp.Z.Infixes
open Gmp.Q.Infixes
open Gmp_util
open Util
open Printf
open Maths
open Geo
open Numeric

module P  = Maths.P
module P2 = Maths.P2
module C = Maths.Rational

module Ppldomainoctalatte: (PPLDOMAIN_TYPE with type region = octagonal_shape_mpq_class) =
  struct
    type region = octagonal_shape_mpq_class

    let make_empty dims =
      ppl_new_Octagonal_Shape_mpq_class_from_space_dimension dims Empty

    let make_new dims =
      ppl_new_Octagonal_Shape_mpq_class_from_space_dimension dims Universe

    let make_point varlist =
      let poly = ppl_new_Octagonal_Shape_mpq_class_from_space_dimension (List.length varlist) Universe in
	List.iter
	  (fun (varid, varval) ->
	     ppl_Octagonal_Shape_mpq_class_add_constraint
	       poly
	       (Equal (Variable varid, Coefficient (Z.of_int varval))))
	  varlist;
	poly

    let region_of_constraints dims lclist =
      let r = ppl_new_NNC_Polyhedron_from_space_dimension dims Universe in (* !!! todo: better way to do this? *)
	List.iter (ppl_Polyhedron_add_constraint r) lclist;
	let r = ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron r in
	  r

    let poly_of_region r = ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class r

    let get_constraints r =
      ppl_Octagonal_Shape_mpq_class_get_minimized_constraints r

    let add_constraint r lc =
      ppl_Octagonal_Shape_mpq_class_add_constraint r lc

    let make_range blower bupper = 
      let r = make_new 1 in
	add_constraint r
	  (Greater_Or_Equal (Variable 0, Coefficient blower));
	add_constraint r
	  (Less_Or_Equal (Variable 0, Coefficient bupper));
	r

    let string_of_region r =
      string_of_constraints (get_constraints r)
    let print_region r = print_string ((string_of_region r) ^ "\n")

    let string_of_region_with_map r vmap =
      string_of_constraints_with_map (get_constraints r) vmap
    let print_region_with_map r vmap = print_string ((string_of_region_with_map r vmap) ^ "\n")

    let make_rational_point varlist =
      (*printf "making rational point\n";*)
      let poly = ppl_new_Octagonal_Shape_mpq_class_from_space_dimension (List.length varlist) Universe in
	List.iter
	  (fun (varid, varval) ->
	     let num = Q.get_num varval in
	     let den = Q.get_den varval in
	     ppl_Octagonal_Shape_mpq_class_add_constraint
	       poly
	       (Equal (Times (den, Variable varid), Coefficient num)))
	  varlist;
	(*printf "result = %s\n" (string_of_region poly);*)
	poly

	  (*
    let make_rational_point_poly varlist =
      printf "making rational point\n";
      let poly = ppl_new_NNC_Polyhedron_from_space_dimension (List.length varlist) Universe in
	List.iter
	  (fun (varid, varval) ->
	     let num = Q.get_num varval in
	     let den = Q.get_den varval in
	       ppl_Polyhedron_add_constraint
		 poly
		 (Equal (Times (den, Variable varid), Coefficient num)))
	  varlist;
	printf "result = %s\n" (string_of_constraints (ppl_Polyhedron_get_minimized_constraints poly));
	poly
	  *)

    let copy_region r = ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class r

    let region_is_nonempty r = ppl_Octagonal_Shape_mpq_class_contains_integer_point r;;

    let get_dimensions r = ppl_Octagonal_Shape_mpq_class_space_dimension r

    let add_dimensions r dims = ppl_Octagonal_Shape_mpq_class_add_space_dimensions_and_embed r dims
    let add_dimensions_and_set r dims =
      add_dimensions r (List.length dims);
      List.iter
	(fun (anum, aval) ->
	   add_constraint r (Equal (Variable anum, Coefficient aval)))
	dims
      
    let map_dimensions r dimsmap       = ppl_Octagonal_Shape_mpq_class_map_space_dimensions r dimsmap
    let remove_higher_dimensions r dim = ppl_Octagonal_Shape_mpq_class_remove_higher_space_dimensions r dim
    let duplicate_dimension r dim      = ppl_Octagonal_Shape_mpq_class_expand_space_dimension r dim 1

      (*
	let _is_not_unit z1 =
	(Z.abs z1) >! zone

	let _rid_rational_bounds r =
	List.iter
	(fun c -> match c with
	| Less_Or_Equal (Times (c1, Variable v), Coefficient (c2)) when _is_not_unit c1 ->
	printf "ridding %s\n" (string_of_constraint c);
	if c1 >! zzero then
	add_constraint r (Less_Or_Equal (Variable v, Coefficient (Z.fdiv_q c2 c1)))
	else
	add_constraint r (Greater_Or_Equal (Variable v, Coefficient (Z.cdiv_q c2 c1)))
	| Greater_Or_Equal (Times (c1, Variable v), Coefficient (c2)) when _is_not_unit c1 ->
	printf "ridding %s\n" (string_of_constraint c);
	if c1 >! zzero then
	add_constraint r (Greater_Or_Equal (Variable v, Coefficient (Z.cdiv_q c2 c1)))
	else
	add_constraint r (Less_Or_Equal (Variable v, Coefficient (Z.fdiv_q c2 c1)))
	| _ -> ()
	)
	(ppl_Octagonal_Shape_mpq_class_get_minimized_constraints r);
	printf "result of rid rationals %s\n" (string_of_region r)
      *)

    let intersect_region_poly r p2 =
      let p1 = ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class r in
	ppl_Polyhedron_intersection_assign p1 p2;
	ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron p1


    let intersect_regions_assign r1 r2    = 
      ppl_Octagonal_Shape_mpq_class_intersection_assign r1 r2

    let union_regions_assign r1 r2        = ppl_Octagonal_Shape_mpq_class_upper_bound_assign r1 r2

    let product_regions_assign r1 r2      = ppl_Octagonal_Shape_mpq_class_concatenate_assign r1 r2

    let regions_are_disjoint r1 r2 =
      ppl_Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class r1 r2

    let affine_image r avar alexp =
      ppl_Octagonal_Shape_mpq_class_affine_image r avar alexp zone

    let _partition_regions_c_octa_of_nnc_poly p =
      (* this is probably unsound except for octagons, but still unsure *)

      let cone = Coefficient (Z.of_int 1) in
      (*let o = ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron p in*)
      let clist = List.map
	(fun c ->
	   match c with
	     | Greater_Than (le1, le2) -> Greater_Or_Equal (le1, Plus (le2, cone))
	     | Less_Than (le1, le2) -> Less_Or_Equal (Plus (le1, cone), le2)
	     | x -> x
	) (ppl_Polyhedron_get_minimized_constraints p) in
	region_of_constraints (ppl_Polyhedron_space_dimension p) clist

    let partition_regions r1 r2 =
      let (rinter, rout) = ppl_Octagonal_Shape_mpq_class_linear_partition r1 r2 in
	(rinter, List.map _partition_regions_c_octa_of_nnc_poly (pointset_get_disjuncts rout))

    let _region_min_max_height r points vnum1 =
      let vnum2 = get_dimensions r in

      let heights = List.map
	(fun p ->
	   let vlist = Array.to_list (Array.mapi (fun i v -> (i, v)) p) in

	   let point_octa = make_rational_point vlist in
	     ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimension point_octa vnum1;
	     intersect_regions_assign point_octa r;
	     let bound_points = vectors_of_poly (poly_of_region point_octa) in
	       match bound_points with
		 | p1 :: [] -> zone
		 | p1 :: p2 :: [] when p1.(vnum1) >=/ p2.(vnum1) ->
		     ((qfloor p1.(vnum1)) -! (qceil p2.(vnum1))) +! zone
		 | p2 :: p1 :: [] when p1.(vnum1) >=/ p2.(vnum1) ->
		     ((qfloor p1.(vnum1)) -! (qceil p2.(vnum1))) +! zone
		 | _ -> (
		     printf "vnum1=%d\n" vnum1;
		     printf "shape:%s\n" (string_of_region r);
		     printf "points:\n";
		     List.iter (fun p ->
				  printf "\tpoint: %s\n" (QVECTOR.to_string p))
		       points;
		     printf "bad point: %s\n" (QVECTOR.to_string p);
		     printf "point region: %s\n" (string_of_region point_octa);
		     printf "bounding points:\n";
		     List.iter (fun p -> printf "\tpoint: %s\n" (QVECTOR.to_string p)) bound_points;
			 
		     raise (General_error "bad number of bounding points")
		     )
	)
	points in
      let (pmin, pmax) = List.fold_left (fun (amin, amax) v ->
					   ((if v <! amin then v else amin),
					    (if v >! amax then v else amax))) (List.hd heights, List.hd heights) (List.tl heights) in

	ifdebug (
	  let (lmin, lmax) = 
	(
	  let r = copy_region r in
	  let le = Minus(Variable (vnum1), Variable (vnum2)) in
	    duplicate_dimension r vnum1;
	    let lout = (poly_maximize (poly_of_region r) le) in (* !!! todo: rid latte *)
	      (zone, lout +! zone)
	) in

	if (pmin <! lmin) || (pmax >! lmax) then
	  (
	    printf "latte  min/max = %s %s\n" (Z.to_string lmin) (Z.to_string lmax);
	    printf "points min/max = %s %s\n" (Z.to_string pmin) (Z.to_string pmax);
	    raise (General_error "latte based heights were better")
	  ));
	(pmin, pmax)

    let region_min_max_height =
      let h = Hashtbl.create 1024 in	
	fun r vnum1 ->
	  let points = vectors_of_poly (poly_of_region r) in
	    try Hashtbl.find h points with
	      | Not_found ->
		  let ret = _region_min_max_height r points vnum1 in
		    Hashtbl.replace h points ret;
		    ret
    ;;
	    
    let regions_are_disjoint r1 r2 =
      ppl_Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class r1 r2

    let _bounds_of_box p = 
      let dims = ppl_Octagonal_Shape_mpq_class_space_dimension p in
      let gens = ppl_Octagonal_Shape_mpq_class_get_minimized_constraints p in
      let vmin = Array.make dims 999999 in
      let vmax = Array.make dims (-999999) in (* todo: use max/min int *)
	List.iter
	  (fun g ->
	     match g with
	       | Greater_Or_Equal (Variable d, Coefficient c) -> 
		   let cint = Z.to_int c in
		     if cint > Array.get vmax d then Array.set vmax d cint;
		     if cint < Array.get vmin d then Array.set vmin d cint;
	       | Greater_Or_Equal (Times (z, Variable d), Coefficient c) ->
		   let cint = Z.to_int (Z.divexact c z) in
		     if cint > Array.get vmax d then Array.set vmax d cint;
		     if cint < Array.get vmin d then Array.set vmin d cint;
	       | Equal (Times (z, Variable d), Coefficient c) ->
		   let cint = Z.to_int (Z.divexact c z) in
		     if cint > Array.get vmax d then Array.set vmax d cint;
		     if cint < Array.get vmin d then Array.set vmin d cint;
	       | _ -> 
		   printf "\n*** can't handle constraint: ";
		   print_constraint g;
		   printf "\n";
		   flush stdout;
		   raise (General_error "unexpected constraint type"))
	  gens;
	(vmin, vmax)

    let enum_region p = (* !!! todo: this is completely untested *)
      let (vmin, vmax) = _bounds_of_box p in
      let vec_min = Array.to_list vmin in
      let vec_max = Array.to_list vmax in
      let minmax = list_zip vec_min vec_max in
      let ranges = List.map (fun (amin, amax) -> list_range amin amax) minmax in
	list_prod_list ranges

    let rec _is_point_in_bounds (vmin, vmax) apoint =
      let ret = ref None in
      let i = ref 0 in
	while (!i < Array.length vmin) && (is_none !ret) do
	  let amin = Array.get vmin !i in
	  let amax = Array.get vmax !i in
	  let point = Array.get apoint !i in
	    (if (amin < point) &&  (point < amax) then
	       (ret := Some (!i, point)));
	    i := !i+1
	done;
	!ret

    let rec _find_in_point (vmin, vmax) points =
      match points with
	| apoint :: restpoints -> 
	    let temp = _is_point_in_bounds (vmin, vmax) apoint in
	      if is_some temp then temp else
		_find_in_point (vmin, vmax) restpoints
	| [] -> None

    let _get_degrees dims pairs =
      let temp = Array.make dims 0 in
	List.iter
	  (fun (d1, d2) ->
	     Array.set temp d1 ((Array.get temp d1) + 1);
	     Array.set temp d2 ((Array.get temp d2) + 1))
	  pairs;
	temp
	    
    let _string_of_int_array a =
      String.concat " "
	(List.map string_of_int (Array.to_list a))

    let _region_size_latte r =
      if (ppl_Octagonal_Shape_mpq_class_space_dimension r) = 0 then zone else
	if region_is_nonempty r then
	  (let latte = (latte_of_poly (ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class r)) in (* !!! todo: rid latte *)
	   let result = count_models latte in
	     result
	  )
	else zzero;;

    let region_size r = (*_region_size_latte r*)
      if not (region_is_nonempty r) then zzero else
	(
	 if (get_dimensions r) = 0 then zone else
	   _region_size_latte r
	)

    ;;
end;;
