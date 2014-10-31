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
open Globals

module Ppldomainpoly: (PPLDOMAIN_TYPE with type region = polyhedron) =
  struct
    type region = polyhedron

    let make_empty dims =
      ppl_new_NNC_Polyhedron_from_space_dimension dims Empty

    let make_new dims =
      ppl_new_NNC_Polyhedron_from_space_dimension dims Universe

    let make_point varlist =
      let poly = ppl_new_NNC_Polyhedron_from_space_dimension (List.length varlist) Universe in
	List.iter
	  (fun (varid, varval) ->
	     ppl_Polyhedron_add_constraint
	       poly
	       (Equal (Variable varid, Coefficient (Z.of_int varval))))
	  varlist;
	poly

    let make_rational_point varlist =
      let poly = ppl_new_NNC_Polyhedron_from_space_dimension (List.length varlist) Universe in
	List.iter
	  (fun (varid, varval) ->
	     let num = Q.get_num varval in
	     let den = Q.get_den varval in
	     ppl_Polyhedron_add_constraint
	       poly
	       (Equal (Times (den, Variable varid), Coefficient num)))
	  varlist;
	poly

    let region_of_constraints dims lclist =
(*      let r = ppl_new_NNC_Polyhedron_from_space_dimension dims Universe in
	List.iter (ppl_Polyhedron_add_constraint r) lclist;
	r*)
      let r = ppl_new_NNC_Polyhedron_from_space_dimension dims Universe in (* !!! todo: better way to do this? *)
	List.iter (ppl_Polyhedron_add_constraint r) lclist;
	(*	ignore (ppl_Polyhedron_get_minimized_constraints r);*)
	let r = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron r in
	  r

    let poly_of_region r = r

    let get_constraints r =
      ppl_Polyhedron_get_minimized_constraints r

    let add_constraint r lc =
      ppl_Polyhedron_add_constraint r lc

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

    let copy_region r = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron r

    let region_is_nonempty r = ppl_Polyhedron_contains_integer_point r;;

    let region_size r =
      if (ppl_Polyhedron_space_dimension r) = 0 then zone else
	if region_is_nonempty r then
	  (let latte = (latte_of_poly r) in
	   let result = count_models latte in
	     result
	  )
	else zzero;;

    let get_dimensions r = ppl_Polyhedron_space_dimension r

    let add_dimensions r dims = ppl_Polyhedron_add_space_dimensions_and_embed r dims
    let add_dimensions_and_set r dims =
      add_dimensions r (List.length dims);
      List.iter
	(fun (anum, aval) ->
	   add_constraint r (Equal (Variable anum, Coefficient aval)))
	dims
      
    let map_dimensions r dimsmap       = ppl_Polyhedron_map_space_dimensions r dimsmap
    let remove_higher_dimensions r dim = ppl_Polyhedron_remove_higher_space_dimensions r dim
    let duplicate_dimension r dim      = ppl_Polyhedron_expand_space_dimension r dim 1

    let intersect_regions_assign r1 r2    = ppl_Polyhedron_intersection_assign r1 r2
    let intersect_region_poly r1 r2 =
      let r1 = copy_region r1 in
	intersect_regions_assign r1 r2;
	r1
    let union_regions_assign r1 r2        = ppl_Polyhedron_poly_hull_assign r1 r2
    let product_regions_assign r1 r2      = ppl_Polyhedron_concatenate_assign r1 r2

    let regions_are_disjoint r1 r2 =
      ppl_Polyhedron_is_disjoint_from_Polyhedron r1 r2

    let affine_image r avar alexp =
      ppl_Polyhedron_affine_image r avar alexp zone

    let partition_regions r1 r2 =
      let (rinter, rout) = ppl_Polyhedron_linear_partition r1 r2 in
	(rinter, pointset_get_disjuncts rout)

    let __region_min_max_height_points r points (vnum1: int) =
      let vnum2 = get_dimensions r in

      let heights = List.map
	(fun p ->
	   let vlist = Array.to_list (Array.mapi (fun i v -> (i, v)) p) in
	   let point_octa = make_rational_point vlist in
	     ppl_Polyhedron_unconstrain_space_dimension point_octa vnum1;
	     let r2 = copy_region r in
	       intersect_regions_assign r2 point_octa;
	       let bound_points = vectors_of_poly r2 in

	       match bound_points with
		 | p1 :: [] -> zone
		 | p1 :: p2 :: [] when p1.(vnum1) >=/ p2.(vnum1) ->
		     (*((qfloor p1.(vnum1)) -! (qceil p2.(vnum1))) +! zone*)
		     
		     (qfloor (p1.(vnum1) -/ p2.(vnum1))) +! zone

		 | p2 :: p1 :: [] when p1.(vnum1) >=/ p2.(vnum1) ->
		     (*((qfloor p1.(vnum1)) -! (qceil p2.(vnum1))) +! zone*)

		     (qfloor (p1.(vnum1) -/ p2.(vnum1))) +! zone
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
		let lout = (poly_maximize r le) in
		  (zone, lout +! zone)
	    ) in

	    (*printf "latte  min/max = %s %s\n" (Z.to_string lmin) (Z.to_string lmax);
	      printf "points min/max = %s %s\n" (Z.to_string pmin) (Z.to_string pmax);*)

	    if (pmin <! lmin) || (pmax > lmax) then
	      (
		printf "latte  min/max = %s %s\n" (Z.to_string lmin) (Z.to_string lmax);
		printf "points min/max = %s %s\n" (Z.to_string pmin) (Z.to_string pmax);
		raise (General_error "latte based heights were better")
	      ));
	(pmin, pmax)

    let _region_min_max_height_points =
      let h = Hashtbl.create 1024 in	
	fun r vnum1 ->
	  let poly = poly_of_region r in 
	  let points = vectors_of_poly poly in
	  let points_hash = (latte_of_poly poly) ^ " " ^ (string_of_int vnum1) in
	    try 
	      Hashtbl.find h points_hash
	    with
	      | Not_found ->
		  let ret = __region_min_max_height_points r points vnum1 in
		    Hashtbl.replace h points_hash ret;
		    ret
    ;;

    let _region_min_max_height_latte r vnum1 =
      let vnum2 = get_dimensions r in
      let r = copy_region r in
      let le = Minus(Variable (vnum1), Variable (vnum2)) in
	duplicate_dimension r vnum1;
	let lout = (poly_maximize r le) in
	  (zone, lout +! zone)

    let region_min_max_height r vnum1 =
      (if !Globals.use_latte_minmax
       then _region_min_max_height_latte
       else _region_min_max_height_points) r vnum1

    let regions_are_disjoint r1 r2 =
      ppl_Polyhedron_is_disjoint_from_Polyhedron r1 r2

    let _point_of_list (point: int list) = 
      make_point
	(pair_second
	   (List.fold_left (fun (i, a) p -> (i+1,
					     ((i, p) :: a))) (0, []) point))

    let _is_in_region r point =
      let point_region = _point_of_list point in
	intersect_regions_assign point_region r;
	not (ppl_Polyhedron_is_empty point_region)

    let enum_region p =
      let vs = List.map Array.to_list (vectors_of_poly p) in
      let (vec_min, vec_max) = list_of_lists_bounds vs in
      let minmax = list_zip vec_min vec_max in
      let ranges = List.map (fun (amin, amax) -> list_range (Z.to_int (qceil amin)) (Z.to_int (qfloor amax))) minmax in
	List.filter (_is_in_region p) (list_prod_list ranges)

end;;
