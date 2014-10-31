open State
open Ppl_ocaml
open Ppl_util
open Lang
open Latte
open Ppldomain
open Gmp
open Gmp.Z.Infixes
open Gmp_util
open Util
open Printf
open Maths
open Geo

module P  = Maths.P
module P2 = Maths.P2
module C = Maths.Rational

module Ppldomainocta: (PPLDOMAIN_TYPE with type region = octagonal_shape_mpz_class) =
  struct
    type region = octagonal_shape_mpz_class

    let make_empty dims =
      ppl_new_Octagonal_Shape_mpz_class_from_space_dimension dims Empty

    let make_new dims =
      ppl_new_Octagonal_Shape_mpz_class_from_space_dimension dims Universe

    let make_point varlist =
      let poly = ppl_new_Octagonal_Shape_mpz_class_from_space_dimension (List.length varlist) Universe in
	List.iter
	  (fun (varid, varval) ->
	     ppl_Octagonal_Shape_mpz_class_add_constraint
	       poly
	       (Equal (Variable varid, Coefficient (Z.of_int varval))))
	  varlist;
	poly

    let make_rational_point varlist =
      let poly = ppl_new_Octagonal_Shape_mpz_class_from_space_dimension (List.length varlist) Universe in
	List.iter
	  (fun (varid, varval) ->
	     let num = Q.get_num varval in
	     let den = Q.get_den varval in
	     ppl_Octagonal_Shape_mpz_class_add_constraint
	       poly
	       (Equal (Times (den, Variable varid), Coefficient num)))
	  varlist;
	poly

    let region_of_constraints dims lclist =
      let r = ppl_new_NNC_Polyhedron_from_space_dimension dims Universe in (* !!! todo: better way to do this? *)
	List.iter (ppl_Polyhedron_add_constraint r) lclist;
(*	ignore (ppl_Polyhedron_get_minimized_constraints r);*)
	let r = ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron r in
	  r

    let poly_of_region r = ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class r

    let get_constraints r =
      ppl_Octagonal_Shape_mpz_class_get_minimized_constraints r

    let add_constraint r lc =
      ppl_Octagonal_Shape_mpz_class_add_constraint r lc

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

    let copy_region r = ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class r

    let region_is_nonempty r = ppl_Octagonal_Shape_mpz_class_contains_integer_point r;;

    let get_dimensions r = ppl_Octagonal_Shape_mpz_class_space_dimension r

    let add_dimensions r dims = ppl_Octagonal_Shape_mpz_class_add_space_dimensions_and_embed r dims
    let add_dimensions_and_set r dims =
      add_dimensions r (List.length dims);
      List.iter
	(fun (anum, aval) ->
	   add_constraint r (Equal (Variable anum, Coefficient aval)))
	dims
      
    let map_dimensions r dimsmap       = ppl_Octagonal_Shape_mpz_class_map_space_dimensions r dimsmap
    let remove_higher_dimensions r dim = ppl_Octagonal_Shape_mpz_class_remove_higher_space_dimensions r dim
    let duplicate_dimension r dim      = ppl_Octagonal_Shape_mpz_class_expand_space_dimension r dim 1

    let intersect_region_poly r p2 =
      let p1 = ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class r in
	ppl_Polyhedron_intersection_assign p1 p2;
	ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron p1
    let intersect_regions_assign r1 r2    = 
      ppl_Octagonal_Shape_mpz_class_intersection_assign r1 r2
    let union_regions_assign r1 r2        = ppl_Octagonal_Shape_mpz_class_upper_bound_assign r1 r2
    let product_regions_assign r1 r2      = ppl_Octagonal_Shape_mpz_class_concatenate_assign r1 r2

    let regions_are_disjoint r1 r2 =
      ppl_Octagonal_Shape_mpz_class_is_disjoint_from_Octagonal_Shape_mpz_class r1 r2

    let affine_image r avar alexp =
      ppl_Octagonal_Shape_mpz_class_affine_image r avar alexp zone

    let _partition_regions_c_octa_of_nnc_poly p =
      (* this is probably unsound except for octagons, but still unsure *)
      let cone = Coefficient (Z.of_int 1) in
      (*let o = ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron p in*)
      let clist = List.map
	(fun c ->
	   match c with
	     | Greater_Than (le1, le2) -> Greater_Or_Equal (le1, Plus (le2, cone))
	     | Less_Than (le1, le2) -> Less_Or_Equal (Plus (le1, cone), le2)
	     | x -> x
	) (ppl_Polyhedron_get_minimized_constraints p) in
	region_of_constraints (ppl_Polyhedron_space_dimension p) clist

    let partition_regions r1 r2 =
      let (rinter, rout) = ppl_Octagonal_Shape_mpz_class_linear_partition r1 r2 in
	(rinter, List.map _partition_regions_c_octa_of_nnc_poly (pointset_get_disjuncts rout))

    let region_min_max_height r (vnum1: int) =
      let vnum2 = get_dimensions r in

      let points = vectors_of_poly (poly_of_region r) in
      let heights = List.map
	(fun p ->
	   let vlist = Array.to_list (Array.mapi (fun i v -> (i, v)) p) in
	   let point_octa = make_point vlist in
	     ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimension point_octa vnum1;
	     let r2 = copy_region r in
	       intersect_regions_assign r2 point_octa;
	       let bound_points = vectors_of_poly (poly_of_region r2) in
		 match bound_points with
		   | p1 :: [] -> zone
		   | p1 :: p2 :: [] ->
		       ((qceil p1.(vnum1)) -! (qfloor p2.(vnum1))) +! zone
		   | _ -> raise (General_error "bad number of many bounding points"))
	(List.map intvector_of_qvector points) in
      let (pmin, pmax) = List.fold_left (fun (amin, amax) v ->
					   ((if v <! amin then v else amin),
					    (if v >! amax then v else amax))) (List.hd heights, List.hd heights) (List.tl heights) in

	ifdebug (
      let (lmin, lmax) = 
	(
	  let r = copy_region r in
	  let le = Minus(Variable (vnum1), Variable (vnum2)) in
	    duplicate_dimension r vnum1;
	    let lout = (poly_maximize (ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class r) le) in (* !!! todo: rid latte *)
	      (zone, lout +! zone)
	) in
	printf "latte  min/max = %s %s\n" (Z.to_string lmin) (Z.to_string lmax);
	printf "points min/max = %s %s\n" (Z.to_string pmin) (Z.to_string pmax);
	
	if (pmin <! lmin) || (pmax > lmax) then
	  (
	    (*printf "latte  min/max = %s %s\n" (Z.to_string lmin) (Z.to_string lmax);
	    printf "points min/max = %s %s\n" (Z.to_string pmin) (Z.to_string pmax);*)
	    raise (General_error "latte based heights were better")
	  ));
	(pmin, pmax)
	    
    let regions_are_disjoint r1 r2 =
      ppl_Octagonal_Shape_mpz_class_is_disjoint_from_Octagonal_Shape_mpz_class r1 r2

    let _bounds_of_box p = 
      let dims = ppl_Octagonal_Shape_mpz_class_space_dimension p in
      let gens = ppl_Octagonal_Shape_mpz_class_get_minimized_constraints p in
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

    let _get_related_pair acons =
      let do_pairs (c1, v1, c2, v2, b) =
	(
	  if (c1 >! zzero) &&
	    (c2 >! zzero) then
	      ([],
	       [
		 (v1, v2, P2.of_list [P.of_z_list [znone; b]])
(*		 ;
		 (v2, v1, P2.of_list [P.of_z_list [znone; b]])*)
		 
	       ]
	      )
	  else if 
	    (c1 <! zzero) &&
	      (c2 <! zzero) then
		([
		   (v1, v2, P2.of_list [P.none; P.monomial 0 (C.const (Q.from_z (Z.neg b)))])
		   (*; (*P.polynomial_of_z_list [znone; Z.neg b]); *)
		   (v2, v1, P2.of_list [P.none; P.monomial 0 (C.const (Q.from_z (Z.neg b)))])
		     (*P.polynomial_of_z_list [znone; Z.neg b])*)*)


		 ], [])
	  else if (c1 <! zzero) then 
	    (
	      [(v1, v2, P2.of_list [P.one; P.monomial 0 (C.const (Q.from_z (Z.neg b)))])], (*P.polynomial_of_z_list [zone; Z.neg b])],*)
	      [(v2, v1, P2.of_list [P.of_z_list [zone; b]])]
	    )
	  else
	    (
	      [(v2, v1, P2.of_list [P.one; P.monomial 0 (C.const (Q.from_z (Z.neg b)))])], (*(P.polynomial_of_z_list [zone; Z.neg b])],*)
	      [(v1, v2, P2.of_list [P.of_z_list [zone; b]])]
	    )
	) in
	match acons with
	  | Greater_Or_Equal (Plus
				((Times (c1, Variable v1)),
				 (Times (c2, Variable v2))),
			      Coefficient (b)) -> do_pairs (Z.neg c1, v1, Z.neg c2, v2, Z.neg b)
	  | Less_Or_Equal (Plus
			     ((Times (c1, Variable v1)),
			      (Times (c2, Variable v2))),
			   Coefficient (b)) -> do_pairs (c1, v1, c2, v2, b)
	  | _ -> 
	      print_constraint acons;
	      flush stdout;
	      raise (General_error "don't know how to handle pair")
	     
    let rec _find_related_dims dims conses : ('a list) * ('a list) =
      match conses with
	| [] -> ([], [])
	| c :: rest ->
	    let (lower_rest, upper_rest) = (_find_related_dims dims rest) in
	    let temp = dims_in_constraint c in
	      if (List.length temp) > 1 then
		let (lower, upper) = _get_related_pair c in
		  (List.append lower lower_rest,
		   List.append upper upper_rest)
	      else
		(lower_rest, upper_rest)

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

	
    let _region_size_polynomials r = raise Not_implemented
(*
      let p = poly_of_region r in
      let d = ppl_Polyhedron_space_dimension p in
      let gens = ppl_Polyhedron_get_minimized_generators p in
      let points = List.map (point_of_generator d) gens in
	ifdebug (
	  printf "original points:\n";
	  List.iter (fun p -> printf "vector: %s\n" (ZVECTOR.to_string p)) points);
	
	(*
	  let (halfspaces, removed_dims) = halfspaces_of_poly_for_counting p in
	  ifdebug (printf "******** will remove dimensions %s ****************\n" (String.concat " " (List.map string_of_int removed_dims));
	  printf "original halfspaces:\n";
	  List.iter (fun h -> printf "\thalfspace: %s\n" (ZVECTOR.to_string h)) halfspaces);
	  let newdims = d - (List.length removed_dims) in
	  count_of_convex_hull
	  newdims
	  (List.map (point_remove_dims removed_dims) points)
	  (List.map (halfplane_remove_dims removed_dims) halfspaces)
	*)

	let (newdims, halfs, points) = halfspaces_and_points_of_poly_for_counting p in
	  count_of_convex_hull
	    newdims
	    points
	    halfs

	(*let halfspaces = halfspaces_of_poly p in
	  count_of_convex_hull d (List.map (point_of_generator d) gens) halfspaces*)
*)
	  
    let _region_size_latte r =
      if (ppl_Octagonal_Shape_mpz_class_space_dimension r) = 0 then zone else
	if region_is_nonempty r then
	  (let latte = (latte_of_poly (ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class r)) in (* !!! todo: rid latte *)
	   let result = count_models latte in
	     result
	  )
	else zzero;;


    (*
    let rec _octa_poly_size p =
      (* !!! todo: recognize special cases (pyramid, equality, simplex) to use simpler counting methods *)

      let dims = ppl_Polyhedron_space_dimension p in

	ifdebug (
	  printf "\n\n\nfinding octa size of \n";
	  print_constraints (ppl_Polyhedron_get_minimized_constraints p);
	  printf "\n");

      if not (ppl_Polyhedron_contains_integer_point p) then zzero else
	
      let points = vectors_of_poly p in
      let (vmin, vmax) = bounds_of_poly p in
      let inpoint = _find_in_point (vmin, vmax) points in
	match inpoint with
	  | Some ((dim, splitpoint)) ->

	      ifdebug (printf "splitting by %d on dim %d\n" splitpoint dim;
		       flush stdout);

	      let p1 = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron p in
	      let p2 = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron p in
		(*printf "made polies\n"; flush stdout;*)

		ppl_Polyhedron_add_constraint p1
		  (Less_Or_Equal (Variable dim, Coefficient (Z.of_int splitpoint)));
		ppl_Polyhedron_add_constraint p2
		  (Greater_Or_Equal (Variable dim, Coefficient (Z.of_int (splitpoint + 1))));
		_octa_poly_size p1 +! _octa_poly_size p2
	  | None ->
	      let latte_size = _region_size_latte (ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron p) in
	      let conses         = ppl_Polyhedron_get_minimized_constraints p in
	      let related_dims   = _find_related_dims dims conses in
	      let (vmin, vmax) = bounds_of_poly p in
		ifdebug (
		  printf "vmin = %s\n" (String.concat " " (List.map string_of_int (Array.to_list vmin)));
		  printf "vmax = %s\n" (String.concat " " (List.map string_of_int (Array.to_list vmax))));
		
	      let piotr_size = 
		if ((List.length (pair_first related_dims)) = 0) &&
		  ((List.length (pair_second related_dims)) = 0) then
		  (
		    ifdebug (printf "using bounding box method\n"; flush stdout);
		    bounding_size (vmin, vmax)
		  )
		else 
		  simple_shape_size dims vmin vmax related_dims in
		
		if not (Z.equal latte_size piotr_size) then
		  raise (General_error ("simple region sizing was not correct, latte = " ^ (Z.to_string latte_size) ^ ", mine = " ^ (Z.to_string piotr_size)));

		piotr_size
		
    ;;*)

    let region_size r = (*_region_size_latte r*)
      if not (region_is_nonempty r) then zzero else
	(
	 if (get_dimensions r) = 0 then zone else
	   let piotr_size = (*_octa_poly_size (poly_of_region r)*)
	     (_region_size_polynomials r) in
	     ifdebug (
	       let latte_size = _region_size_latte r in
		 if not (Z.equal latte_size piotr_size) then
		   raise (General_error ("octa sizing was not correct, latte = " ^ (Z.to_string latte_size) ^ ", mine = " ^ (Z.to_string piotr_size)))
	     );
	     piotr_size)
    ;;
end;;
