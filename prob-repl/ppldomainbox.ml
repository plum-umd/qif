open Ppl_ocaml
open Pplutil
open Lang
open Gmp
open Gmp.Z.Infixes
open Gmp.Q.Infixes
open Gmputil
open Util
open Printf
open Option
open ExtArray

(*module Ppldomainbox: (Ppldomain.PPLDOMAIN_TYPE with type region = rational_box) = struct*)

type region = rational_box

let make_empty dims =
  ppl_new_Rational_Box_from_space_dimension dims Empty

let make_new dims =
  ppl_new_Rational_Box_from_space_dimension dims Universe

let make_point varlist =
  let poly = ppl_new_Rational_Box_from_space_dimension (List.length varlist) Universe in
  List.iter
    (fun (varid, varval) ->
      ppl_Rational_Box_add_constraint
	poly
	(Equal (Variable varid, Coefficient (Z.of_int varval))))
    varlist;
  poly

let make_rational_point varlist =
  let poly = ppl_new_Rational_Box_from_space_dimension (List.length varlist) Universe in
  List.iter
    (fun (varid, varval) ->
      let num = Q.get_num varval in
      let den = Q.get_den varval in
      ppl_Rational_Box_add_constraint
	poly
	(Equal (Times (den, Variable varid), Coefficient num)))
    varlist;
  poly

let region_of_constraints dims lclist =
  let r = ppl_new_C_Polyhedron_from_space_dimension dims Universe in (* !!! todo: better way to do this? *)
  List.iter (ppl_Polyhedron_add_constraint r) lclist;
  (*	ignore (ppl_Polyhedron_get_minimized_constraints r);*)
  let r = ppl_new_Rational_Box_from_C_Polyhedron r in
  r

let poly_of_region r = ppl_new_C_Polyhedron_from_Rational_Box r

let get_constraints r =
  ppl_Rational_Box_get_minimized_constraints r

let add_constraint r lc =
  ppl_Rational_Box_add_constraint r lc

let region_is_nonempty r =
  (*not (ppl_Rational_Box_is_empty r);;*)
  ppl_Rational_Box_contains_integer_point r;;

(*let of_constraints_poly r lc =
  if not (region_is_nonempty r) then r else begin
    let p1 = ppl_new_C_Polyhedron_from_Rational_Box r in
    ppl_Polyhedron_add_constraints p1 lc;
    ppl_new_Rational_Box_from_C_Polyhedron p1
  end*)

let of_constraints_poly r lc =
  if not (region_is_nonempty r) then r else begin
    let p1 = ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box r in
    ppl_Octagonal_Shape_mpq_class_add_constraints p1 lc;
    ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class p1
  end

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

let copy_region r = ppl_new_Rational_Box_from_Rational_Box r

let get_dimensions r = ppl_Rational_Box_space_dimension r

let add_dimensions r dims = ppl_Rational_Box_add_space_dimensions_and_embed r dims
let add_dimensions_and_set r dims =
  add_dimensions r (List.length dims);
  List.iter
    (fun (anum, aval) ->
      add_constraint r (Equal (Variable anum, Coefficient aval)))
    dims
      
let map_dimensions r dimsmap       = ppl_Rational_Box_map_space_dimensions r dimsmap
let remove_higher_dimensions r dim = ppl_Rational_Box_remove_higher_space_dimensions r dim
let duplicate_dimension r dim      = ppl_Rational_Box_expand_space_dimension r dim 1

let intersect_region_poly r p2 =
  let p1 = ppl_new_C_Polyhedron_from_Rational_Box r in
  ppl_Polyhedron_intersection_assign p1 p2;
  ppl_new_Rational_Box_from_C_Polyhedron p1

let intersect_regions_assign r1 r2    = 
      (*      printf "\n---\nintersecting:\n\t";
              print_region r1;
              printf "\nwith\n\t";
              print_region r2;
              printf "\n---\n";*)
  ppl_Rational_Box_intersection_assign r1 r2
let union_regions_assign r1 r2        = ppl_Rational_Box_upper_bound_assign r1 r2
let product_regions_assign r1 r2      = ppl_Rational_Box_concatenate_assign r1 r2

let regions_are_disjoint r1 r2 =
  ppl_Rational_Box_is_disjoint_from_Rational_Box r1 r2

let affine_image r avar alexp =
  ppl_Rational_Box_affine_image r avar alexp zone

let partition_regions r1 r2 =
  let (rinter, rout) = ppl_Rational_Box_linear_partition r1 r2 in
  (rinter, List.map ppl_new_Rational_Box_from_NNC_Polyhedron (pointset_get_disjuncts rout))

let regions_are_disjoint r1 r2 =
  ppl_Rational_Box_is_disjoint_from_Rational_Box r1 r2

let _bounds_of_box p = 
  let dims = ppl_Rational_Box_space_dimension p in
  let gens = ppl_Rational_Box_get_minimized_constraints p in
  let bounds = List.map (fun g ->
    match g with
      | Greater_Or_Equal (Variable d, Coefficient c) -> 
	(d, Q.from_z c)
      | Greater_Or_Equal (Times (z, Variable d), Coefficient c) ->
	(d, (Q.from_z c) // (Q.from_z z))
      | Equal (Times (z, Variable d), Coefficient c) ->
	(d, (Q.from_z c) // (Q.from_z z))
      | _ -> 
	printf "\n*** can't handle constraint: ";
	print_constraint g;
	printf "\n";
	flush stdout;
	failwith "unexpected constraint type")
    gens in
  let vmin = Array.make dims None in
  let vmax = Array.make dims None in
  List.iter (fun (d, b) ->
    if (is_none vmin.(d)) then
      vmin.(d) <- Some b;
    if (is_none vmax.(d)) then
      vmax.(d) <- Some b;
    if b < get vmin.(d) then
      vmin.(d) <- Some b;
    if b > get vmax.(d) then
      vmax.(d) <- Some b) bounds;
  (Array.map (fun v -> Z.to_int (qceil (get v))) vmin, 
   Array.map (fun v -> Z.to_int (qfloor (get v))) vmax)

let _mins_maxes p =
  let (vmin, vmax) = _bounds_of_box p in
  let vec_min = Array.to_list vmin in
  let vec_max = Array.to_list vmax in
  List.combine vec_min vec_max

let region_bounds p =
  let (vmin, vmax) = _bounds_of_box p in
  Array.map2 (fun i j -> (i,j)) vmin vmax

let enum_region p = (* !!! todo: this is completely untested *)
  if not (region_is_nonempty p) then [] else
    let minmax = _mins_maxes p in
    let ranges = List.map (fun (amin, amax) -> list_range amin amax) minmax in
    list_prod_list ranges

let region_size r =
  if not (region_is_nonempty r) then zzero else
    if (ppl_Rational_Box_space_dimension r) = 0 then zone else
      let minmax = _mins_maxes r in	  
      let temp = ref zone in
      List.iter (fun (amin, amax) -> temp := !temp *! ((Z.of_int amax) -! (Z.of_int amin) +! zone)) minmax;
      !temp
          
let region_min_max_height r vnum1 =
  if not (region_is_nonempty r) then (zzero, zzero) else
    if (ppl_Rational_Box_space_dimension r) = 0 then (zzero, zzero) else (* probably not necessary *)
      let minmax = _mins_maxes r in
      let (amin, amax) = List.nth minmax vnum1 in
      let temp = 
	((Z.of_int amax) -! (Z.of_int amin) +! zone) in
      (temp, temp)

(*end;;*)
