open Ppl_ocaml
open Gmp
open Gmp.Z.Infixes
open Gmp.Q.Infixes
open Gmputil
open Util
open Printf

let ppl_version () = ppl_version ();;

let poly_copy p = ppl_new_C_Polyhedron_from_C_Polyhedron p;; 

let poly_is_disjoint p1 p2 = ppl_Polyhedron_is_disjoint_from_Polyhedron p1 p2;;

let poly_is_nonempty p = ppl_Polyhedron_contains_integer_point p;;
let poly_is_empty p = not (ppl_Polyhedron_contains_integer_point p);;

let rec string_of_linear_expression = function
  | Variable v ->
      String.concat ""
	["V("; string_of_int v; ")"]
  | Coefficient c ->
      (Z.to_string c)
  | Unary_Minus e ->
      String.concat ""
	["-("; string_of_linear_expression e; ")"]
  | Unary_Plus e -> string_of_linear_expression e
  | Plus (e1, e2) ->
      String.concat ""
	["("; string_of_linear_expression e1; " + "; string_of_linear_expression e2; ")"]
  | Minus (e1, e2) ->
      String.concat ""
	["("; string_of_linear_expression e1; " - "; string_of_linear_expression e2; ")"]
  | Times (c, e) ->
      String.concat ""
	[(Z.to_string c); "*("; string_of_linear_expression e; ")"]

let rec print_linear_expression le = print_string (string_of_linear_expression le)

let string_reln_of_constraint = function
  | Less_Than (le1, le2) -> ("<", le1, le2)
  | Less_Or_Equal (le1, le2) -> ("<=", le1, le2)
  | Equal (le1, le2) -> ("==", le1, le2)
  | Greater_Than (le1, le2) -> (">", le1, le2)
  | Greater_Or_Equal (le1, le2) -> (">=", le1, le2)

let rec string_of_constraint c = 
  let (s, le1, le2) = string_reln_of_constraint c in
    String.concat ""
      [
	string_of_linear_expression le1;
	s;
	string_of_linear_expression le2
      ]

let string_of_constraints cl =
  String.concat "\t"
    (List.map string_of_constraint cl)

let rec print_constraint c = print_string (string_of_constraint c)

let print_constraints cl = List.iter (fun c -> print_constraint c; printf " ") cl
;;

let print_generator g =
  (match g with
     | Line (le) -> printf "line: "; print_linear_expression le
     | Ray (le) -> printf "ray: "; print_linear_expression le
     | Point (le, c) -> printf "point: (%d) " (Z.to_int c); print_linear_expression le
     | Closure_Point (le, c) -> printf "closure point: (%d) " (Z.to_int c); print_linear_expression le);
  printf "\n"

let print_generators gl = List.iter print_generator gl;;

let print_grid_generator g =
  (match g with
     | Grid_Line (le) -> printf "grid_line: "; print_linear_expression le
     | Grid_Parameter (le, c) -> printf "grid_parameter: (%d)" (Z.to_int c); print_linear_expression le
     | Grid_Point (le, c) -> printf "grid_point: (%d) " (Z.to_int c); print_linear_expression le);
  printf "\n"

let print_grid_generators gl = List.iter print_grid_generator gl;;

let print_congruence (le1, le2, c) =
  printf "congruence (%d)\n" (Z.to_int c);
  printf "  le1: "; print_linear_expression le1; printf "\n";
  printf "  le2: "; print_linear_expression le2; printf "\n"
;;

let print_congruences cl =
  List.iter print_congruence cl;;

let pointset_get_disjuncts ps =
  let ret = ref [] in
  let it = ppl_Pointset_Powerset_NNC_Polyhedron_begin_iterator ps in
  let itend = ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator ps in
    while (not (ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equals_iterator it itend)) do
      let p = ppl_Pointset_Powerset_NNC_Polyhedron_get_disjunct it in
      (*let pc = ppl_new_C_Polyhedron_from_NNC_Polyhedron p in *)
	ret := p :: (! ret);
	ppl_Pointset_Powerset_NNC_Polyhedron_increment_iterator it
    done;
    ! ret
;;

let polies_are_disjoint p1 p2 =
  ppl_Polyhedron_is_disjoint_from_Polyhedron p1 p2

let hull_of_polies p1 p2 =
  let ret = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron p1 in
    ppl_Polyhedron_poly_hull_assign ret p2;
    ret

let vector_of_point_linear_expression dims le =
  let v = Array.make dims qzero in
  let rec inspector ale =
    (match ale with
       | Coefficient (c) -> (if (Z.cmp c zzero <> 0) then failwith "expected 0 coeff")
       | Plus (le1, le2) -> inspector le1; inspector le2
       | Times (c, Variable (n)) -> Array.set v n (Q.from_z c)
       | _ -> failwith "unexpected point linear expression form") in
    inspector le; v
;;

let vectors_of_poly p = 
  let p = ppl_new_C_Polyhedron_from_NNC_Polyhedron p in
  (* the above gets rid of closure points, there might be an issue somewhere that
     makes non closed polies appear, not sure if this really fixes it completely *)
  let dims = ppl_Polyhedron_space_dimension p in
  let gens = ppl_Polyhedron_get_minimized_generators p in
    List.map
      (fun g ->
	 match g with
	   | Point (le, c) -> let ret = vector_of_point_linear_expression dims le in
	       if not (c =! zone) then
		 (
		   (*raise (General_error "was not one");
		   *)
		  Array.iteri (fun i v -> Array.set ret i (v // (Q.from_z c))) ret);
	       ret
	   | _ ->
	       (
		 print_generator g;
		 failwith "unexpected generator type"
	       )
      )
      gens
;;

let enum_of_poly p =
  let vs = vectors_of_poly p in
  let (vec_min, vec_max) = list_of_lists_bounds (List.map Array.to_list vs) in
  let minmax = List.combine vec_min vec_max in
  let ranges = List.map (fun (amin, amax) ->
    list_range (Z.to_int (qceil amin)) (Z.to_int (qfloor amax))) minmax in
    list_prod_list ranges
;;

let poly_of_constraints dims lclist =
  let poly = ppl_new_NNC_Polyhedron_from_space_dimension dims Universe in
    List.iter (ppl_Polyhedron_add_constraint poly) lclist;
    poly

let constraints_reduce dims cl =
  let poly = poly_of_constraints dims cl in
    ppl_Polyhedron_get_minimized_constraints poly
