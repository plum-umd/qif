(* a lot of stuff taken from the ocaml test program at ppl-0.10.2/interfaces/OCaml/test/ppl_ocaml_generated_test.ml *)

open Printf
open Gmp
open Gmp_util
open Ppl_ocaml
open Lang
open Logical
open Util
open Printf
open Gmp.Z.Infixes
open Gmp.Q.Infixes
open Geo

type varmap = (Lang.varid, int) Bimap.bmap

let rec print_varmap (amap: varmap) =
  printf "{%s}" (String.concat ", "
		   (List.map (fun (k, v) -> sprintf "%s=%d" (Lang.varid_to_string k) v)
		      (Bimap.pairs amap)))
  
let poly_copy p = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron p;;

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

let rec string_of_linear_expression_with_map l (vmap: (int, Lang.varid) Hashtbl.t) =
  let r = fun a -> string_of_linear_expression_with_map a vmap in
    match l with 
      | Variable v -> Lang.varid_to_string (Hashtbl.find vmap v)
      | Coefficient c ->
	  (Z.to_string c)
      | Unary_Minus e ->
	  String.concat ""
	    ["-("; r e; ")"]
      | Unary_Plus e -> r e
      | Plus (e1, e2) ->
	  String.concat ""
	    ["("; r e1; " + "; r e2; ")"]
      | Minus (e1, e2) ->
	  String.concat ""
	    ["("; r e1; " - "; r e2; ")"]
      | Times (c, e) ->
	  String.concat ""
	    [(Z.to_string c); "*("; r e; ")"]

let rec print_linear_expression_with_map l vmap =
  print_string (string_of_linear_expression_with_map l vmap)

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

let rec string_of_constraint_with_map c vmap =
  let (s, le1, le2) = string_reln_of_constraint c in
    String.concat ""
      [
	string_of_linear_expression_with_map le1 vmap;
	s;
	string_of_linear_expression_with_map le2 vmap
      ]
;;

let rec print_constraint_with_map c vmap =
  print_string (string_of_constraint_with_map c vmap)
;;

let print_constraints cl = List.iter (fun c -> print_constraint c; printf " ") cl
;;

let string_of_constraints_with_map cl vmap = 
  String.concat "\t"
    (List.map (fun c -> string_of_constraint_with_map c vmap) cl)
;;

let print_constraints_with_map cl vmap =
  print_string (string_of_constraints_with_map cl vmap)

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
       | Coefficient (c) -> (if (Z.cmp c zzero <> 0) then raise (General_error "expected 0 coeff"))
       | Plus (le1, le2) -> inspector le1; inspector le2
       | Times (c, Variable (n)) -> Array.set v n (Q.from_z c)
       | _ -> raise (General_error "unexpected point linear expression form")) in
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
		 raise (General_error "unexpected generator type")
	       )
      )
      gens
;;

let enum_of_poly p =
  let vs = vectors_of_poly p in
  let (vec_min, vec_max) = list_of_lists_bounds (List.map Array.to_list vs) in
  let minmax = list_zip vec_min vec_max in
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

module LETERM: (TERMINAL_TYPE with type term = linear_constraint) = struct
  type term = linear_constraint
  let term_true = Equal (Coefficient zone,
			 Coefficient zone)
  let term_false = Equal (Coefficient zzero,
			  Coefficient zone)
  let string_of_term t = string_of_constraint t
  let print_term t = print_constraint t
  let term_negate t = 
    let pone e = Plus (e, Coefficient zone) in
      match t with
	| Less_Than (le1, le2) ->
	    [Greater_Or_Equal (le1, le2)]
	| Less_Or_Equal (le1, le2) ->
	    [Greater_Or_Equal (le1, pone le2)]
	| Equal (le1, le2) ->
	    [Less_Or_Equal (pone le1, le2); Greater_Or_Equal (le1, pone le2)]
	| Greater_Than (le1, le2) ->
	    [Less_Or_Equal (le1, le2)]
	| Greater_Or_Equal (le1, le2) ->
	    [Less_Or_Equal (pone le1, le2)]
end

module LE = Logical(LETERM)

type trans = (string * linear_expression)
type ppl_trans = (int * linear_expression)

let rec linear_expression_of_aexp vmap ae =
  let r = linear_expression_of_aexp vmap in
    match ae with
      | AEVar (id) -> 
	  (try Variable (Bimap.find vmap id)
	   with Not_found ->
	     print_varmap vmap;
	     raise (General_error (sprintf "variable [%s] not found in varmap" (Lang.varid_to_string id))))
      | AEInt (v) -> Coefficient (Z.of_int v)
      | AEBinop (("*", _), AEInt (c), exp2) -> Times (Z.from_int c, r exp2)
      | AEBinop (("*", _), exp2, AEInt c)    -> Times (Z.from_int c, r exp2)
      | AEBinop (b, exp1, exp2) ->
	  (match b with
	     | ("+", _) ->
		 Plus ((r exp1),
		       (r exp2))
	     | ("-", _) ->
		 Minus ((r exp1),
			(r exp2))
	     | (_, _) -> raise (General_error "todo: other operations"))
	      
let rec logical_of_lexp vmap alexp : LE.logical =
  let pone e = Plus (e, Coefficient (Z.of_int 1)) in
  let r = logical_of_lexp vmap in
  let re = linear_expression_of_aexp vmap in
    match alexp with
      | LEBool (0) -> LE.LFalse
      | LEBool (1) -> LE.LTrue
      | LEBinop (b, lexp1, lexp2) ->
	  (match b with
	     | ("or", _)  -> LE.LOr [(r lexp1); (r lexp2)]
	     | ("and", _) -> LE.LAnd [(r lexp1); (r lexp2)]
	     | _ -> raise (General_error "should not happen"))
      | LEReln (r, aexp1, aexp2) ->
	  (* note C_Polyhedron needs closed constraints, hence we convert > and < into >= +1 and +1 <= *)
	  (match r with
	     | ("<=", _) ->  LE.LTerm (Less_Or_Equal (re aexp1, re aexp2))
	     | ("<", _)  ->  LE.LTerm (Less_Or_Equal (pone (re aexp1), re aexp2))
	     | (">=", _) ->  LE.LTerm (Greater_Or_Equal (re aexp1, re aexp2))
	     | (">", _)  ->  LE.LTerm (Greater_Or_Equal (re aexp1, pone (re aexp2)))
	     | ("==", _) ->  LE.LTerm (Equal (re aexp1, re aexp2))
	     | ("!=", _) ->
		 let t1 = (re aexp1) in
		 let t2 = (re aexp2) in
		   LE.LOr ([LE.LTerm (Less_Or_Equal (pone t1, t2));
			    LE.LTerm (Greater_Or_Equal (t1, pone t2))])
	     | _ -> raise (General_error "should not happen"))
	  | _ -> raise (General_error "should not happen")

let trans_of_stmt (vmap: varmap) astmt =
  match astmt with
    | SAssign (name, varexp) -> (name, linear_expression_of_aexp vmap varexp)
    | _ -> raise (General_error "can only make transformation from assignment statements")

let rec build_varmap varlist =
  let firstnum = ref 0 in
  let tempmap: varmap = Bimap.create (2 * (List.length varlist)) in
    List.iter
      (fun varid ->
	 if not (Bimap.mem tempmap varid) then 
	   (Bimap.add tempmap varid !firstnum;
	    firstnum := !firstnum + 1))
      varlist;
    tempmap
      
let bounds_of_poly p = 
  (*let dims = ppl_Polyhedron_space_dimension p in*)
  let vs = List.map Array.to_list (vectors_of_poly p) in
  let (vec_min, vec_max) = list_of_lists_bounds vs in
    (Array.of_list vec_min, Array.of_list vec_max)

let rec dims_in_linear_expression le =
  match le with
    | Coefficient (c) -> []
    | Plus (le1, le2) -> list_unique
	(List.append
	   (dims_in_linear_expression le1)
	   (dims_in_linear_expression le2))
    | Times (c, Variable (n)) ->
	if not (Z.is_zero c) then [n] else []
    | _ -> raise (General_error ("unknown le in dims_in_le"))
	
let rec dims_in_constraint acons =
  let (le1, le2) = 
    match acons with
      | Less_Than (le1, le2) -> (le1, le2)
      | Less_Or_Equal (le1, le2) -> (le1, le2)
      | Greater_Than (le1, le2) -> (le1, le2) 
      | Greater_Or_Equal (le1, le2) -> (le1, le2)
      | Equal (le1, le2) -> (le1, le2) in
    list_unique
      (List.append
	 (dims_in_linear_expression le1)
	 (dims_in_linear_expression le2))

let bounding_size bounds =
  let (vmin, vmax) = bounds in
  let vec_min = Array.to_list vmin in
  let vec_max = Array.to_list vmax in
  let minmax =  list_zip vec_min vec_max in
  let temp = ref zone in
    List.iter (fun (amin, amax) -> temp := !temp *! ((Z.of_int amax) -! (Z.of_int amin) +! zone)) minmax;
    !temp

let point_of_generator d g =
  (match g with
     | Point (le, c) -> vector_of_point_linear_expression d le
     | _ -> raise (General_error "non-point generator given")
  )

let rec _lspec_row_of_le
    (a: ZVECTOR.t)
    (le: linear_expression)
    (zc: ZNUM.t) = (* todo: probably needs more testing *)
  let r = _lspec_row_of_le a in
    match le with
      | Variable (v) ->
	  Array.set a (v+1) (ZNUM.plus zc (Array.get a (v+1)))
      | Coefficient (zc2) ->
	  Array.set a 0 (ZNUM.plus (ZNUM.times zc2 zc) (Array.get a 0))
      | Unary_Minus (le1) ->
	  r le1 (ZNUM.times zc ZNUM.none)
      | Unary_Plus (le1) ->
	  r le1 zc
      | Plus (le1, le2) ->
	  r le1 zc;
	  r le2 zc
      | Minus (le1, le2) ->
	  r le1 zc;
	  r le2 (ZNUM.times zc ZNUM.none)
      | Times (zc2, le1) ->
	  r le1 (ZNUM.times zc zc2)
;;

let halfspace_of_le dims le : ZVECTOR.t =
  let a = Array.make (dims + 1) ZNUM.zero in
    _lspec_row_of_le a le ZNUM.one;
    a

let constraints_rid_equals cs =
  let czero = Coefficient zzero in
    List.fold_left
      (fun ret c ->
	 match c with
	   | Equal (le1, le2) ->
	       (Greater_Or_Equal (Minus (le1, le2), czero)) ::
		 (Greater_Or_Equal (Minus (le2, le1), czero)) ::
		 ret
	   | x -> x :: ret)
      [] cs

let constraints_find_redundant_dims dims cs =
  let removed_dims = Array.create dims false in
    List.iter 
      (fun c ->
	 ifdebug (printf "constraint: %s\n" (string_of_constraint c));
	 match c with
	   | Equal (le1, le2) ->
	       let temp = Array.create dims ZNUM.zero in
	       let temp1 = halfspace_of_le dims le1 in
	       let temp2 = halfspace_of_le dims le2 in
		 Array.iteri (fun i v -> if i > 0 then temp.(i-1) <- ZNUM.minus v temp2.(i)) temp1;
		 let ds = ref [] in
		   Array.iteri (fun i v -> if ((ZNUM.sign v) <> 0) then ds := i :: !ds) temp;
		   (match !ds with
		      | d1 :: d2 :: [] -> removed_dims.(if d1 > d2 then d1 else d2) <- true
		      | d1 :: [] -> removed_dims.(d1) <- true
		      | _ -> raise (General_error "non-octagon constraint given, don't like"))
	   | x -> ()) cs ;
    List.map
      pair_first
      (List.filter
	 pair_second
	 (Array.to_list (Array.mapi (fun i v -> (i, v)) removed_dims)))

let constraints_rid_equals_for_counting dims cs =
  let removed_dims = Array.create dims false in
  let new_conses = List.fold_left
    (fun ret c ->
       printf "constraint: %s\n" (string_of_constraint c);
       match c with
	 | Equal (le1, le2) ->
	     let temp = Array.create dims ZNUM.zero in
	     let temp1 = halfspace_of_le dims le1 in
	     let temp2 = halfspace_of_le dims le2 in
	       Array.iteri (fun i v -> if i > 0 then temp.(i-1) <- ZNUM.minus v temp2.(i)) temp1;
	       let ds = ref [] in
		 Array.iteri (fun i v -> if ((ZNUM.sign v) <> 0) then ds := i :: !ds) temp;
		 (match !ds with
		    | d1 :: d2 :: [] -> removed_dims.(if d1 > d2 then d1 else d2) <- true
		    | d1 :: [] -> removed_dims.(d1) <- true
		    | _ -> raise (General_error "non-octagon constraint given, don't like"));
		 ret
	 | x -> x :: ret)
    [] cs in
    (new_conses,
     List.map
       pair_first
       (List.filter
	  pair_second
	  (Array.to_list (Array.mapi (fun i v -> (i, v)) removed_dims))))
      
let halfspace_of_constraint dims c =
  match c with
    | Greater_Or_Equal (le1, Coefficient (coeff)) ->
	let a = halfspace_of_le dims le1 in
	  Array.set a 0 (Z.sub (Array.get a 0) coeff);
	  a
    | _ -> 
	print_constraint c;
	raise (General_error "unusual specification from PPL?, expected le >= c")

let plane_of_constraint dims c =
  match c with
    | Equal (le1, Coefficient (coeff)) ->
	let a = halfspace_of_le dims le1 in
	  Array.set a 0 (Z.sub (Array.get a 0) coeff);
	  a
    | _ -> 
	print_constraint c;
	raise (General_error "unusual specification from PPL?, expected le == c")

let constraints_of_poly p =
    ppl_Polyhedron_get_minimized_constraints p

let poly_dimensions p = ppl_Polyhedron_space_dimension p

let halfspaces_of_poly p =
  let cons = constraints_rid_equals (constraints_of_poly p) in
    List.map (halfspace_of_constraint (poly_dimensions p)) cons

let planes_of_poly p =
  let cons = (constraints_of_poly p) in
    List.map (plane_of_constraint (poly_dimensions p)) cons

(*
  let halfspaces_of_poly_for_counting p =
  let dims = (poly_dimensions p) in
  let (cons, removed) = constraints_rid_equals_for_counting dims (constraints_of_poly p) in
  (List.map (halfspace_of_constraint dims) cons, removed)
*)

let map_to_equal c =
  let (le1, le2) =
    (match c with
       | Less_Than (le1, le2) -> (le1, le2)
       | Less_Or_Equal (le1, le2) -> (le1, le2)
       | Greater_Than (le1, le2) -> (le1, le2)
       | Greater_Or_Equal (le1, le2) -> (le1, le2)
       | _ -> raise (General_error "didn't expect an equality here")) in
    Equal (le1, le2)

let get_boundary p =
  let cons = List.map map_to_equal (ppl_Polyhedron_get_minimized_constraints p) in
  let p = ppl_new_NNC_Polyhedron_from_constraints cons in
    planes_of_poly p

let halfspaces_and_points_of_poly_for_counting p =
  let dims = (poly_dimensions p) in
  let removed = constraints_find_redundant_dims dims (constraints_of_poly p) in
  let pr = point_removal_dim_map removed dims in
  let p = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron p in
    ppl_Polyhedron_map_space_dimensions p pr;
    let dims = ppl_Polyhedron_space_dimension p in
    let gens = ppl_Polyhedron_get_minimized_generators p in
    let points = List.map (point_of_generator dims) gens in
      (dims,
       halfspaces_of_poly p,
       points)
 
