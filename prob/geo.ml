open Gmp
open Gmp.Z.Infixes
open Gmp_util
open Util
open Numeric
open Printf

let string_of_halfspace (h: ZVECTOR.t) =
  String.concat " " (List.map ZNUM.to_string (Array.to_list h))

let string_of_halfspaces (ls: ZVECTOR.t list) = 
  (String.concat
     "\n"
     (List.map string_of_halfspace ls)) 
;;

let dot_prod a1 a2 =
  if Array.length a1 <> Array.length a2 then
    raise (General_error "arrays of different sizes given, cannot do dot product")
  else
    let temp = ref 0 in
      Array.iteri (fun i v -> temp := !temp + v * a2.(i)) a1;
      !temp

let string_of_point p =
  String.concat " " (List.map Z.to_string (Array.to_list p));;

let is_point_on_halfplane p h =
(*  printf "is point %s on halfplane %s\n" (string_of_halfspace p) (string_of_halfspace h);*)
  let temp = ref zzero in
    Array.iteri (fun i v -> temp := !temp -! (v *! h.(i+1))) p;
    let res = if (Array.length p) = 1 then true else Z.equal !temp h.(0) in
      (*printf "answer = %s\n" (string_of_bool res);*)
      res

let point_removal_dim_map drem dims =
  let newdims = dims - (List.length drem) in
  let ret = Array.create newdims (0,0) in
  let j = ref 0 in
    for i = 0 to newdims - 1 do
      while List.mem !j drem do
	j := !j + 1
      done;
      ret.(i) <- (!j, i);
      j := !j + 1
    done;
    Array.to_list ret

let point_remove_dims drem p = 
  let newdims = (Array.length p) - (List.length drem) in
  let ret = Array.create newdims p.(0) in
  let j = ref 0 in
    for i = 0 to newdims - 1 do
      while List.mem !j drem do
	j := !j + 1
      done;
      ret.(i) <- p.(!j);
      j := !j + 1
    done;
    ret

let halfplane_remove_dims drem p =
  point_remove_dims (List.map (fun d -> d + 1) drem) p

module GEO
  (NM: NUMBER_TYPE)
  (*  (VM: VECTOR_TYPE with type nt = NM.t)
      (MM: MATRIX_TYPE with type nt = NM.t and type vt = VM.t)*) = struct
    module VM:(VECTOR_TYPE with type nt = NM.t) = VECTOR(NM)
    module MM:(MATRIX_TYPE with type nt = NM.t and type vt = VM.t) = MATRIX(NM)
    type nt = NM.t
    type vt = VM.t
    type mt = MM.t

    let orientation (all_points: vt list) =
      let n = (List.length all_points) in
      let m = (VM.size (List.hd all_points)) + 1 in
	if n <> m then
	  (
	    List.iter (fun p -> printf "point: %s\n" (VM.to_string p)) all_points;
	    raise (General_error "incompatible dimensions for orientation calculation"))
	else
	  let temp = MM.create n m in 
	    for i = 0 to n - 1 do
	      temp.(i).(0) <- NM.one;
	      MM.blit_vector (List.nth all_points i) 0 temp i 1 (n-1)
	    done;
	    (*printf "will find determinant of\n";
	      printf "%s\n" (MM.to_string temp);*)
	    MM.determinant temp

    let orientation_of (points: vt list) (apoint: vt) =
      orientation (apoint :: points)

    let rec facets_of_simplex (acone : vt list) =
      let num_points = List.length acone in
      let num_dims   = VM.size (List.hd acone) in
	ifdebug (printf "faceting %d in %d dimension(s)\n" num_points num_dims;
		 List.iter (fun p -> printf "faceting point: %s\n" (VM.to_string p)) acone);
	if num_points <> num_dims + 1 then
	  (raise (General_error (sprintf "%d points do not form a simplex in %d dimension(s)" num_points num_dims)));
	list_map_via_center
	(fun before center after ->
	   let temp = ref (List.append before after) in
	     (if (NM.sign (orientation_of !temp center)) > 0 then
		let f :: s :: rest = !temp in
		  temp := s :: f :: rest);
	     !temp
	) acone

    let rec split_into_simplices dims point_list =
      let partition_outsiders facets points =
	let points = ref points in
	  (List.map
	     (fun facet -> 
		let (outside, others) =
		  list_partition
		    (fun apoint -> let o = NM.sign (orientation_of facet apoint) in
		       (*(if o = 0 then (raise (General_error "not in general position")));*)
		       o > 0)
		    !points in
		  points := others;
		  (facet, outside))
	     facets) in
      
      (* assumes all points would lie on the convex hull *)
      if List.length point_list <= (dims+1) then [(1, point_list)]
      else
	let (cone, rest) = list_split_into (dims+1) point_list in
	  ifdebug (printf "cone has size %d\n" (List.length cone));
	  let facets = facets_of_simplex cone in
	  let facets_points = queue_of_list (partition_outsiders facets rest) in
	  let cones = ref (split_into_simplices dims cone) in
	    while not (Queue.is_empty facets_points) do
	      let (facet, outside) = Queue.pop facets_points in
		if List.length outside <> 0 then
		  (cones := List.append !cones
		     (List.map (fun (c, pl) -> (-1 * c, pl))
			(split_into_simplices dims facet));
		   let apoint :: rest = outside in
		   let next_cone = apoint :: facet in
		     cones := (1, next_cone) :: !cones;
		     let facets = facets_of_simplex next_cone in
		       List.iter
			 (fun (f, o) -> Queue.add (f, o) facets_points)
			 (partition_outsiders facets rest)
		  )
	    done;
	    !cones

    let rec split_cone dims source_point point_list =
      ifdebug
	(printf "\n\n\nsplit cone ================\n";
	 printf "dims=%d, source=%s, points=\n" dims (VM.to_string source_point);
	 List.iter (fun p -> printf "\t%s\n" (VM.to_string p)) point_list);

      let partition_outsiders facets points =
	let points = ref points in
	  (List.map
	     (fun facet -> 
		let (outside, others) =
		  list_partition
		    (fun apoint -> let o = NM.sign (orientation_of facet apoint) in
		       (*(if o = 0 then (raise (General_error "not in general position")));*)
		       o > 0)
		    !points in
		  points := others;
		  (facet, outside))
	     facets) in
      
	(* assumes all points would lie on the convex hull *)
	if List.length point_list <= dims then (
	  ifdebug (printf "=== returning base base ====\n");
	  [(1, source_point, point_list)])
	else
	  let (cone, rest) = list_split_into dims point_list in
	    (*printf "cone has size %d\n" (List.length cone);*)
	    let facets = (facets_of_simplex (source_point :: cone)) in
	    let facets_points = queue_of_list (partition_outsiders facets rest) in
	    let cones = ref (split_cone dims source_point cone) in
	      while not (Queue.is_empty facets_points) do

		ifdebug (printf "queue:\n";
		  List.iter (fun (f, p) -> 
		  printf "facet: %s\n" (String.concat ", " (List.map VM.to_string f)) ;
		  printf "points: %s\n\n" (String.concat ", " (List.map VM.to_string p)))
		  (list_of_queue facets_points);
		  printf "end queue\n");
		

		let (facet, outside) = Queue.pop facets_points in
		let facet = List.filter (fun p -> p != source_point) facet in
		  if List.length outside <> 0 then
		    (cones := List.append !cones
		       (List.map (fun (c, sp, pl) -> (-1 * c, sp, pl))
			  (*(split_cone dims source_point facet)*)
			  [(1, source_point, facet)]


		       );
		     let apoint :: rest = outside in
		     let next_cone = apoint :: facet in
		       cones := (1, source_point, next_cone) :: !cones;
		       let facets = (facets_of_simplex (source_point :: next_cone)) in

			 List.iter
			   (fun (f, o) -> Queue.add (f, o) facets_points;
			      if ((List.mem source_point f)) && ((List.length o) > 0)then
				raise (General_error "were points opposite source_point!")
			   )

			   (partition_outsiders facets rest)
		    )
	      done;
	      (*	      printf "returning non-base:\n";*)
	      (*	      List.iter (fun (w, p, pl) ->
			      printf "weight %d, point %s, points:\n" w (VM.to_string p);
			      List.iter (fun p -> printf "\t%s\n" (VM.to_string p)) pl)*)

	      !cones

end;;

module INTGEO = GEO(INT);;
module ZGEO = GEO(ZNUM);;
