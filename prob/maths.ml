open Util
open Gmp
open Gmp.Q.Infixes
open Gmp.Z.Infixes
open Gmp_util
open Printf
open Geo
open Numeric

let rec is_prime =
  let h = Hashtbl.create 32 in
  (fun p ->
     try let temp = Hashtbl.find h p in temp
     with Not_found ->
       let temp = 
	 match p with
	   | 1 -> false
	   | 2 -> true
	   | 3 -> true
	   | p ->
	       List.for_all
		 (fun p2 -> not (p mod p2 = 0))
		 (primes_upto (int_of_float (sqrt (float_of_int p)))) in
	 Hashtbl.replace h p temp;
	 temp)
and primes_upto =
  let h = Hashtbl.create 32 in
    (fun p ->
       try let temp = Hashtbl.find h p in temp
       with Not_found ->
	 let temp = 
	   if p < 2 then [] else
	     let last = ref 2 in
	     let ret = ref [2] in
	       while !last < p do
		 last := next_prime !last;
		 ret := !last :: !ret
	       done;
	       !ret in
	   Hashtbl.replace h p temp;
	   temp)
and next_prime =
  let h = Hashtbl.create 32 in
    (fun p ->
       try let temp = Hashtbl.find h p in temp
       with Not_found ->
	 let temp = 
	   match p with
	     | 2 -> 3
	     | p ->
		 let temp = ref (p + 2) in
		   while not (is_prime !temp) do
		     temp := !temp + 2
		   done;
		   !temp in
	   Hashtbl.replace h p temp;
	   temp)

let num_primes =
  let generated = ref (Array.of_list [2;3;5;7;11;13;17;19]) in
    fun how_many ->
      if how_many <= Array.length !generated then !generated
      else
	let biggest = Array.get !generated ((Array.length !generated) - 1) in
	let temp = ref (primes_upto biggest) in
	  while List.length !temp < how_many do
	    temp := primes_upto (2 * (List.hd !temp) + 1)
	  done;
	  generated := Array.of_list (List.rev !temp);
	  !generated

let rec factorial = 
  let h = Hashtbl.create 32 in
    fun (k: int) ->
      try
	let temp = Hashtbl.find h k in
	  temp
      with
	  Not_found ->
	    (let temp =
	       (if k < 2 then zone else
		  (Z.from_int k) *! (factorial (k - 1)))
	     in
	       Hashtbl.replace h k temp;
	       temp)

let ffactorial = memoize2 1024
  (fun (a: int) (b: int) ->
     List.fold_left (fun a i -> a *! (Z.from_int i)) zone (list_range (a - b + 1) a))

let choose = memoize2 1024
  (fun (a: int) (b: int) ->
     if b < a - b then
       Z.cdiv_q
	 (ffactorial a b)
	 (factorial b)
     else
       let b = a - b in
	 Z.cdiv_q
	   (ffactorial a b)
	   (factorial b))
    

(*
let rec bernoulli = memoize1 32 _bernoulli and
    _bernoulli m = 
  (if m =! zzero then qone else
     let mq = Q.from_z m in
       List.fold_left (fun a k -> (Q.from_z (choose m k)) */ ((bernoulli k) // (mq -/ (Q.from_z k) +/ qone)))
	 qzero (zlist_range zzero (m -! zone)))
*)

(* http://en.wikipedia.org/wiki/Bernoulli_number#Values_of_the_Bernoulli_numbers *)
let rec bernoulli =  
  let h = Hashtbl.create 32 in
    fun m ->
      (if m = 0 then qone else
	 try
	   let temp = Hashtbl.find h m in temp
	 with
	   | Not_found ->
	       let mq = Q.from_int m in
	       let temp =
		 qone -/ (qsum (fun k ->
				  let kq = Q.from_int k in
				    (Q.from_z (choose m k)) */
				      ((bernoulli k) // (mq -/ kq +/ qone)))
			    0 (m - 1)) in
		 Hashtbl.replace h m temp;
		 temp)
;;

(* http://en.wikipedia.org/wiki/Bernoulli_number#Values_of_the_Bernoulli_numbers *)
let gen_sum (m: int) (n: int) : Z.t =
  let mq = Q.from_int m in
  let nz = Z.from_int n in
    Q.get_num
      ((qsum (fun k ->
		(Q.from_z (choose (m + 1) k)) */
		  (bernoulli k) */ (Q.from_z (Z.pow_ui nz (m + 1 - k))))
	  0 m) // (mq +/ qone))
;;

(* counting lattice points stuffs *)
let standard_pyramid_size (d : int) (sidelength : int) =
  gen_sum
    (d - 1)
    (sidelength + 1)
  

(* http://books.google.com/books?id=tofDpuuEDu4C&pg=PA29&lpg=PA29&dq=counting+integer+points+in+a+standard+simplex&source=bl&ots=UWSUrFpX2f&sig=h4ToduCOHqC0rg2BsFYeAgmM5w4&hl=en&ei=iAmCTsjpB-Hs0gH7r71_&sa=X&oi=book_result&ct=result&resnum=4&ved=0CCwQ6AEwAw#v=onepage&q=counting%20integer%20points%20in%20a%20standard%20simplex&f=false *)      
let standard_simplex_size d sidelength =
  choose
    (d+sidelength)
    d
  
module type EdgeNoteType = sig
  type t
  val string_of_note: t -> string
end;;

module Graph (W: EdgeNoteType) = struct
  type note = W.t

  type graph = {num_nodes: int ref;
		in_lists: (int list) array;
		out_lists: (int list) array;
		edge_notes: note array;
		init_note: note
	       }

  let make num_nodes init_note = 
    {num_nodes = ref num_nodes;
     in_lists = Array.make num_nodes [];
     out_lists = Array.make num_nodes [];
     edge_notes = Array.make (num_nodes * num_nodes) init_note;
     init_note = init_note
    }

  let _edge_index g n1 n2 = !(g.num_nodes) * n2 + n1

  let set_note g n1 n2 w =
    Array.set g.edge_notes (_edge_index g n1 n2) w

  let get_note g n1 n2 =
    Array.get g.edge_notes (_edge_index g n1 n2)

  let connect (g: graph) (from_node : int) (to_node : int) note =
    Array.set g.in_lists to_node
      (from_node ::
	 (Array.get g.in_lists to_node));
    Array.set g.out_lists from_node
      (to_node ::
	 (Array.get g.out_lists from_node));
    set_note g from_node to_node note

  let out_degree g n = List.length (Array.get g.out_lists n)
  let in_degree g n = List.length (Array.get g.in_lists n)

  let in_list g n = Array.get g.in_lists n
  let out_list g n = Array.get g.out_lists n

  let lonely_nodes g =
    let ret = ref [] in
      Array.iteri (fun i l -> if (List.length l) = 0 then ret := i :: !ret)
	g.in_lists;
      !ret

  let famous_nodes g =
    let ret = ref [] in
      Array.iteri (fun i l -> if (List.length l) = 0 then ret := i :: !ret)
	g.out_lists;
      !ret

  let disconnect g from_node to_node =
    Array.set g.out_lists from_node
      (List.filter (fun n -> n <> to_node) (Array.get g.out_lists from_node));
    Array.set g.in_lists to_node
      (List.filter (fun n -> n <> from_node) (Array.get g.in_lists to_node));
    let temp = get_note g from_node to_node in
      set_note g from_node to_node g.init_note;
      temp

  let copy g =
    {num_nodes = ref !(g.num_nodes);
     in_lists = Array.copy g.in_lists;
     out_lists = Array.copy g.out_lists;
     edge_notes = Array.copy g.edge_notes;
     init_note = g.init_note
    }

  let has_edges g =
    List.exists
      (fun l -> (List.length l) > 0)
      (Array.to_list g.in_lists)

  let reverse_edge g n1 n2 =
    let temp = get_note g n1 n2 in
      ignore (disconnect g n1 n2);
      connect g n2 n1 temp

  let make_reverse g =
    copy
      {num_nodes = ref !(g.num_nodes);
       in_lists = g.out_lists;
       out_lists = g.in_lists;
       edge_notes = g.edge_notes;
       init_note = g.init_note
      }

  (* http://en.wikipedia.org/wiki/Topological_sorting *)
  let topological_sort g =
    let g = copy g in
    let l = Queue.create () in
    let s = queue_of_list (lonely_nodes g) in
      while not (Queue.is_empty s) do
	let n = Queue.pop s in
	  Queue.add n l;
	  List.iter (fun m ->
		       ignore (disconnect g n m);
		       if (List.length (Array.get g.in_lists m)) = 0 then Queue.add m s
		    )
	    (Array.get g.out_lists n);
      done;
      if has_edges g then raise (General_error "graph was cyclic");
      List.rev (list_of_queue l)

  let is_splitting g =
    List.exists (fun l -> (List.length l) > 1)
      (Array.to_list g.out_lists)


  let get_splitting_edges g = 
    let ret = ref [] in
      Array.iteri (fun v1 l -> if (List.length l) > 1 then ret := List.append (List.map (fun v2 -> (v1, v2)) l) !ret)
	g.out_lists;
      !ret

	(*
  let make_non_splitting g =
    let topo = topological_sort g in
      List.iter
	(fun n ->
	   if out_degree g n > 1 then
	     
	) topo*)

  let string_of_graph (g : graph) : string =
    let temp = ref 
      ("Graph of "
       ^ (string_of_int !(g.num_nodes))
       ^ " nodes:\n") in
      Array.iteri (fun i l ->
		     temp := !temp
		     ^ "\tnode "
		     ^ (string_of_int i)
		     ^ ": " 
		     ^ (String.concat " "
			  (List.map
			     (fun n ->
				(string_of_int n)
				^ "("
				^ (W.string_of_note (get_note g i n))
				^ ")") l))
		     ^ "\n")
	g.out_lists;
      !temp
end;;

let string_of_level l = match l with
  | 0 -> "x"
  | 1 -> "y"
  | 2 -> "z"
  | 3 -> "u"
  | 4 -> "v"
  | 5 -> "w"
  | _ -> raise (General_error "not enough dimension labels defined for polynomials")

module type Coeff = sig
  type t
  type st

  val level: int
  val const: Q.t -> t
  val unit: st -> t
  val monomial: int -> st -> t
  val shift: t -> int -> t
  val zero: t
  val one: t
  val none: t
  val of_int_list: (int list) -> t
  val add: t -> t -> t
  val mult_const: t -> Q.t -> t
  val div_const: t -> Q.t -> t
  val mult: t -> t -> t
  val sum: (int -> t) -> int -> int -> t
  val neg: t -> t

  val eval: t -> Z.t list -> Q.t
  val is_zero: t -> bool
  val to_string: t -> string
  val compose: t -> t -> int -> t
  val coeff: t -> int -> st
  val coeffs: t -> (int * st) list
end;;

module Rational: Coeff = struct
  type t = Q.t
  type st = Q.t

  let monomial i c =
    if i <> 0 then raise (General_error "cannot make non-unit rational")
    else c
  let shift t i = raise (General_error "cannot shift rational")
  let const c = c
  let unit c = c
  let level = -1
  let zero = qzero
  let one = qone
  let none = qnone
  let neg t = Q.neg t
  let of_int_list il = Q.from_int (List.hd il)
  let add c1 c2 = c1 +/ c2
  let mult_const c1 c2 = c1 */ c2
  let div_const c1 c2 = c1 // c2
  let mult c1 c2 = c1 */ c2
  let sum f a b = qsum f a b
  let eval c inl = c
  let is_zero c = Q.is_zero c
  let to_string c = Q.to_string c
  let compose c f i = c
  let coeff c i = c
  let coeffs c = [(0, c)]
end;;

module TERM = struct
  type t = Q.t * (int array)
  let resize s ((c, l): t) = (c, array_resize s 0 l)
  let max_dim ((c, l): t) = 
    let temp = ref (-1) in
      Array.iteri (fun i v -> if v <> 0 then temp := i) l;
      !temp
  let degree ((_, l): t) = 
    Array.fold_left (fun a v -> a + v) 0 l
  let dims ((c, l): t): int = Array.length l
  let copy ((c, l): t): t = (c, Array.copy l)
  let of_list (c: Q.t) (l: int list): t = (c, Array.of_list l)
  let negate (v, x) = (Q.neg v, x)
  let similar ((_, t1): t) ((_, t2) : t) : bool =
    if Array.length t1 <> Array.length t2 then raise (General_error "comparing terms of different sizes");
    let i = ref 0 in
      while (!i < Array.length t1) && ((Array.get t1 !i) = (Array.get t2 !i)) do
	i := !i+1
      done;
      !i = Array.length t1
  let add_similar ((c1, x): t) ((c2, _): t) = (c1 +/ c2, x)
  let add_similar_list (tl: t list) : t =
    List.fold_left add_similar (List.hd tl) (List.tl tl)
  let mult_scalar (s: Q.t) ((c1, x1): t) = (c1 */ s, x1)
  let mult (c1, x1) (c2, x2) : t =
    let temp = Array.create (max (Array.length x1) (Array.length x2)) 0 in
      Array.blit x1 0 temp 0 (Array.length x1);
      Array.iteri (fun i v -> Array.set temp i ((Array.get temp i) + v)) x2;
      (c1 */ c2, temp)

  let pow p ((c, l): t) =
    (qpow c p, Array.map (fun e -> e * p) l)

  let get_coeff (c, x) = c
  let get_exp d (c, x) =
    if Array.length x <= d then 0 else Array.get x d
  let get_exps ((c, x): t): int array = x
  let set_coeff c2 ((c, x): t) = (c2, x)
  let set_exp (d: int) (v: int) ((c, x): t) =
    let temp = Array.create (max (d+1) (Array.length x)) 0 in
      Array.blit x 0 temp 0 (Array.length x);
      Array.set temp d v;
      (c, temp)

  let mult_list (cl: t list) : t =
    List.fold_left (fun accum aterm -> mult accum aterm)
      (of_list qone [])
      cl

  let to_string (c, pl) =
    (qcoeff_to_string c)
    ^ (String.concat ""
	 (Array.to_list (Array.mapi (fun i e -> (if e = 0 then "" else sprintf "%s^%d" (string_of_level i) e)) pl)))

  let invert (c, p) =
    let temp = Array.copy p in
      Array.iteri (fun i v -> Array.set temp i ((-1) * v)) temp;
      (Q.inv c, temp)

  let is_constant ((c, p): t) =
    not (List.exists (fun i -> not (i = 0)) (Array.to_list p))

  let nexp_factor ((c, l): t) =
    let temp = Array.copy l in
      Array.iteri (fun i v -> if v > 0 then Array.set temp i 0) temp;
      (c, temp)

  let subst_monomials (dims: int) monos ((c, x): t) =
    let temp = ref 0 in
      Array.iteri (fun dim exp ->
		     if Array.length monos > dim then
		       (temp := !temp + (exp * (Array.get monos dim));
			Array.set x dim 0)
		  )
	x;
      if (Array.length x >= 1) then
	Array.set x 0 !temp

  let mono (l: int list): t = of_list qone l

  let is_zero ((c, x): t) = Q.is_zero c

  let zero = of_list qzero []
  let one = of_list qone []
  let none = of_list qnone []
end;;
      
(*
class type term_type = object
  method get_num_dims: int
  method set_coeff: Q.t -> unit
  method get_coeff: Q.t
  method set_exp: int -> int -> unit
  method get_exp: int -> int
end;;

class term: term_type = object (dims)
  val num_dims: int = dims
  val mutable coeff: Q.t = qzero
  val exps: int array = Array.create num_dims 0

  method get_num_dims = num_dims
  method set_coeff q = coeff <- q
  method get_coeff = coeff
  method set_exp i v = Array.set exps i v
  method get_exp i = Array.get exp i
end;;
*)

(*
class type poly_type = object
  method copy: poly_type
  method mult_assign: poly_type -> unit
  method add_assign: poly_type -> unit
  method sub_assign: poly_type -> unit
  method get_terms: TERM.t list
  method set_terms: TERM.t list -> unit
  method simplify: unit
  method subst_monomials: int -> int array -> unit
  method to_string: string
  method nexp_factor: TERM.t
  method mult_assign_list: poly_type list -> unit
end;;
*)

class poly = object (p1)
  val mutable max_vars: int = 16
  val mutable terms: TERM.t list = []

  method get_terms = terms

  method set_terms (nt: TERM.t list) = 
    if List.length nt > 0 then
      terms <- nt
    else
      terms <- [TERM.zero]

  method degree = list_max (List.map TERM.degree terms)

  method min_term_degree d = list_min (List.map (TERM.get_exp d) terms)

  method factor_out_assign d =
    let md = p1#min_term_degree d in
    let ret_term = TERM.set_exp d md TERM.one in
      terms <- List.map (fun t -> TERM.set_exp d ((TERM.get_exp d t) - md) t) terms;
      ret_term
    
  method terms_by_degree =
    let ret = Array.make (p1#degree+1) TERM.zero in
      List.iter (fun t -> ret.(TERM.degree t) <- t) terms;
      ret

  method coeffs_by_degree_hash =
    let ret = Hashtbl.create (p1#degree+1) in
      List.iter (fun t -> Hashtbl.replace ret (TERM.degree t) (TERM.get_coeff t)) terms;
      ret

  method copy : poly =
    let ret = new poly in
      ret#set_terms (List.map TERM.copy terms);
      ret

  method mult_term_assign t1 =
    terms <- List.map (fun t2 -> TERM.mult t1 t2) terms

  method mult_assign (p2: poly) =
    terms <-
      List.map (fun (t1, t2) -> TERM.mult t1 t2)
      (list_prod p1#get_terms p2#get_terms)

      (*
	method mult_assign_trunc p2 d dmax =
	let md1 = p1#min_term_degree d in
	let md2 = p2#min_term_degree d in
      *)

  method mult_list_assign pl = 
    List.iter (fun p2 -> p1#mult_assign p2) pl

  method add_assign (p2: poly) =
    terms <- List.append terms p2#get_terms;

  method add_list_assign (pl: poly list) =
    List.iter (fun p2 -> p1#add_assign p2) pl

  method sub_assign (p2: poly) =
    terms <- List.append terms (List.map TERM.negate p2#get_terms);

  method simplify_assign =
    let max_dim = list_max (List.map TERM.max_dim terms) in
      terms <- List.map (TERM.resize (max_dim + 1)) terms;
(*      List.iter (fun t -> printf "%s (%d), " (TERM.to_string t) (TERM.dims t)) terms;*)
      let temp = ref [] in
	while List.length terms > 0 do
	  let (t1: TERM.t) = List.hd terms in
	    terms <- List.tl terms;
	    let ((similar: TERM.t list), (rest: TERM.t list)) =
	      List.partition (fun t2 -> TERM.similar t1 t2) terms in
(*	      printf "comparing to %s\n" (TERM.to_string t1);
(*	      printf "similar are %s\n" (String.concat ", " (List.map TERM.to_string similar));
	      printf "not similar are %s\n" (String.concat ", " (List.map TERM.to_string rest));*)*)
	      terms <- rest;
	      temp := (TERM.add_similar_list (t1 :: similar)) :: !temp;
	done;
	p1#set_terms (List.filter (fun t -> not (TERM.is_zero t)) !temp);
(*	printf "\n\n";
	List.iter (fun t -> printf "%s (%d), " (TERM.to_string t) (TERM.dims t)) terms*)


	  (*
  method simplify_univariate_assign = raise Not_implemented
	  *)

  method subst_monomials_assign (dims: int) monos =
    List.iter (TERM.subst_monomials dims monos) terms

  method mult_assign_list (pl: poly list) =
    List.iter (fun p2 -> p1#mult_assign p2) pl

  method nexp_factor =
    TERM.mult_list
      (List.map (fun t -> TERM.invert (TERM.nexp_factor t))
	 (List.filter
	    (fun t -> not (TERM.is_constant t))
	    terms))

   method exp_binomial_assign (e: int) = 
     let a = List.nth terms 0 in
     let b = List.nth terms 1 in
       terms <- List.map (fun i ->
			    TERM.mult_scalar
			      (Q.from_z (choose e i))
			      (TERM.mult (TERM.pow i a) (TERM.pow (e - i) b)))
	 (list_range 0 e)

   method exp_binomial_trunc_assign (dmax: int) (e: int): unit = 
     let a = List.nth terms 0 in
     let b = List.nth terms 1 in
     let da = TERM.degree a in
     let db = TERM.degree b in
       if da = db && da * e > dmax then terms <- [TERM.zero]
       else
	 let i_start = ref 0 in
	 let i_end = ref e in
	   if da <> db then
	     (
	       let temp1 = Z.from_int (dmax - db * e) in
	       let temp2 = Z.from_int (da - db) in
		 if da - db > 0 then
		   i_end := Z.to_int (Z.fdiv_q temp1 temp2)
		 else if da - db < 0 then
		   i_start := Z.to_int (Z.cdiv_q temp1 temp2)
	     );
	   i_start := max 0 !i_start;
	   i_end := min e !i_end;
	   terms <-
	     if !i_start <= !i_end
	     then List.map (fun i ->
			      TERM.mult_scalar
				(Q.from_z (choose e i))
				(TERM.mult (TERM.pow i a) (TERM.pow (e - i) b)))
	       (list_range !i_start !i_end)
	     else
	       [TERM.copy TERM.zero];
	   ()


  method exp_assign e = 
    if List.length terms = 2 then p1#exp_binomial_assign e
    else
      (
	if e = 0 then
	  terms <- [TERM.of_list qone []]
	else
	  let p2: poly = p1#copy in
	    for i = 2 to e do
	      p1#mult_assign p2
	    done;
	    ()
      )

  method exp_trunc_assign dmax e = 
    if List.length terms = 2 then p1#exp_binomial_trunc_assign dmax e
    else (* todo *)
      (
	if e = 0 then
	  terms <- [TERM.of_list qone []]
	else
	  let p2: poly = p1#copy in
	    for i = 2 to e do
	      p1#mult_assign p2
	    done;
	    ()
      )

  method subst_assign (d: int) (p2: poly) : poly =
    let temp_pieces = ref [] in
      List.iter (fun t ->
		   let e = TERM.get_exp d t in
		     if e < 0 then raise (General_error "cannot subst into negative exponent")
		     else if e = 0 then temp_pieces := (new poly_of_terms [t]) :: !temp_pieces
		     else if e > 0 then 
		       let temp_term = TERM.set_exp d 0 t in
		       let temp_piece = p2#copy in
			 temp_piece#exp_assign e;
			 temp_piece#mult_term_assign temp_term;
			 temp_pieces := temp_piece :: !temp_pieces
		) terms;
      let ret = new poly_zero in
	ret#add_list_assign !temp_pieces;
	ret

  method subst_trunc_assign (d: int) (dmax: int) (p2: poly) : poly =
    let temp_pieces = ref [] in
      List.iter (fun t ->
		   let e = TERM.get_exp d t in
		   let temp_term = TERM.set_exp d 0 t in
		     if e < 0 then raise (General_error "cannot subst into negative exponent")
		     else if e = 0 then temp_pieces := (new poly_of_terms [temp_term]) :: !temp_pieces
		     else if e > 0 then 
		       let temp_piece = p2#copy in
			 temp_piece#exp_trunc_assign dmax e;
			 temp_piece#mult_term_assign temp_term;
			 temp_pieces := temp_piece :: !temp_pieces;
		) terms;
      let ret = new poly_zero in
	ret#add_list_assign !temp_pieces;
	ret
	
  method to_string = String.concat " " (List.map TERM.to_string terms)

end and poly_of_terms (new_terms: TERM.t list) = object (p)
  inherit poly
  initializer p#set_terms new_terms

end and poly_one = object (p)
  inherit poly_of_terms [TERM.of_list qone []]

end and poly_zero = object (p) 
  inherit poly_of_terms []

end;;

let plist_to_string pl =
  String.concat "" (List.map (fun p -> sprintf "(%s)" p#to_string) pl)
;;

class poly_rational num den = object (pr)
  val mutable num_polies: poly list = if List.length num = 0 then [new poly_of_terms [TERM.zero]] else num
  val mutable den_polies: poly list = if List.length den = 0 then [new poly_of_terms [TERM.zero]] else den
(*  val mutable den_polies: poly list = den*)

  method copy =
    new poly_rational
      (List.map (fun p -> p#copy) num_polies)
      (List.map (fun p -> p#copy) den_polies)

  method get_num = num_polies
  method get_den = den_polies

  method set_num pl =
    num_polies <- pl

  method set_den pl =
    den_polies <- pl

  method mult_assign p =
    num_polies <- p :: num_polies

  method div_assign p =
    den_polies <- p :: den_polies

  method subst_monomials_assign dims monos =
    List.iter (fun p -> p#subst_monomials_assign dims monos) num_polies;
    List.iter (fun p -> p#subst_monomials_assign dims monos) den_polies

  method clear_nexp_in_den_assign =
    let fp = ref (TERM.of_list qone []) in
      List.iter (fun (p: poly) ->
		   let fact = p#nexp_factor in
		     if TERM.is_constant fact then ()
		     else (p#mult_assign (new poly_of_terms [fact]);
			   fp := TERM.mult !fp fact); ())
	den_polies;
      List.iter (fun p -> p#mult_assign (new poly_of_terms [!fp])) num_polies

  method subst_assign d (p2: poly) = 
    num_polies <- List.map (fun (p1: poly) -> p1#subst_assign d p2) num_polies;
    den_polies <- List.map (fun (p1: poly) -> p1#subst_assign d p2) den_polies

  method subst_trunc_assign d dmax (p2: poly) =
    num_polies <- List.map (fun (p1: poly) -> p1#subst_trunc_assign d (2 * dmax) p2) num_polies;
    den_polies <- List.map (fun (p1: poly) -> p1#subst_trunc_assign d (2 * dmax) p2) den_polies

  method simplify_assign =
    let num = new poly_one in
    let den = new poly_one in
      (List.iter (fun p -> p#simplify_assign) num_polies);
      (List.iter (fun p -> p#simplify_assign) den_polies);
      num#mult_list_assign num_polies;
      den#mult_list_assign den_polies;
      num#simplify_assign;
      den#simplify_assign;
      num_polies <- [num];
      den_polies <- [den]

  method eval_at_zero_pole = (* see the latte paper, above example 7 *)
    let c = pr#copy in
    let num = (List.hd c#get_num) in
    let den = (List.hd c#get_den) in
    let den_factor = den#factor_out_assign 0 in
    let degree = 1 + (TERM.get_exp 0 den_factor) in
      ifdebug (printf "den = %s and %s\n" (TERM.to_string den_factor) (den#to_string));
      (*let a = Array.map TERM.get_coeff num#terms_by_degree in
      let b = Array.map TERM.get_coeff den#terms_by_degree in 
      let a = array_resize degree qzero a in
      let b = array_resize degree qzero b in*)
      let a = num#coeffs_by_degree_hash in
      let b = den#coeffs_by_degree_hash in
      let get_coeff h i =
	(try Hashtbl.find h i with
	   | Not_found -> Hashtbl.replace h i qzero;
	       qzero) in
	ifdebug (printf "a = %s\n" (String.concat "," (List.map (fun i -> Q.to_string (get_coeff a i)) (range degree)));
		 printf "b = %s\n" (String.concat "," (List.map (fun i -> Q.to_string (get_coeff b i)) (range degree)));
		 flush stdout);
      let c = Array.make degree qzero in
	c.(0) <- (get_coeff a 0) // (get_coeff b 0);
	for k = 1 to degree - 1 do
	  c.(k) <- ((get_coeff a k) -/ (qsum (fun i -> (get_coeff b i) */ c.(k - i)) 1 k)) // (get_coeff b 0)
	done;
	ifdebug (printf "c = %s\n" (String.concat "," (List.map Q.to_string (Array.to_list c))));
	c.(degree-1)

  method to_string =
    sprintf "%s / %s"
      (plist_to_string num_polies)
      (plist_to_string den_polies)
end;; 

module MPolynomial(C:Coeff) = struct
  type polynomial = C.t array
  type t = polynomial
  type st = C.t

  let zero = Array.make 1 C.zero
  let one  = Array.make 1 C.one
  let none = Array.make 1 C.none 

  let level = C.level + 1

  let const c = 
    let ret = Array.make 1 C.zero in
      Array.set ret 0 (C.const c);
      ret

  let coeff p i = Array.get p i

  let coeffs p = Array.to_list (Array.mapi (fun i c -> (i, c)) p)

  let degree p = (Array.length p) - 1

  let of_list pl : polynomial =
    Array.of_list (List.rev pl)

  let of_z_list pl : polynomial =
    Array.of_list
      (List.rev
	 (List.map (fun a -> C.const (Q.from_z a)) pl))

  let of_int_list pl : polynomial =
    Array.of_list
      (List.rev
	 (List.map (fun a -> C.const (Q.from_int a)) pl))

  let to_string p =
    let temp = ref [] in
      Array.iteri (fun i c ->
		     if not (C.is_zero c) then
		       temp := (
			 " +("
			 ^ (C.to_string c)
			 ^ ")"
			 ^ (if i = 0 then "" else " " ^ (string_of_level level) ^ "^" ^ (string_of_int i))
		       ) :: !temp)
	p;
      if (List.length !temp) = 0 then temp := ["0"];
      String.concat "" !temp

  let is_zero p = List.for_all (fun c -> C.is_zero c) (Array.to_list p)

  let mult_const p1 (c: Q.t) =
    Array.map (fun c2 -> C.mult_const c2 c) p1

  let mult_scalar p s = 
    Array.map (fun c -> C.mult c s) p

  let div_const p1 (c: Q.t) =
    Array.map (fun c2 -> C.div_const c2 c) p1

  let mult_power p1 (power: int) =
    let ret = Array.create ((Array.length p1) + power) qzero in
      Array.iteri (fun i c ->
		     Array.set ret (i + power) c)
	p1;
      ret

  let add p1 p2 =
    let (pbig, psmall) =
      if (Array.length p1) > (Array.length p2) then (p1, p2) else (p2, p1) in
    let ret = Array.copy pbig in
      Array.iteri (fun i c ->
		     Array.set ret i (C.add c (Array.get ret i)))
	psmall;
      ret

  let sum f ibegin iend =
    let temp = ref zero in
      List.iter (fun i ->
		   temp := add !temp (f i)
		)
	(list_range ibegin iend);
      !temp

  let sum_list l =
    let temp = ref zero in
      List.iter (fun i ->
		   temp := add !temp i
		)
	l;
      !temp

  let mult p1 p2 : polynomial =
    let ret = Array.make ((degree p1) + (degree p2) + 1) C.zero in
      Array.iteri
	(fun n _ ->
	   Array.set ret n
	     (C.sum (fun i -> C.mult (Array.get p1 i) (Array.get p2 (n - i)))
		(max 0 (n - (degree p2)))
		(min n (degree p1))))
	ret;
      ret
    
  let sub p1 p2 = add p1 (mult_const p2 (Q.from_int (-1)))

  let neg p1 = sub zero p1

  let prod f ibegin iend =
    let temp = ref one in
      List.iter (fun i ->
		   temp := mult !temp (f i)
		)
	(list_range ibegin iend);
      !temp

  let prod_list f l =
    let temp = ref one in
      List.iter (fun i ->
		   temp := mult !temp (f i)
		)
	l;
      !temp

  let monomial power c =
    let ret = Array.make (power+1) C.zero in
      Array.set ret power c;
      ret

  let gen_sum (m: int) : polynomial =
    let temp =
      sum (fun k ->
	     monomial
	       (m + 1 - k)
	       (C.const ((Q.from_z (choose (m + 1) k)) */ (bernoulli k))))
	0 m in
      div_const temp (Q.from_int (m + 1))

  let discrete_integral (p: polynomial) : polynomial = 
    sum (fun i -> mult_scalar (gen_sum i) (Array.get p i))
      0 (degree p)

  let eval (p: polynomial) (xl: Z.t list) : Q.t =
    qsum
      (fun i -> (C.eval (Array.get p i) (List.tl xl)) */ (Q.from_z (Z.pow_ui (List.hd xl) i)))
      0 (degree p)

  let rec pow (p: polynomial) (i: int) : polynomial =
    match i with
      | 0 -> one
      | 1 -> p
      | i -> mult p (pow p (i-1))

  let unit c = monomial 0 c

  let level_up c =
    sum_list
      (List.map (fun (i, c) -> monomial i (C.unit c))
	 (C.coeffs c))

  let level_down c =
    C.sum (fun i -> C.monomial i (C.coeff (coeff c i) 0)) 0 (degree c)

  let shift (p: polynomial) (i: int) : polynomial =
    let temp = Array.make ((degree p) + i + 1) C.zero in
      Array.iteri (fun j c -> Array.set temp (i+j) c) p;
      temp

  let level_merge p =
    C.sum (fun i -> C.shift (coeff p i) i)
      0 (degree p)

  let compose (p: polynomial) (fp: polynomial) (i : int) =
    (*printf "compose level = %d, i = %d, p = %s\t\t fp = %s\n" level i (to_string p) (to_string fp);*)
    if i > level then raise (General_error "composing with a variable that is not present");
    if level = i then
      sum (fun i ->
	     mult_scalar (pow fp i) (coeff p i))
	0 (degree p)
    else
      Array.map (fun p ->
		   C.compose p (coeff fp 0) i)
	p

  module Infixes = struct
    let ( *^ ) x y = mult x y
    let ( +^ ) x y = add x y
    let ( -^ ) x y = sub x y
  end;;
end;;

(*module P = Polynomial;;*)
module C = Rational;;
module P  = MPolynomial(C);;
module P2 = MPolynomial(P);;

module N = struct
  type t = string * P2.t
  let string_of_note (d, p) = d ^ " " ^ (P2.to_string p)
end;;

module G = Graph(N)

let _simple_shape_size_nonsplitting dims vmin vmax glower gupper =
  let g = gupper in
  let polies = Array.make dims P2.one in

    Array.iteri (fun i _ ->
		   Array.set polies i (P2.of_list [P.none; P.of_list [C.one; C.one]]))
      polies;

  let topo = G.topological_sort g in

  let upper_in_relation d in_relation =
    match G.get_note gupper d in_relation with
      | ("", _) -> P2.monomial 0 (P.monomial 0 (C.const (Q.from_int (Array.get vmax d)))) 
      | ("<=", p) -> p 
      | (_, _) -> raise Not_expected in
    
  let lower_in_relation d in_relation =
    match G.get_note glower d in_relation with
      | ("", _) -> P2.monomial 0 (P.monomial 0 (C.const (Q.from_int (Array.get vmin d))))
      | (">=", p) -> p
      | (_, _) -> raise Not_expected in

  let part_discrete_integral p =
    let p = P2.coeff p 0 in
    let pi = P.discrete_integral p in
    let pi2 = P2.level_up pi in
      (*printf "integrating %s\n" (P.to_string p);
	printf "integrated = %s\n" (P.to_string pi);
	printf "leveled up = %s\n" (P2.to_string pi2);*)

      P2.sum_list [(P2.monomial 0 pi);
		   (P2.neg pi2);
		   (P2.level_up p)
		  ]
  in

    (printf "topo sort = %s\n" (String.concat " " (List.map string_of_int topo)); flush stdout);

    (*
    ifdebug
      (printf "all nodes:\n";
       Array.iteri (fun i _ -> 
		      printf "node %d: %s\n" i (P2.to_string (Array.get polies i)))
	 polies;
       printf "done\n"; flush stdout);*)
    
    let processed = Hashtbl.create dims in
    let free_nodes = Hashtbl.create dims in
      List.iter (fun i -> Hashtbl.add free_nodes i true) topo;
      
      List.iter
	(fun v1 ->
(*	   Hashtbl.add processed v1 true;
	   let related = (List.filter
			    (fun a -> Hashtbl.mem processed a) (list_unique (List.append
									       (G.in_list gupper v1)
									       (G.in_list glower v1)))
									 ) in*)
	   let related = list_unique (List.append (G.in_list gupper v1)
				       (G.in_list glower v1)) in
	     ifdebug (printf "processing %d, related = %d\n" v1 (List.length related));
	     (if (List.length related) > 0 then
		Array.set polies v1
		  (let integrated = 
		     (part_discrete_integral
			(let all_product = 
			   (P2.prod_list
			      (fun v2 ->
				 Hashtbl.remove free_nodes v2;
				 let p = Array.get polies v2 in
				 let lower = lower_in_relation v2 v1 in
				 let upper = upper_in_relation v2 v1 in
				   ifdebug (printf "writing %d based on %d, lower: %s\t upper: %s\n" v2 v1 (P2.to_string lower) (P2.to_string upper);
					    printf "original: %s\n" (P2.to_string p));
				   let p = P2.compose p lower 1 in
				     ifdebug (printf "with lower: %s\n" (P2.to_string p));
				     let p = P2.compose p upper 0 in
				       ifdebug (printf "with upper: %s\n" (P2.to_string p));
				       P2.monomial 0 (P2.level_merge p))
			      related) in
			   printf "all product = %s\n" (P2.to_string all_product);
			   all_product))
		      in
		     printf "integrated = %s\n" (P2.to_string integrated);
		     integrated
		  )
		  
	     ))
	topo;
      
      (*
      ifdebug
	(printf "all nodes:\n";
	 Array.iteri (fun i _ -> 
			printf "node %d: %s\n" i (P2.to_string (Array.get polies i)))
	   polies;
	 printf "done\n"; flush stdout);*)
      
      let temp = 
	zprod_list
	  (fun i ->
	     (*ifdebug (printf "famous node %d: %s\n" i (P2.to_string (Array.get polies i)));*)
	     
	     Q.get_num
	       (P2.eval
		  (Array.get polies i)
		  [Z.from_int (Array.get vmin i);
		   Z.from_int (Array.get vmax i)]
	       ))
	  (List.filter (fun i -> Hashtbl.mem free_nodes i) (G.famous_nodes g)) in
	printf "--- counted ---\n";
	temp

let _reverse_upper p =
  let p = P2.coeff p 0 in
    P2.level_up (P.add p P.one)

let _reverse_lower p =
  P2.monomial
    0
    (P2.level_down
       (P2.sub p P2.one))

let rec _simple_shape_size_given_graphs dims vmin vmax glower gupper l do_not_split =
  printf "--- trying to count, level = %d ---\n" l;
  (printf "got graph lower %s\n" (G.string_of_graph glower); flush stdout);
  (printf "got graph upper %s\n" (G.string_of_graph gupper); flush stdout);
  printf "do_not_split = %s\n" (String.concat "," (List.map (fun (d1, d2) -> sprintf "(%d,%d)" d1 d2) do_not_split));
  if (l > 10) then raise (General_error "we're in too deep");

(*  if (G.is_splitting gupper) or (G.is_splitting glower) then*)
  if (G.is_splitting gupper) then
    (printf "\n\n---- is splitting ----\n\n";

    let splits = List.filter (fun p -> not (List.mem p do_not_split))
(*      (List.append*)
	 (G.get_splitting_edges gupper) 
(*	 (G.get_splitting_edges glower)
      )*)
    in
      if (List.length splits) = 0 then
	raise (General_error "cannot avoid a splitting loop");

    let (v1, v2) = List.hd splits in
    let temp_lower = G.copy glower in
    let temp_upper = G.copy gupper in

    let cons_upper = pair_second (G.disconnect temp_upper v1 v2) in
    let cons_lower = pair_second (G.disconnect temp_lower v2 v1) in

      printf "will split on edge (%d, %d):\n" v1 v2;
      printf "\t<=: %s\n" (P2.to_string cons_upper);
      printf "\t>=: %s\n" (P2.to_string cons_lower);

      let cons_upper_rev = _reverse_upper cons_upper in
      let cons_lower_rev = _reverse_lower cons_lower in

	printf "reversed:\n";
	printf "\t<=: %s\n" (P2.to_string cons_upper_rev);
	printf "\t>=: %s\n" (P2.to_string cons_lower_rev);
	
	let temp_size = _simple_shape_size_given_graphs dims vmin vmax temp_lower temp_upper (l+1) (do_not_split) in

	  G.connect temp_lower v1 v2 (">=", cons_upper_rev);
	  G.connect temp_upper v2 v1 ("<=", cons_lower_rev);

	  let temp_size2 = _simple_shape_size_given_graphs dims vmin vmax temp_lower temp_upper (l+1) ((v1, v2) :: (v2, v1) :: do_not_split) in

	    temp_size -! temp_size2)
  else
    _simple_shape_size_nonsplitting dims vmin vmax glower gupper
  

let simple_shape_size dims vmin vmax relations : Z.t =
  let glower = G.make dims ("", P2.zero) in
  let gupper = G.make dims ("", P2.zero) in

    List.iter (fun (v1, v2, r) -> G.connect glower v1 v2 (">=", r)) (pair_first relations);
    List.iter (fun (v1, v2, r) -> G.connect gupper v1 v2 ("<=", r)) (pair_second relations);

    (printf "got graph lower %s\n" (G.string_of_graph glower); flush stdout);
    (printf "got graph upper %s\n" (G.string_of_graph gupper); flush stdout);
    
    _simple_shape_size_given_graphs dims vmin vmax glower gupper 0 []

    (*let g = (if G.is_splitting g then G.make_reverse g else g) in
      if G.is_splitting g then*)

let count_of_cones (dims: int) (cones: (int * (int list) * (int list list)) list) : ZNUM.t =
  (* see example 7 in Effective Lattice Point Counting in Rational Convex Polytopes, page 14 *)
  let random_monomial () =
    let ret = Array.make dims 0 in
      Array.iteri (fun i v -> ret.(i) <- (if Random.bool() then -1 else 1)) ret;
      ret in
  let is_mono_good gens mono =
    List.for_all (fun gen -> 0 <> (dot_prod gen mono)) gens in
  let find_monomials gens =
    let gens = List.map Array.of_list gens in
    let temp = random_monomial () in
      while not (is_mono_good gens temp) do
	(*printf "mono=%s\n" (String.concat " " (List.map string_of_int (Array.to_list temp)));*)
	let i = Random.int (Array.length temp) in
	  temp.(i) <- temp.(i) + (Random.int 3) - 1;
	  if temp.(i) = 0 then temp.(i) <- (if Random.bool () then -1 else 1)
      done;
      ifdebug (printf "final mono=%s\n" (String.concat " " (List.map string_of_int (Array.to_list temp))));
      temp in
  let all_gens = List.fold_left (fun a (w, p, g) -> List.append a g) [] cones in
  let mono = find_monomials all_gens in
  let rat_of_cone (weight, point, gens) =
    (
      let num = [new poly_of_terms [TERM.of_list qone point]] in
      let den = List.map
	(fun gen -> new poly_of_terms [TERM.of_list qone [];
				       TERM.of_list qnone gen])
	gens in
	(weight, new poly_rational num den, mono)
    ) in

  let terms = (List.map rat_of_cone cones) in

    ifdebug (
      printf "\n\n\n=====================\ncones:\n";
      List.iter (fun (w, r, monos) -> printf "(weight = %d) %s\n" w r#to_string) terms;
      printf "\nmaking univariate\n";
      flush stdout); 
    
(*    let primes = num_primes dims in*)
      (*    let dim_range = list_range 0 (dims-1) in*)
    
      List.iter
	(fun (w, t, m) -> t#subst_monomials_assign dims m)
	terms;
      
      let terms = List.map (fun (w,t,m) -> (w,t)) terms in

	ifdebug (List.iter (fun (w, r) -> printf "(weight = %d) %s\n" w r#to_string) terms;
		 printf "\nclearing denominator of negative exponents\n");
	List.iter (fun (w, t) -> t#clear_nexp_in_den_assign) terms;

	ifdebug (
	  List.iter (fun (w, r) -> printf "%s\n" r#to_string) terms)
	;

      let neg_exp = ref 0 in
	List.iter (fun (w, t) -> 
		     let d = (List.hd (t#get_num))#degree in
		       if d < !neg_exp then neg_exp := d) terms;
	
	List.iter (fun (w, t) ->
		     (List.hd (t#get_num))#mult_term_assign (TERM.of_list qone [-1 * !neg_exp])) terms;

	ifdebug (
	  printf "\nfactoring out negative powers (%d)\n" !neg_exp;
	  List.iter (fun (w, r) -> printf "%s\n" r#to_string) terms;
	  printf "\nsubstituting x -> x + 1\n");

	List.iter (fun (w, r) -> r#subst_trunc_assign 0 (List.length (r#get_den)) (new poly_of_terms [TERM.one; TERM.mono [1]])) terms;

	ifdebug (
	  List.iter (fun (w, r) -> printf "%s\n" r#to_string) terms;
	  printf "\nsimplifying\n");

      List.iter (fun (w, r) -> r#simplify_assign) terms;

      ifdebug (
      List.iter (fun (w, r) -> printf "%s\n" r#to_string) terms
      );

      let temp = List.map (fun (w, r) ->
			     let res = r#eval_at_zero_pole in
			       ifdebug (
			       printf "part: %d * %s\n" w (Q.to_string res));
			       (Q.from_int w) */ res
			  ) terms in
      let result = qlist_sum temp in
	ifdebug (printf "result = %s\n" (Q.to_string result));
	flush stdout;
	(*(if (not (Z.equal zone (Q.get_den result))) then
	   raise (General_error (sprintf "denominator of final result %s was not 1" (Q.to_string result))));*)
	(Q.get_num result)

let count_of_convex_hull (d: int) (points: ZVECTOR.t list) (halfplanes: ZVECTOR.t list) =
  ifdebug (printf "d=%d\n" d;
	   printf "halfplanes are\n";
	   List.iter (fun h -> printf "\t%s\n" (string_of_point h)) halfplanes;
	   printf "points are\n";
	   List.iter (fun p2 -> printf "\t%s\n" (string_of_point p2)) points);
  
  let is_octa v =
    let i = ref 0 in
    let ret = ref true in
    let last_d = ref 0 in
      while !ret && !i < Array.length v do
	let d = abs(v.(!i)) in
	  (if d <> 0 then
	     (if !last_d = 0 then
		last_d := d;
	      if d <> !last_d then
		ret := false));
	  i := !i + 1
      done;
      ifdebug (printf "is_octa(%s) = %s\n"
		 (String.concat " " (List.map string_of_int (Array.to_list v)))
		 (string_of_bool !ret));
      !ret in
  let is_on_a_halfplane halfplanes v1 minhalfplanes =
    let onplanes = (List.filter (is_point_on_halfplane v1) halfplanes) in
    let m = ZMATRIX.of_vectors onplanes in
    let r = zmatrix_rank m in
      ifdebug (printf "point %s shares %d planes\n" (ZVECTOR.to_string v1) r);
      (r >= minhalfplanes) in

  let find_noncoplanar_pair vl halfplanes =
    let (p1, p1rel) :: rest = vl in
    let halfplanes = List.filter (is_point_on_halfplane p1) halfplanes in
    let ((p2, p2rel) :: others1, others2) =
      list_partition (fun (p2, p2rel) -> not (List.exists (is_point_on_halfplane p2) halfplanes)) rest in
      ((p1, p1rel), (p2, p2rel), List.append others1 others2) in

  let rec split_into_simple_cones
      (p1: ZVECTOR.t)
      (vl: ZVECTOR.t list) =    
    ZGEO.split_cone d p1 vl
  in 

  let make_relative_point p1 p2 =
    let a = ZVECTOR.create (Array.length p1) in
      Array.iteri
	(fun i v -> a.(i) <- ZNUM.of_int (ZNUM.sign (ZNUM.minus p2.(i) v))) p1;
      a in

  let intlist_of_zvector p = List.map ZNUM.to_int (Array.to_list p ) in

    (*
  let cones_all = ZGEO.split_into_simplices d points in
  let cones_of_point = Hashtbl.create (List.length points) in
    List.iter (fun p -> Hashtbl.replace cones_of_point p []) points;
    List.iter
      (fun (w, ps) -> 
	 (List.iter (fun (p, ps) ->
		       let old_list = Hashtbl.find cones_of_point p in
			 Hashtbl.replace cones_of_point p ((w, ps) :: old_list)
		    )
	    (list_map_via_center
	       (fun before center after -> (center, List.append before after)) ps)))
      cones_all;

    
    let cones = Hashtbl.fold
      (fun p clist a -> 
	 List.append a
	   (List.map (fun (w, ps) ->
			(w,
			 p,
			 List.map (fun p2 -> make_relative_point p p2) ps
			)) clist)) cones_of_point [] in
    *)

      let cones_halfplanes =
	List.map
	  (fun p1 -> (p1, List.filter (fun h -> is_point_on_halfplane p1 h) halfplanes))
	  points in
      let cones_points =
	List.map
	  (fun (p1, halfplanes) ->
	     let ret = 
	       (p1, List.filter
		  (fun p2 ->
		     (p1 <> p2) &&
		       (is_on_a_halfplane halfplanes p2 (d-1))) points) in
	       ifdebug (printf "point %s has supporting halfplanes:\n" (ZVECTOR.to_string p1);
			List.iter (fun h -> printf "\t%s\n" (ZVECTOR.to_string h)) halfplanes);
	       ret)
	  cones_halfplanes in
    
  let cones_abs = List.fold_left
    (fun a c -> List.append a c)
    []
    (List.map (fun (p1, points) ->
		 let cones = split_into_simple_cones p1 points in
		   
		   (*
		     let pH = Hashtbl.create ((List.length points) + 1) in
		     List.iter
		     (fun (w, p, pl) ->
		     (List.iter
		     (fun p ->
		     try Hashtbl.replace pH p ((Hashtbl.find pH p) + w)
		     with Not_found -> Hashtbl.replace pH p w)
		     (p :: pl))
		     )
		     cones;
		     printf "point weights:\n";
		     Hashtbl.iter (fun p w -> printf "\t%d: %s\n" w (ZVECTOR.to_string p)) pH;
		   *)


		   cones
)
       cones_points) in

  let cones_rel =
    List.map
      (fun (w, p1, points) -> (w, p1, List.map (make_relative_point p1) points))
      cones_abs in
      
      count_of_cones
	d (List.map (fun (w, p, vl) ->
		       (w,
			intlist_of_zvector p,
			List.map intlist_of_zvector vl)
		    ) cones_rel)
