open Printf
open Unix
open Str

exception General_error of string;;
exception Not_applicable;;
exception Loop_exit;;
exception Not_implemented;;
exception Not_expected;;

let debug s = 
  prerr_string s;
  prerr_string "\n";
  flush Pervasives.stderr

(* lg: float -> float
   Log base 2. *)
let lg n =
  (log n) /. (log 2.0)
;;

let pair_first (a, b) = a
let pair_second (a, b) = b

let triple_first (a, b, c) = a
let triple_second (a, b, c) = b
let triple_third (a, b, c) = c

let list_first a = List.hd a
let list_second a = List.hd (List.tl a)

let fabs a = if a > 0.0 then a else (-1.0) *. a
;;

let list_empty = function [] -> true | _ -> false
let list_nonempty = function [] -> false | _ -> true

let list_of_queue q =
  Queue.fold (fun accum i -> i :: accum) [] q

let queue_of_list l =
  let ret = Queue.create () in
    List.iter (fun i -> Queue.add i ret) l;
    ret

let rec list_pairs l =
  match l with
    | [] -> []
    | h :: t ->
	List.append
	  (List.map (fun h2 -> (h, h2)) t)
	  (list_pairs t)

let list_pairs_and_rest l =
  let pairs = list_pairs l in
    List.map (fun (i1, i2) ->
		(i1, i2, List.filter (fun i3 -> i3 != i1 && i3 != i2) l))
      pairs

(*
let list_pairs_and_rest l =
  let qret = Queue.create in
  let q1 = Queue.create in
  let q1done = Queue.create in
    List.iter (fun i -> Queue.add i q1) l;
    while (not (Queue.is_empty q1)) do
      let i1 = Queue.pop q1 in
      let q2 = Queue.copy q1 in
      let qcdone = Queue.create in
	while (not (Queue.is_empty qc)) do
	  let i2 = Queue.pop qc in
	    Queue.add (i, i2, List.append (list_of_queue qdone) (list_of_queue qcdone));
	    Queue.add i2 qcdone
	done;

	Queue.add i qdone;
    done*)

let rec list_prod_gen (l1: 'a list) (l2: 'a list) (c: 'a -> 'a -> 'a): 'a list =
  match l1 with
    | [] -> []
    | e1 :: t1 ->
	List.append
	  (List.map (fun e2 -> c e1 e2) l2)
	  (list_prod_gen t1 l2 c)
;;

(* list_prod: 'a list -> 'b list -> ('a * 'b) list
   Produces the product of the two lists. That is, (a,b) is in the product
   iff a is in the first list and b is in the second. *)
let rec list_prod l1 l2 = match l1 with
  | [] -> []
  | i1 :: t1 ->
      List.append
	(List.map (fun x -> (i1,x)) l2)
	(list_prod t1 l2)
;;

let rec list_prod_as_list l1 l2 = match l1 with
  | [] -> []
  | i1 :: t1 ->
      List.append
	(List.map (fun x -> [i1 ; x]) l2)
	(list_prod_as_list t1 l2)
;;

(* list_prod_list ('a list) list -> ('a list) list *)
let rec list_prod_list llist = match llist with
  | [] -> []
  | l1 :: [] -> List.map (fun i -> [i]) l1
  | options :: tailoptions ->
      let tailcombinations = list_prod_list tailoptions in
      List.fold_left
	(fun accum combination ->
	   List.append
	     accum
	     (List.map (fun i -> i :: combination) options)
	) [] tailcombinations
;;

let rec list_range blower bupper =
  if blower > bupper then
    []
  else
    blower :: (list_range (blower + 1) bupper)
;;

let rec range bupper = list_range 0 (bupper - 1)

let rec list_ultimate better_than l i =
  match l with
    | [] -> i
    | h :: t -> list_ultimate better_than t (if (better_than h i) then h else i);;

let rec list_ultimate_nonempty better_than l =
  match l with
    | [] -> raise Not_found
    | h :: [] -> h
    | h :: t ->
	let best_rest = list_ultimate_nonempty better_than t in
	  if (better_than h best_rest) then h else best_rest

let list_sum l = List.fold_left (fun accum i -> accum + i) 0 l
let list_sum_float l = List.fold_left (fun accum i -> accum +. i) 0.0 l

let list_sum_general z add l = List.fold_left (fun accum i -> add accum i) z l

let list_min l = list_ultimate (fun a b -> a < b) l (List.hd l)
let list_max l = list_ultimate (fun a b -> a > b) l (List.hd l)

let rec list_take_and_rest i take rest = match (i,rest) with
  | (0, l) -> (take, rest)
  | (n, h :: t) -> list_take_and_rest (i-1) (h :: take) t
  | _ -> raise (General_error "shouldn't happen")

let list_split_into i l =
  if i = 0 then ([], l)
  else if i = (List.length l) then (l, [])
  else list_take_and_rest i [] l
  
let rec list_split_into_blocks (bsize: int) (l: 'a list) : 'a list list =
  if List.length l <= bsize then [l]
  else
    let (ablock, arest) = list_split_into bsize l in
      ablock :: (list_split_into_blocks bsize arest)

(* from wikipedia *)
(* uniq: 'a list -> 'a list
   Removes duplicates from the given list. *)
let list_unique lst =
  let unique_set = Hashtbl.create (List.length lst) in
    List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
    Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

let rec list_of_lists_transpose ll =
  let rec firsts_rests afirsts arests ll =
    (match ll with
       | [] -> (List.rev afirsts, List.rev arests)
       | (h1::t1) :: t -> firsts_rests (h1 :: afirsts) (t1 :: arests) t
       | ([]) :: t -> (List.rev afirsts, List.rev arests) 
    ) in
  let (firsts, rests) = firsts_rests [] [] ll in
    match firsts with
      | [] -> []
      | _ -> firsts :: list_of_lists_transpose rests

let list_of_lists_bounds v =
  let t = list_of_lists_transpose v in
    (List.map (fun ll -> list_min ll) t,
     List.map (fun ll -> list_max ll) t)
;;

let rec list_zip l1 l2 =
  match (l1, l2) with
    | ([], []) -> []
    | (h1::t1, h2::t2) -> (h1,h2) :: (list_zip  t1 t2)
    | _ -> raise (General_error ("differently length lists given"))
;;

let lists_append ll = List.fold_left List.append [] ll

let list_subtract ll minus =
  List.filter (fun item -> not (List.mem item minus)) ll

let address o = ((Obj.magic (Obj.repr o)): int)

let array_permute a =
  (* http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle *)
  let l = Array.length a in
  let b = Array.make l (Array.get a 0) in
    Array.set b 0 (Array.get a 0);
    for i = 1 to l - 1 do
      let j = Random.int (i + 1) in
	Array.set b i (Array.get b j);
	Array.set b j (Array.get a i)
    done;
    b
;;

let rec array_splice_out a l =
  let al = Array.to_list a in
  let rec comp i al =
    match al with
      | [] -> al
      | h :: t -> if List.mem i l then comp (i+1) t else h :: comp (i+1) t in
    Array.of_list (comp 0 al)

let rec array_resize s e l =
    let temp = Array.create s e in
      (if (Array.length l >= s) then 
	 Array.blit l 0 temp 0 s
       else
	 Array.blit l 0 temp 0 (Array.length l));
      temp
	
let string_search r s =
  try Str.search_forward r s 0; true
  with Not_found -> false

let string_split s split_on =
  let r = Str.regexp_string split_on in
    Str.split r s

let string_map f s =
  let ret = ref "" in
    String.iter (fun c -> 
		   ret := !ret ^ (f (String.make 1 c)))
      s;
    !ret

let write_file filename content =
  let fd = openfile filename [O_WRONLY; O_CREAT; O_TRUNC] 0o600 in
  let chan = out_channel_of_descr fd in
    output_string chan content;
    close_out chan
;;

let chan_read_all c =
  let b = Buffer.create 4096 in
  let s = String.create 4096 in
    try
      while true do
	let did_read = input c s 0 4096 in
	  if did_read = 0 then raise End_of_file;
	  Buffer.add_substring b s 0 did_read
      done; ""
    with End_of_file -> 
      Buffer.contents b

let exec_and_read_all cmd env =
  let (chan_in, chan_out, chan_err) =
    open_process_full cmd env in
  let ret = 
    (chan_read_all chan_in,
     chan_read_all chan_err) in
    close_process_full (chan_in, chan_out, chan_err);
    ret

let _chan_last_line chan : string * string =
  let all = ref "" in
  let line = ref "" in
    (try 
       while true do
	 line := input_line chan;
       done; (!line, !all)
     with
       | End_of_file -> (!line, !all)
       | _ -> raise (General_error "_chan_last_line: something bad happened"))
;;

let uncurry2 f = fun (a, b) -> f a b;;
let curry2 f = fun a b -> f (a,b);;

let memoize1 isize f =
  let h = Hashtbl.create isize in
    fun k ->
      try
	let temp = Hashtbl.find h k in
	  temp
      with
	  Not_found ->
	    (let temp = f k
	     in
	       Hashtbl.replace h k temp;
	       temp)
;;

let memoize2 isize f =
  curry2 (memoize1 isize (uncurry2 f))
;;

let fill_template (reps: (string * string) list) (s: string) : string =
  List.fold_left
    (fun cstring (from_string, to_string) ->
       Str.global_replace (Str.regexp from_string) to_string cstring)
    s reps

let is_none (a : 'a option) =
  match a with
    | None -> true
    | _ -> false

let is_some (a : 'a option) =
  match a with
    | None -> false
    | _ -> true

let get_some (a: 'a option) =
  match a with
    | Some a -> a
    | None -> raise (General_error "couldn't get some, was none")
;;

let array_satisfying_indices f a : int list =
  let ret = ref [] in
    Array.iteri (fun i o -> if (f o) then ret := i :: !ret) a;
    !ret

let graph_components gsize g : int list list =
  let comp = Array.make gsize 0 in
    Array.iteri (fun i a -> Array.set comp i i) comp;
    let path_to_root = fun d ->
      let path  = ref [] in
      let cnode = ref d in
	while (Array.get comp !cnode) != !cnode do
	  path := !cnode :: !path;
	  cnode := Array.get comp !cnode
	done;
	!cnode :: !path in
    let unify = fun d1 d2 -> 
      let p1 = path_to_root d1 in
      let p2 = path_to_root d2 in
	List.iter (fun anode ->
		     Array.set comp anode
		       (List.hd p1))
	  (List.append p1 p2) in
    let rec make_comps (dims: int list) : int list list =
      match dims with
	| [] -> []
	| aroot :: rest ->
    let acomp = array_satisfying_indices (fun d -> d = aroot) comp in
	      acomp :: (make_comps rest) in
      List.iter (fun (d1, d2) -> unify d1 d2) g;
      let comp_roots = list_unique (Array.to_list comp) in
	make_comps comp_roots

let hash_keys h =
  Hashtbl.fold (fun k _ a -> k :: a) h []

let list_of_hash h = Hashtbl.fold (fun k v a -> (k, v) :: a) h []

let list_partition f l =
  List.fold_left (fun (ain, aout) e -> if f e then (e :: ain, aout) else (ain, e :: aout)) ([], []) l

let run_tests tl =
  Printexc.record_backtrace true;
  List.iter
    (fun (tname, tfun) ->
       printf "===== running %s tests =====\n" tname;
       (try tfun () 
	with 
	  | e ->
	      printf "%s\n" (Printexc.to_string e);
	      Printexc.print_backtrace Pervasives.stdout);
       printf "===== done %s tests =====\n\n" tname;
       flush Pervasives.stdout
    ) tl

let rec _list_map_via_center (accum: 'a list) (before: 'b list) (after: 'b list) (f: 'b list -> 'b -> 'b list -> 'a) : 'a list=
  match after with
    | [] -> accum
    | h :: t ->
	_list_map_via_center
	  ((f before h t) :: accum)
	  (h :: before)
	  t
	  f

let list_map_via_center f l =
  _list_map_via_center [] [] l f
;;

(*
(* oops, ocaml doesn't support actual threads *)
let threads = 8;;

let list_map_threaded f l =
  if (List.length l < threads * 2) then
    List.map f l
  else
    let blocks = Array.of_list (list_split_into_blocks ((List.length l) / threads) l) in
    let num_blocks = Array.length blocks in
    let results = Array.make num_blocks [] in
    let threads = Array.make num_blocks (Thread.self ()) in
      Array.iteri
	(fun i ablock ->
	   let t = Thread.create
	     (fun i ->Array.set results i (List.map f ablock))
	     i in
	     Array.set threads i t)
	blocks;
      Array.iter (fun t -> Thread.join t) threads;
      Array.fold_left (fun ret resblock -> List.append ret resblock) [] results
*)     
      
  
  
  
  
