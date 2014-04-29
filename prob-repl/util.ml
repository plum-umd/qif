open ExtList
open ExtString

type ('a, 'b) splitcontinue = ('a -> 'b list) -> 'b list

exception Unchanged;;

(* http://stackoverflow.com/questions/16269393/how-to-get-the-number-of-cores-on-a-machine-with-ocaml *)
let cpu_count () : int = 
  try
    match Sys.os_type with 
    | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS") 
    | _ ->
      let i = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
      let close () = Pervasives.ignore (Unix.close_process_in i) in
      try Scanf.fscanf i "%d" (fun n -> close (); n) with e -> close (); raise e
  with
  | Not_found | Sys_error _ | Failure _ | Scanf.Scan_failure _ 
  | End_of_file | Unix.Unix_error (_, _, _) -> 1
;;

let rec list_range a b =
  if a = b then []
  else a :: (list_range (a+1) b);;

let lists_subtract lista listb =
  List.remove_if (fun e -> List.mem e listb) lista

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

let list_min l = list_ultimate (fun a b -> a < b) l (List.hd l)
let list_max l = list_ultimate (fun a b -> a > b) l (List.hd l)

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

let rec list_prod l1 l2 = match l1 with
  | [] -> []
  | i1 :: t1 ->
      List.append
	(List.map (fun x -> (i1,x)) l2)
	(list_prod t1 l2)
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

let current_executable = Sys.argv.(0);;
let current_dir = Filename.dirname current_executable;;
let original_dir = Unix.getcwd ();;

let file_relative f = (* relative to starting envionment *)
  if Filename.is_relative f then
    original_dir ^ Filename.dir_sep ^ f
  else
    f
;;

let string_indent tab s =
  let pieces = String.nsplit s "\n" in
  tab ^ (String.concat ("\n" ^ tab) pieces)
;;

let rec make_split lower upper prec =
  let points = (upper - lower) + 1 in
  if (points < 2) || (prec < 1) then
    [(lower, upper)]
  else
    let mid = (upper + lower) / 2 in
    let temp1 = make_split lower mid (prec - 1) in
    let temp2 = make_split (mid + 1) upper (prec - 1) in
    List.append temp1 temp2
