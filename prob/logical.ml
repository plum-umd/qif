open Util;;
open Printf;;

(*
type 'a lform = 
  | LTrue
  | LFalse
  | LAnd of 'a lform list
  | LOr of 'a lform list
  | LNot of 'a lform
  | LTerm of 'a
*)

  (* match aform with
     | LTrue -> ...
     | LFalse -> ...
     | LAnd llist -> ...
     | LOr llist -> ...
     | LNot l1 -> ...
     | LTerm aterm -> ...
  *)

module type TERMINAL_TYPE = sig
  type term
  val term_true: term
  val term_false: term
  val string_of_term: term -> string
  val print_term: term -> unit
  val term_negate: term -> term list (* negation might be a disjunction of terms *)
end

module type LOGICAL_TYPE = sig
  type term
  type logical =  (* this is allowed? really? *)
    | LTrue
    | LFalse
    | LAnd of logical list
    | LOr of logical list
    | LNot of logical
    | LTerm of term
  val string_of_logical: logical -> string
  val print_logical: logical -> unit
  val simplify_negation: logical -> logical
  val dnf_of_logical: logical -> logical
  val dnf_lists_of_logical: logical -> (term list) list (* list of disjuncts, each a list of conjuncts of terms *)
end;;

module Logical (T: TERMINAL_TYPE)
  : (LOGICAL_TYPE with type term = T.term)
  = struct

    type term = T.term
    type logical = 
      | LTrue
      | LFalse
      | LAnd of logical list
      | LOr of logical list
      | LNot of logical
      | LTerm of term

    let rec string_of_logical alogical = match alogical with
      | LTrue -> "true"
      | LFalse -> "false"
      | LAnd llist ->
	  String.concat ""
	    ["AND ("; String.concat " and " (List.map string_of_logical llist); ")"]
      | LOr llist ->
	  String.concat ""
	    ["OR ("; String.concat " or " (List.map string_of_logical llist); ")"]
      | LNot l1 ->
	  String.concat ""
	    ("NOT (" :: (string_of_logical l1) :: ")" :: [])
      | LTerm t ->
	  T.string_of_term t

    let rec string_of_logical_pretty tabs alogical = 
      let ntabs = tabs ^ "  " in
	match alogical with
	  | LTrue -> tabs ^ "true"
	  | LFalse -> tabs ^ "false"
	  | LAnd llist -> 
	      (String.concat ""
		 ["AND\n" ^ ntabs ;
		  String.concat ("\n" ^ ntabs) (List.map (fun a -> (string_of_logical_pretty (ntabs) a) ^ "") llist);
		  ""])
	  | LOr llist -> 
	      (String.concat ""
		 ["OR\n" ^ ntabs;
		  String.concat ("\n" ^ ntabs) (List.map (fun a -> (string_of_logical_pretty (ntabs) a) ^ "") llist);
		  ""])
	  | LNot l1 ->
	      (String.concat ""
		 ["NOT\n"; tabs; (string_of_logical_pretty tabs l1)])
	  | LTerm aterm ->
	      T.string_of_term aterm

  let rec print_logical alogical = print_string (string_of_logical alogical)

  let rec print_logical_pretty alogical = print_string (string_of_logical_pretty "" alogical)

  let _term_negate t =
    LOr (List.map (fun x -> LTerm x) (T.term_negate t))

  let rec simplify_negation alogical =
    let r = simplify_negation in
      match alogical with
	| LNot(LTrue)  -> LFalse
	| LNot(LFalse) -> LTrue
	| LNot(LAnd llist) -> LOr (List.map (fun a -> r (LNot a)) llist)
	| LNot(LOr llist)  -> LAnd (List.map (fun a -> r (LNot a)) llist)
	| LNot(LNot(l1)) -> r l1
	| LNot(LTerm(aterm)) -> _term_negate aterm
	| LAnd llist -> LAnd (List.map r llist)
	| LOr llist  -> LOr (List.map r llist)
	| a -> a

  let rec simplify_unaries alogical =
    match alogical with
      | LOr ([single]) -> simplify_unaries single
      | LAnd ([single]) -> simplify_unaries single
      | _ -> alogical

  let rec _collect_conjuncts llist =
    (List.fold_left
       (fun (tallycon, tallyrest) al -> match al with
	  | LAnd (sublist) -> 
	      (let (subcon, subrest) = _collect_conjuncts sublist in
		 (List.append tallycon subcon, List.append tallyrest subrest))
	  | _ -> (tallycon, al :: tallyrest))
       ([], []) llist)

  let rec simplify_conjunction alogical =
    match alogical with
      | LAnd (llist) ->
	  let llist = List.map simplify_conjunction llist in
	  let (conjuncts, rest) = _collect_conjuncts llist in
	    LAnd (List.append conjuncts rest)
      | LOr (llist) -> LOr (List.map simplify_conjunction llist)
      | _ -> alogical 

  let simplify alogical = simplify_conjunction (simplify_unaries (simplify_negation alogical))

  let _collapse_disjuncts llist =
    (List.fold_left
       (fun tally al -> match al with
	  | LOr (sublist) -> List.append tally sublist
	  | _ -> raise (General_error "was not a list of disjuncts"))
       [] llist)

  let rec _collapse_conjuncts llist =
    let (conjuncts, rest) = 
      (List.fold_left
	 (fun (tallycon, tallyrest) al -> match al with
	    | LAnd (sublist) -> (List.append tallycon (_collapse_conjuncts sublist), tallyrest)
	    | LOr ([LAnd (sublist)]) -> (List.append tallycon sublist, tallyrest)
	    | _ -> (tallycon, al :: tallyrest))
	 ([], []) llist) in
      List.append conjuncts rest

  let dnf_of_logical alogical =
    let rec aux alog = match alog with
      | LOr (llist)  -> LOr (_collapse_disjuncts (List.map aux llist))
      | LAnd (llist) ->
	  let llistdnf = List.map aux llist in
	  let llistors = List.map
	    (fun i -> match i with
	       | LOr (ret) -> ret
	       | _ -> raise (General_error "uhh, wtf?")) llistdnf in
	  let disjuncts = list_prod_list llistors in
	    LOr (List.map (fun i -> LAnd i) disjuncts)
      | LTrue -> LOr [LTrue]
      | LFalse -> LOr [LFalse]
      | LTerm (a) -> LOr [LTerm a]
      | _ -> raise (General_error "formula was not negation simplified") in
    let simplified = (simplify alogical) in
      aux simplified

  let _terms_of_list llist =
    List.map (fun i -> match i with
		| LTerm (t) -> t
		| _ -> raise (General_error "not term list given"))
      llist

  let dnf_lists_of_logical alogical = let dnf = (dnf_of_logical alogical) in
    match dnf with
      | LOr (llist) ->
	  List.map (fun al2 -> match al2 with
		      | LTrue -> [T.term_true]
		      | LFalse -> [T.term_false]
		      | LAnd (llist) -> _terms_of_list llist
		      | LTerm (t) -> [t]
		      | x ->  print_logical alogical;
			  raise (General_error "was not dnf"))
	    llist
      | _ -> raise (General_error "should not be here, seriously")

end;;
