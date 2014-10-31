open Printf
open State
open Stateset
open Eval
open Util

module Stateset
  (ESM: EVALSTATE_TYPE)
  : (STATESET_TYPE) =

struct
  module ES = ESM

  type stateset = state list

  let stateset_empty = []

  let stateset_default varlist =
    [S.state_default varlist]

  let stateset_new varlist = raise Not_applicable;;

  let stateset_point astate = [astate]

  let stateset_size aset = List.length aset

  let print_stateset aset =
    printf "Stateset of %d\n" (stateset_size aset);
    List.iter (fun a ->
		 printf "\t";
		 S.print_state a;
		 printf "\n")
      aset

  let stateset_uniform varid blower bupper =
    let r = list_range blower bupper in
      List.map (fun n -> S.state_set S.state_empty varid n) r

  let stateset_uniform_in aset varid blower bupper = raise Not_applicable

  let stateset_intersect aset1 aset2 =
    List.filter
      (fun s1 ->
	 List.exists (fun s2 -> S.state_eq_weak s1 s2) aset2)
      aset1

  let stateset_exclude aset1 aset2 =
    List.filter
      (fun s1 ->
	 not (List.exists (fun s2 -> S.state_eq s1 s2) aset2))
      aset1      

  let stateset_split ss1 alexp =
    List.fold_left
      (fun (slin, slout) astate ->
	 (match ES.eval_lexp alexp astate with
	    | 0 -> (slin, astate :: slout)
	    | 1 -> (astate :: slin, slout)
	    | _ -> raise (General_error "logical expression evaluated to something other than 0 or 1")))
      ([], []) ss1

  let stateset_transform aset astmt =
    List.map
      (fun s ->
	 let (aval, astate) = ESM.eval astmt s in
	   astate)
      aset

  let stateset_intersect_partition aset1 aset2 =
    let c = stateset_intersect aset1 aset2 in
    let a = stateset_exclude aset1 c in
    let b = stateset_exclude aset2 c in
      [a;b;c]

  let stateset_prod ss1 ss2 =
    list_prod_gen ss1 ss2
      (fun s1 s2 -> S.state_merge s1 s2)

  let stateset_set_all ss sl =
    List.map (fun s -> S.state_set_list s sl) ss

  let stateset_on_vars ss vl =
    List.map (fun s -> S.state_on_vars s vl) ss

  let stateset_given_state ss s1 =
    List.filter (fun s2 -> S.state_agrees_on s2 s1) ss

  let stateset_enum_on_vars vl ss = stateset_on_vars ss vl

end;;
