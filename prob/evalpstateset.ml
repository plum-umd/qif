open Eval
open State
open Lang
open Pstateset
open Evalstate
open Util
open Gmp_util
open Gmp.Q.Infixes
open Gmp.Z.Infixes
open Gmp
open Printf

exception Eval_error of string;;

module Eval
  (PSSM: PSTATESET_TYPE)
  : (EVALPSTATESET_TYPE with type pstateset = PSSM.pstateset) =
struct
  module PSS = PSSM

  type pstateset = PSS.pstateset

  let rec peval (cstmt: stmt) (p: pstateset) : pstateset = 
    ifdebug
      (
	Lang.print_stmt_type cstmt;
	printf " ";
	flush stdout
      );
    match cstmt with
      | SSkip -> p

      | SDefine (name, datatype) ->
	  PSS.addvar p name

      | SOutput (name, toagent) -> p

      | SAssign (name, varaexp) ->
	  PSS.transform p cstmt
	    (* change eventually, don't want to be sending cstmt *)
	    
      | SSeq (stmt1, stmt2) ->
	  let p1 = peval stmt1 p in
	    peval stmt2 p1

      | SPSeq (stmt1, stmt2, prob, n1, n2) ->
	  let p1 = PSS.prob_scale (peval stmt1 p) prob in
	  let p2 = PSS.prob_scale (peval stmt2 p) (qone -/ prob) in
	    (PSS.abstract_plus p1 p2)
	   
      | SIf (guardlexp, stmt1, stmt2) ->
	  let (psstrue, pssfalse) = PSS.split p guardlexp in
	  let pss2true = (peval stmt1 psstrue) in
	  let pss2false = (peval stmt2 pssfalse) in
	    PSS.abstract_plus pss2true pss2false

      | SWhile (guardlexp, whilebody) -> 
	  let (psstrue, pssfalse) = PSS.split p guardlexp in
	  let pssret = ref pssfalse in
	  let pss2true = ref (peval whilebody psstrue) in
	    while (PSS.is_possible !pss2true) do
	      let (psstrue, pssfalse) = PSS.split !pss2true guardlexp in
		pssret := PSS.abstract_plus !pssret pssfalse;
		pss2true := (peval whilebody psstrue);
	    done;
	    !pssret

      | SUniform (varid, blower, bupper) ->
	  let vars_remain = list_subtract (PSS.vars p) [varid] in
	  let p = PSS.project p vars_remain in
	    PSS.prod
	      p
	      (PSS.make_uniform varid (Z.of_int blower) (Z.of_int bupper)) (* todo: parse directly into Z.t *)

  (* peval_start: stmt -> string list -> dist
     Evaluates the given statement on a distribution in which the only possible states is one in which
     all variables have value 0. *)

  let peval_start s =
    let p = PSS.make_new [] in
      peval s p

end;;
