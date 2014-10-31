open Lang
open Util
open Filename
open State
open Parser_util

let rec subst_aexp aaexp (varid: Lang.varid) aaexp2 =
  let r = fun a -> subst_aexp a varid aaexp2 in
    match aaexp with 
      | AEInt (v) -> AEInt (v)
      | AEVar (id) ->
	  if (id = varid) then aaexp2 else AEVar (id)
      | AEBinop (b, exp1, exp2) ->
	  AEBinop (b, r exp1, r exp2)
;;

let rec subst_lexp alexp (varid: Lang.varid) aaexp =
  let r = fun a -> subst_lexp a varid aaexp in
  let raexp = fun a -> subst_aexp a varid aaexp in
    match alexp with
      | LEBool  (v) -> LEBool (v)
      | LEBinop (b, exp1, exp2) -> LEBinop (b, r exp1, r exp2)
      | LEReln  (rln, exp1, exp2) -> LEReln (rln, raexp exp1, raexp exp2)
;;
  
let rec subst_stmt s (varid: Lang.varid) aaexp =
  let r = fun s -> subst_stmt s varid aaexp in
  let rlexp = fun s -> subst_lexp s varid aaexp in
  let raexp = fun s -> subst_aexp s varid aaexp in
    match s with
      | SAssign (name, varexp) -> 
	  if (name = varid) then (raise (General_error ("assignment to a variable (" ^ (Lang.varid_to_string name) ^ ") that will be substituted")));
	  SAssign (name, raexp varexp)
      | SSkip -> SSkip
      | SSeq (s1, s2) -> SSeq (r s1, r s2)
      | SPSeq (s1, s2, p, n1, n2) -> SPSeq (r s1, r s2, p, n1, n2)
      | SIf (guardexp, s1, s2) -> SIf (rlexp guardexp, r s1, r s2)
      | SWhile (guardexp, bodystmt) -> SWhile (rlexp guardexp, r bodystmt)
      | SUniform (name, blower, bupper) ->
	  if (name = varid) then (raise (General_error ("uniform to a variable (" ^ (Lang.varid_to_string name) ^ ") that will be substituted")));
	  s
      | SDefine (name, datatype) ->
	  if (name = varid) then (raise (General_error ("definition of a variable (" ^ (Lang.varid_to_string name) ^ ") that will be substituted")));
	  s
      | SOutput (name, toagent) ->
	  if (name = varid) then (raise (General_error ("output of a variable (" ^ (Lang.varid_to_string name) ^ ") that will be substituted")));
	  s

let rec subst_pstmt inpstmt (id: Lang.varid) withaexp =
  match inpstmt with
    | PSSubst (id2, withaexp2, inpstmt2) -> 
	if id2 = id then
	  raise (General_error ("re#definition of " ^ (Lang.varid_to_string id)));
	PSSubst (id2,
		 subst_aexp withaexp2 id withaexp,
		 subst_pstmt inpstmt2 id withaexp)
    | PSInc (filename, includedfrom, inpstmt) ->
	PSInc (filename, includedfrom, subst_pstmt inpstmt id withaexp)
    | PSStmt (astmt) -> PSStmt (subst_stmt astmt id withaexp)
;;

let rec preeval_includes (apstmt: pstmt): pstmt =
  match apstmt with
    | PSSubst (id, withaexp, inpstmt) -> 
	PSSubst (id, withaexp, preeval_includes inpstmt)
    | PSInc (filename, includedfrom, inpstmt) ->
	let dirname = Filename.dirname includedfrom in
	let filenamefull = Filename.concat dirname filename in
	let apstmt = parse_file filenamefull Parser.pstmt in 
	  preeval_includes apstmt
    | PSStmt (astmt) -> PSStmt (astmt)
;;

let rec preeval (apstmt: pstmt) : stmt = 
  match apstmt with
    | PSSubst (id, withaexp, inpstmt) -> 
	preeval (subst_pstmt inpstmt id withaexp)
    | PSInc (filename, includedfrom, inpstmt) ->
	let dirname = Filename.dirname includedfrom in
	let filenamefull = Filename.concat dirname filename in
	let apstmt = parse_file filenamefull Parser.pstmt in 
	  preeval apstmt
    | PSStmt (astmt) -> astmt

let rec _predefine_as_state_construct astmt (astate: State.state) idlist =
  match idlist with
    | [] -> PSStmt astmt
    | aid :: rest -> PSSubst (aid,
			      Lang.AEInt (astate#get aid),
			      _predefine_as_state_construct astmt astate rest)

let predefine_as_state astmt astate idlist =
  preeval (_predefine_as_state_construct astmt astate idlist)
