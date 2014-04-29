open Printf
open ExtHashtbl
open ExtList

exception Type_error of string;;

type valueident = string
type typeident = string
type opident = string
type label = string
and datatype =
  | TUnit
  | TBool
  | TInt
  | TReal
  | TString
  | TRecord of (string * datatype) list
  | TFun of datatype list * datatype
  | TIdent of typeident
and value =
  | VDeclared of valueident
  | VUnit
  | VBool of bool
  | VInt of int
  | VReal of float
  | VString of string
  | VFun of valueident list * stmt (* no closures *)
  | VRef of int (* reference to heap for structure storage *)
  | VAbsRef of int (* reference to abstract values *)
  | VRecord of (valueident * value) list
and exp =
  | EValue of value
  | EIdent of valueident
  | ERecord of (valueident * exp) list
  | EMember of exp * valueident
  | EBinop of opident * exp * exp
  | EUnop of opident * exp
  | EApp of valueident * (exp list)
  | ERandInt of int * int
  | ERandReal of float * float
  | ERandBool of float
and stmt =
  | SSkip
  | SSeq of stmt * stmt
  | SAssign of exp * exp
  | SDefineValue of valueident * datatype
  | SDefineType of typeident * datatype
  | SDefineFun of valueident * datatype * ((valueident * datatype) list) * stmt
(* name, return type, (input name, input type) list, body *)
  | SIf of exp * stmt * stmt
  | SWhile of exp * exp * stmt
  | SReturn of exp
  | SAssert of exp
and pstmt =
  | PSInclude of string * pstmt
  | PSDefineExp of valueident * exp * pstmt
  | PSSeq of pstmt * pstmt
  | PSStmt of stmt
  | PSUi of uicmd
and uicmd =
  | UiReset of int * int
  | UiView of exp * exp * exp
  | UiExit
;;

let string_of_ident a = a

let rec copy_value v = match v with
(*  | VString s -> VString (String.copy s)*)
  | VRecord nvl -> VRecord (List.map (fun (fname, fval) -> (fname, copy_value fval)) nvl)
(*  | VRecord (vr) -> VRecord (Hashtbl.map copy_value vr)*)
  | x -> x (* VFun, VUnit, VBool, VInt, VReal, VRef *)

let bool_to_int b = if b then 1 else 0;;

let string_of_valueident i : string = i

let rec string_of_datatype dt : string =
  match dt with
  | TUnit -> "unit"
  | TBool -> "bool"
  | TInt -> "int"
  | TReal -> "real"
  | TString -> "string"
(*  | TProduct (da) -> sprintf "(%s)"
    (String.concat
       ", "
       (List.map string_of_datatype da))*)
  | TRecord (dh) -> sprintf "{%s}"
    (String.concat
       ", "
       (List.map 
	  (fun (n, dt1) -> sprintf "%s: %s" n (string_of_datatype dt1)) dh))
  | TFun (dt1, dt2) -> sprintf "%s -> %s" (String.concat "," (List.map string_of_datatype dt1)) (string_of_datatype dt2)
  | TIdent (i) -> i
(*  | TLabeled (llist, dt2) -> sprintf "%s {%s}" (string_of_datatype dt2) (string_of_labellist llist)*)
and string_of_value v : string =
  match v with
  | VDeclared id -> sprintf "[declared %s]" id
  | VRef i -> sprintf "[ref %d]" i
  | VAbsRef i -> sprintf "[absref %d]" i
  | VUnit -> "()"
  | VBool (b) -> string_of_bool b
  | VInt (i) -> string_of_int i
  | VReal (f) -> string_of_float f
  | VString (s) -> sprintf "\"%s\"" s
  | VRecord (vh) -> sprintf "{%s}"
    (String.concat
       "; "
       (List.map 
	  (fun (n, v1) -> sprintf "%s=%s" n (string_of_value v1)) vh))
  | VFun (invar, body) -> sprintf "fun(%s) {...}" (String.concat ", " invar)

and string_of_exp aexp : string =
  match aexp with
  | EValue (v) -> string_of_value v
  | EIdent (i) -> i
  | ERecord (vh) -> sprintf "{%s}"
    (String.concat
       "; "
       (List.map 
	  (fun (n, v1) -> sprintf "%s = %s" n (string_of_exp v1)) vh))
  | EMember (e1, s) ->
    sprintf "%s.%s" (string_of_exp e1) s
  | EBinop (bop_name, aexp1, aexp2) ->
    sprintf "%s %s %s"
      (string_of_exp aexp1) bop_name (string_of_exp aexp2)
  | EUnop (uop_name, aexp1) ->
    sprintf "%s %s" uop_name (string_of_exp aexp1)
  | EApp (i, explist) ->
    sprintf "%s(%s)" i (String.concat ", " (List.map string_of_exp explist))
  | ERandInt (i1, i2) ->
    sprintf "(uniform %d %d)" i1 i2
  | ERandReal (r1, r2) ->
   sprintf "(uniform %f %f)" r1 r2
  | ERandBool p ->
    sprintf "(rand_bool %f)" p

let rec _string_of_stmt astmt tab = 
  let ntab = "  " ^ tab in
  match astmt with
  | SSkip -> "skip"
  | SSeq (astmt1, astmt2) ->
    sprintf "%s;\n%s%s" (_string_of_stmt astmt1 tab) tab (_string_of_stmt astmt2 tab)
  | SAssign (a, aexp) ->
    sprintf "%s = %s" (string_of_exp a) (string_of_exp aexp)
  | SDefineValue (id, dt) ->
    sprintf "%s %s" (string_of_datatype dt) (string_of_valueident id)
  | SDefineFun (fname, outtype, inlist, bodystmt) ->
    sprintf "%s %s(%s) {...}"
      (string_of_datatype outtype)
      fname
      (String.concat ", " (List.map (fun (id, dt) -> sprintf "%s %s" (string_of_datatype dt) id) inlist))
  | SDefineType (tname, dt) ->
    sprintf "type %s = %s" tname (string_of_datatype dt)
  | SIf (aexp, astmt1, SSkip) ->
    sprintf "if (%s) {\n%s%s\n%s}"
      (string_of_exp aexp)
      ntab
      (_string_of_stmt astmt1 ntab)
      tab
  | SIf (aexp, astmt1, astmt2) ->
    sprintf "if (%s) {\n%s%s\n%s} else {\n%s%s\n%s}"
      (string_of_exp aexp)
      ntab
      (_string_of_stmt astmt1 ntab)
      tab
      ntab
      (_string_of_stmt astmt2 ntab)
      tab
  | SWhile (aexp, EValue (VBool true), astmt1) ->
    sprintf "while (%s) {\n%s%s\n%s}"
      (string_of_exp aexp)
      ntab
      (_string_of_stmt astmt1 ntab)
      tab
  | SWhile (aexp, invexp, astmt1) ->
    sprintf "while (%s) (%s) {\n%s%s\n%s}"
      (string_of_exp aexp)
      (string_of_exp invexp)
      ntab
      (_string_of_stmt astmt1 ntab)
      tab
  | SReturn (aexp) ->
    sprintf "return %s" (string_of_exp aexp)
  | SAssert (aexp) ->
    sprintf "assert %s" (string_of_exp aexp)
let string_of_stmt astmt = _string_of_stmt astmt ""

let bool_of_value v = match v with
  | VBool (b) -> b
  | _ -> raise (Type_error "bool expected")

let int_of_value v = match v with
  | VInt (i) -> i
  | _ -> raise (Type_error "int expected")

let ref_of_value v = match v with
  | VRef (i) -> i
  | _ -> raise (Type_error (sprintf "ref expected, got %s" (string_of_value v)))

(*
let tuple_of_value v = match v with
  | VTuple (t) -> t
  | _ -> raise (Type_error "tuple expected")
*)

let record_of_value v = match v with
  | VRecord (r) -> r
  | _ -> raise (Type_error "record expected")

let fun_of_value v = match v with
  | VFun (i, s) -> (i, s)
  | _ -> raise (Type_error "fun expected")

(*
let rec fold_value fval a aval =
  let a = fval a aval in
  match aval with
  | VTuple (vall) -> List.fold_left (fold_value fval) a vall
  | VRecord (valh) -> List.fold_left (fold_value fval) a (List.of_enum (Hashtbl.values valh))
  | _ -> a
  
let rec fold_exp fval fexp a aexp =
  let a = fexp aexp in
  match aexp with
  | EValue (v) -> fold_value fval a v
  | EMember (exp1, _) -> fold_exp fval fexp a exp1
  | EIndex (exp1, exp2) -> fold_exp fval fexp (fold_exp fval fexp a exp1) exp2
  | EBinop (_, exp1, exp2) -> fold_exp fval fexp (fold_exp fval fexp a exp1) exp2
  | EUnop (_, exp1) -> fold_exp fval fexp a exp1
  | EApp (_, explist) -> fold_exp fval fexp (
  | _ -> a

let rec fold_stmt f astmt a =
  match astmt with
  | SAssign (name, varexp) -> f astmt a
  (*    | SDefine (name, vartype) -> f astmt a*)
  | SSkip -> f astmt a
  | SSeq (s1, s2) ->  fold_stmt f s2 (fold_stmt f s1 (f astmt a))
  (* | SPSeq (s1, s2, p, n1, n2) -> fold_stmt f s2 (fold_stmt f s1
     (f astmt a))*)
  | SIf (guardexp, s1, s2) -> fold_stmt f s2 (fold_stmt f s1 (f astmt a))
  | SWhile (guardexp, invexp, bodystmt) -> fold_stmt f bodystmt (f astmt a)
  (* | SUniform (varid, blower, bupper) -> f astmt a*)
  (* | SOutput (varid, agents) -> f astmt a*)
(*  | SLabeled (lid, s1) -> fold_stmt f s1 (f astmt a)*)
  | SAssert (aexp) -> f astmt a
  | _ -> failwith "not implemented"
*)

let rec intlinear_aexp_of_exp (aexp: exp) : ((int * string) list * int) =
  match aexp with
  | EIdent (id) -> ([(1, id)], 0)
  | EValue (VInt (v)) -> ([], v)
  | EBinop ("*", EValue (VInt c), exp2)
  | EBinop ("*", exp2, EValue (VInt c)) -> 
    let (pairs, constant) = intlinear_aexp_of_exp exp2 in
    (List.map (fun (a,b) -> (a*c, b)) pairs,
     constant * c)
  | EBinop (o, exp1, exp2) when o = "+" || o = "-" ->
    let (pairs1, constant1) = intlinear_aexp_of_exp exp1 in
    let (pairs2, constant2) = intlinear_aexp_of_exp exp2 in
    (List.append pairs1 
       (List.map (fun (a,b) -> (a * (if o = "+" then 1 else (-1)), b)) pairs2),
     constant1 + constant2)
  | _ -> failwith "unexpected expression provided for conversion to linear expression"

let rec get_vars astmt = match astmt with
(*  | SDefineValue (vid, TLabeled (llist, _)) -> [(vid, Some (List.unique llist))]*)
  | SDefineValue (vid, _) -> [(vid, None)]
  | SSeq (s1, s2)
  | SIf (_, s1, s2) -> List.append (get_vars s1) (get_vars s2)
  | SWhile (_, _, s1) -> get_vars s1
(*  | SLabeled (_, s1) -> *)
  | _ -> []

let rec exp_map fexp aexp = 
  let r = exp_map fexp in
  let temp = begin
    match aexp with
    | EBinop (opname, exp1, exp2) -> EBinop (opname, r exp1, r exp2)
    | EUnop (opname, exp1) -> EUnop (opname, r exp1)
    | ERecord nelist -> ERecord (List.map (fun (fn, fe) -> (fn, r fe)) nelist)
    | EMember (exp1, fname) -> EMember (r exp1, fname)
    | EApp (vid, elist) -> EApp (vid, List.map r elist)
    | _ -> aexp
  end in
  try fexp temp with Util.Unchanged -> temp
and stmt_map fexp fstmt astmt = 
  let rstmt = stmt_map fexp fstmt in 
  let rexp = exp_map fexp in
  let temp = begin match astmt with
    | SSeq (s1, s2) -> SSeq(rstmt s1, rstmt s2)
    | SIf (c, s1, s2) -> SIf(rexp c, rstmt s1, rstmt s2)
    | SAssign (aexp, e1) -> SAssign(rexp aexp, rexp e1)
    | SAssert (lexp1) -> SAssert(rexp lexp1)
    | SWhile (lexp1, lexp2, astmt) -> SWhile(rexp lexp1, rexp lexp2, rstmt astmt)
    | SReturn exp1 -> SReturn (rexp exp1)
    | _ -> astmt
  end in 
  try fstmt temp with Util.Unchanged -> temp

let rec seq_of_list stmtlist =
  match stmtlist with
  | astmt :: [] -> astmt
  | astmt :: rest -> SSeq (astmt, seq_of_list rest)
  | [] -> SSkip

let exp_subst_exp what withwhat inwhat =
  exp_map (fun aexp -> match aexp with
    | EIdent x when x = what -> withwhat
    | _ -> aexp) inwhat

let stmt_subst_exp what withwhat inwhat =
  stmt_map (fun aexp -> match aexp with
    | EIdent x when x = what -> withwhat
    | _ -> aexp)
    (fun astmt -> astmt)
    inwhat
  
