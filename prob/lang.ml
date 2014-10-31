open Printf
open Util
open Gmp

type datatype =
  | TBool
  | TInt of int

let datatype_size dt = match dt with
  | TInt (i) -> i
  | TBool -> 1

let render_datatype t = match t with
  | TBool -> "bool"
  | TInt (size) -> sprintf "int%d" size

type agent = string
type varid = agent * string

let agent_list_to_string al = String.concat "," al

let varid_to_string (owner, id) = if owner = "" then id else owner ^ "." ^ id
let varid_list_to_string vl = String.concat "," (List.map varid_to_string vl);

type abinop = (string * (int -> int -> int))
type lbinop = (string * (int -> int -> int))
type lreln  = (string * (int -> int -> int))

let varid_belongs_to anagent (owner, id) = anagent = owner;;

(* arithmetic expression *)
type aexp =
  | AEVar   of varid
  | AEInt   of int
  | AEBinop of abinop * aexp * aexp

(*
  match aexp with
  | AEInt (v) -> ...
  | AEVar (id) -> ...
  | AEBinop (b, exp1, exp2) -> ...
*)

(* logical expression *)
type lexp =
  | LEBool  of int
  | LEBinop of lbinop * lexp * lexp
  | LEReln  of lreln  * aexp * aexp;;

(*
  match lexp with
  | LEBool (v) -> ...
  | LEBinop (b, exp1, exp2) -> ...
  | LEReln (r, exp1, exp2) -> ...
*)

type stmt =
  | SSkip
  | SSeq     of stmt * stmt
  | SPSeq    of stmt * stmt * Q.t * int * int
  | SAssign  of varid * aexp
  | SDefine  of varid * datatype
  | SIf      of lexp * stmt * stmt
  | SWhile   of lexp * stmt
  | SUniform of varid * int * int
  | SOutput  of varid * (agent list)

(*
  match t with
  | SAssign (name, varexp) -> ...
  | SDefine (name, vartype) -> ...
  | SSkip -> ...
  | SSeq (s1, s2) -> ...
  | SPSeq (s1, s2, p, n1, n2) -> ...
  | SIf (guardexp, s1, s2) -> ...
  | SWhile (guardexp, bodystmt) -> ...
  | SUniform (varid, blower, bupper) -> ...
  | SOutput (varid, agents) -> ...
*)

type pstmt =
  | PSSubst of varid * aexp * pstmt
  | PSInc of string * string * pstmt 
  | PSStmt of stmt

(*
  match apstmt with
  | PSSubst (id, withaexp, inpstmt) -> ...
  | PSInc (filename, includedfrom, inpstmt) -> ...
  | PSStmt (astmt) -> ...
*)

type prog = string list * string list * pstmt;;

let bool_to_int b = if b then 1 else 0;;

let plus    a b = a + b;;
let minus   a b = a - b;;
let times   a b = a * b;;
let divides a b = a / b;;

let leq     a b = bool_to_int (a <= b);;
let geq     a b = bool_to_int (a >= b);;
let lesser  a b = bool_to_int (a < b);;
let greater a b = bool_to_int (a > b);;
let equal   a b = bool_to_int (a = b);;

let logical_and a b = min a b;;
let logical_or a b = max a b;;

(* print_aexp: aexp -> unit
   Prints the given arithmetic expression. *)
let rec print_aexp e =
  match e with
    | AEInt (v) -> print_string (string_of_int v)
    | AEVar (id) -> print_string (varid_to_string id)
    | AEBinop (binop, e1, e2) ->
	let (name, op) = binop in
	  print_string "(";
	  print_aexp e1;
	  print_string " ";
	  print_string name;
	  print_string " ";
	  print_aexp e2;
	  print_string ")"

(* print_lexp: lexp -> unit
   Prints the given logical expression. *)
let rec print_lexp e =
  match e with
    | LEBool (v) -> print_string (if v = 0 then "false" else "true")
    | LEBinop (binop, e1, e2) ->
	let (name, op) = binop in
	  print_string "(";
	  print_lexp e1;
	  print_string " ";
	  print_string name;
	  print_string " ";
	  print_lexp e2;
	  print_string ")"
    | LEReln (binop, e1, e2) ->
	let (name, op) = binop in
	  print_string "(";
	  print_aexp e1;
	  print_string " ";
	  print_string name;
	  print_string " ";
	  print_aexp e2;
	  print_string ")"

(* print_stmt_pretty: stmt -> string -> unit
   Prints the given statement prettily, prepending the given string on each line, etc. *)
let rec print_stmt_pretty s tabs =
  match s with
    | SSkip ->
	print_string tabs;
	print_string "skip"
    | SPSeq (s1, s2, p, n1, n2) ->
	print_string tabs;
	printf "pif %d : %d then\n" n1 n2;
	print_stmt_pretty s1 (tabs ^ "  ");
	print_string "\n";
	print_string tabs;
	print_string "else\n";
	print_stmt_pretty s2 (tabs ^ "  ");
	printf "\n";
	print_string tabs;
	print_string "endpif"
    | SAssign (name, ae1) ->
	print_string tabs;
	print_string (varid_to_string name);
	printf " = ";
	print_aexp ae1
    | SIf (guardlexp, s1, s2) ->
	print_string tabs;
	printf "if ";
	print_lexp guardlexp;
	printf " then\n";
	print_stmt_pretty s1 (tabs ^ "  ");
	print_string "\n";
	print_string tabs;
	printf "else\n";
	print_stmt_pretty s2 (tabs ^ "  ");
	printf "\n";
	print_string tabs;
	print_string "endif"
    | SWhile (guardlexp, bodystmt) ->
	print_string tabs;
	printf "while ";
	print_lexp guardlexp;
	printf " do\n";
	print_stmt_pretty bodystmt (tabs ^ "  ");
	print_string "\n";
	print_string tabs;
	print_string "endwhile"
    | SUniform (varid, blower, bupper) ->
	print_string tabs;
	printf "%s = uniform %d %d" (varid_to_string varid) blower bupper
    | SOutput (varid, agents) ->
	print_string tabs;
	printf "output %s to %s" (varid_to_string varid) (String.concat "," agents)
    | SSeq (SDefine (varid1, vartype),
	    SAssign (varid2, aaexp)) when varid1 = varid2 ->
	print_string tabs;
	printf "%s %s = " (render_datatype vartype) (varid_to_string varid1);
	print_aexp aaexp
    | SSeq (SDefine (varid1, vartype),
	    SUniform (varid2, blower, bupper)) when varid1 = varid2 ->
	print_string tabs;
	printf "%s %s = uniform %d %d" (render_datatype vartype) (varid_to_string varid1) blower bupper
    | SDefine (varid, vartype) ->
	print_string tabs;
	printf "%s %s" (render_datatype vartype) (varid_to_string varid)
    | SSeq (s1, s2) ->
	print_stmt_pretty s1 tabs;
	print_string ";\n";
	print_stmt_pretty s2 tabs

;;

let _render_binop_latex (name, op) =
  match name with
    | "==" -> "="
    | "!=" -> "\\neq"
    | ">=" -> "\\geq"
    | "<=" -> "\\leq"
    | ">" -> ">"
    | "<" -> "<"
    | "or" -> "\\vee"
    | "and" -> "\\wedge"
    | "+" -> "+"
    | "-" -> "-"
    | "*" -> "\\times"
    | _ -> raise (General_error ("don't know how to render " ^ name ^ " in latex"))
;;

let render_name_latex n =
  string_map (fun s -> if s = "_" then "\\text{\\_}" else s) n
;;

let render_id_latex id =
  let (agent, name) = id in
    (if agent <> "" then ((render_name_latex agent) ^ ".") else "") ^
      (render_name_latex name)
;;

let rec render_aexp_latex e = 
  match e with
    | AEInt (v) -> (string_of_int v)
    | AEVar (id) -> (render_id_latex id)
    | AEBinop (binop, e1, e2) ->
	sprintf "\\ebinop{%s}{%s}{%s}"
	  (_render_binop_latex binop) 
	  (render_aexp_latex e1)
	  (render_aexp_latex e2)

let rec render_lexp_latex e = 
  match e with
    | LEBool (v) -> (if v = 0 then "\\sfalse" else "\\strue")
    | LEBinop (binop, e1, e2) ->
	sprintf "\\lbinop{%s}{%s}{%s}"
	  (_render_binop_latex binop) 
	  (render_lexp_latex e1)
	  (render_lexp_latex e2)
    | LEReln (binop, e1, e2) ->
	sprintf "\\lreln{%s}{%s}{%s}"
	  (_render_binop_latex binop) 
	  (render_aexp_latex e1)
	  (render_aexp_latex e2)

let rec render_stmt_pretty_latex s tabs =
  let r = render_stmt_pretty_latex in
  let raexp = render_aexp_latex in
  let rlexp = render_lexp_latex in
  let ntabs = tabs ^ "\\;\\;\\;" in
  match s with
    | SSeq (SDefine (_, _), s) ->
	render_stmt_pretty_latex s tabs
    | SSeq (s, SDefine (_, _)) ->
	render_stmt_pretty_latex s tabs
    | SSkip ->
	"\\sskip"
    | SPSeq (s1, SSkip, p, n1, n2) ->
	sprintf "\\spifk\\; %d/%d \\; \\sthenk\\\\\n%s%s" n1 (n1 + n2) 
	  ntabs
	  (r s1 ntabs)
    | SPSeq (s1, s2, p, n1, n2) ->
	sprintf "\\spifk\\; %d/%d \\; \\sthenk\\\\\n%s%s\\\\\n%s\\selsek\\\\\n%s%s" n1 (n1 + n2) 
	  ntabs
	  (r s1 ntabs)
	  tabs
	  ntabs
	  (r s2 ntabs)
    | SSeq (s1, s2) ->
	sprintf "%s\\; ;\\\\\n%s%s"
	  (r s1 (tabs ^ ""))
	  tabs
	  (r s2 (tabs ^ ""))
    | SAssign (name, ae1) ->
	sprintf "\\sassign{%s}{%s}" (render_id_latex name) (raexp ae1)
    | SIf (guardlexp, s1, SSkip) ->
	sprintf "\\sifk \\; %s \\; \\sthenk\\\\\n%s%s"
	  (rlexp guardlexp)
	  ntabs
	  (r s1 (tabs ^ "\\;"))
    | SIf (guardlexp, s1, s2) ->
	sprintf "\\sifk \\; %s \\; \\sthenk\\\\\n%s%s\\\\\n%s\\selsek\\\\\n%s%s"
	  (rlexp guardlexp)
	  ntabs
	  (r s1 (tabs ^ "\\;"))
	  tabs
	  ntabs
	  (r s2 (tabs ^ "\\;"))
    | SWhile (guardlexp, bodystmt) ->
	sprintf "\\swhilek \\; %s \\; \\sdok\\\\\n%s%s"
	  (rlexp guardlexp)
	  ntabs
	  (r bodystmt (tabs ^ "  "))
    | SUniform (varid, blower, bupper) ->
	sprintf "\\suniform{%s}{%d}{%d}" (render_id_latex varid) blower bupper
    | SDefine (varid, dt) ->
	""
    | _ -> raise (General_error "not implemented")
;;

let rec render_pstmt_pretty_latex s tabs =
  match s with
    | PSSubst (id, withaexp, inpstmt) ->
	sprintf "\\psdefine{%s}{%s}\\\\\n%s%s"
	  (render_id_latex id)
	  (render_aexp_latex withaexp)
	  tabs
	  (render_pstmt_pretty_latex inpstmt tabs)
    | PSInc (filename, includedfrom, inpstmt) -> 
	sprintf "\\psinclude{%s}\\\\\n%s%s"
	  filename
	  tabs
	  (render_pstmt_pretty_latex inpstmt tabs)
    | PSStmt (astmt) -> render_stmt_pretty_latex astmt tabs
;;

let render_pstmt_latex s =
  render_pstmt_pretty_latex s "";;

let print_pstmt_latex s =
  print_string "\\begin{displaymath}{\\small\n";
  print_string "\\begin{array}{l}\n";
  print_string "  ";
  print_string (render_pstmt_pretty_latex s "  ");
  print_string "\\\\\n";
  print_string "\\end{array}\n";
  print_string "}\n\\end{displaymath}\n"
;;

let print_stmt_type s = 
  match s with
    | SSkip ->
	printf "skip"
    | SPSeq (_, _, _, _, _) ->
	printf "pif"
    | SSeq (_, _) ->
	printf "seq"
    | SAssign (_, _) ->
	printf "assign"
    | SIf (_, _, _) ->
	printf "if"
    | SWhile (_, _) ->
	printf "while"
    | SUniform (_, _, _) ->
	printf "uniform"
    | SOutput (_, _) ->
	printf "output"
    | SDefine (_, _) ->
	printf "define"
;;

(* print_stmt: stmt -> unit
   Prints the given statement. *)
let rec print_stmt s = print_stmt_pretty s "\t";;

let rec print_stmt_latex s = 
  print_string "\\begin{displaymath}{\\small\n";
  print_string "\\begin{array}{l}\n";
  print_string "  ";
  print_string (render_stmt_pretty_latex s "  ");
  print_string "\\\\\n";
  print_string "\\end{array}\n";
  print_string "}\n\\end{displaymath}\n"
;;

(* collect_vars_aexp: aexp -> string list
   Finds all the variables used in the given expression. *)
let rec collect_vars_aexp e =
  match e with
  | AEInt (v) -> []
  | AEVar (id) -> [id]
  | AEBinop (b, exp1, exp2) -> List.append (collect_vars_aexp exp1) (collect_vars_aexp exp2)
;;

(* collect_vars_lexp: lexp -> string list
   Finds all the variables used in the given expression. *)
let rec collect_vars_lexp e =
  match e with
  | LEBool (v) -> []
  | LEBinop (b, lexp1, lexp2) -> List.append (collect_vars_lexp lexp1) (collect_vars_lexp lexp2)
  | LEReln (b, aexp1, aexp2) -> List.append (collect_vars_aexp aexp1) (collect_vars_aexp aexp2)
;;

(* collect_vars: stmt -> string list
   Finds all the variables used in the given statement. *)
let rec collect_vars s =
  match s with
    | SAssign (name, varaexp) -> name :: (collect_vars_aexp varaexp)
    | SSkip -> []
    | SSeq (s1, s2) -> List.append (collect_vars s1) (collect_vars s2)
    | SPSeq (s1, s2, p, n1, n2) -> List.append (collect_vars s1) (collect_vars s2)
    | SIf (guardlexp, s1, s2) ->
	List.append
	  (collect_vars_lexp guardlexp)
	  (List.append (collect_vars s1) (collect_vars s2))
    | SWhile (guardlexp, bodystmt) ->
	List.append (collect_vars_lexp guardlexp)
	  (collect_vars bodystmt)
    | SUniform (avarid, blower, bupper) -> [avarid]
    | SDefine (avarid, datatype) -> [avarid]
    | SOutput (avarid, toagents) -> [avarid]
;;

(* all_vars: stmt -> string list
   Finds all the unique variables used in the given statement. *)
let all_vars s =
  list_unique (collect_vars s);;

let _sa_of_stmt_newname indices name =
  let (owner, anid) = name in
    try 
      if Hashtbl.find indices name == 0 then
	name
      else
	(owner, "_" ^ anid ^ (string_of_int (Hashtbl.find indices name)))
    with Not_found ->
      raise (General_error ("name " ^ (varid_to_string name) ^ " not found"))
;;
    
let _sa_of_stmt_new_aexp indices name =
  let (owner, anid) = name in
    if Hashtbl.mem indices name then
      if Hashtbl.find indices name == 0 then
	AEVar name
      else
	AEVar (owner, "_" ^ anid ^ (string_of_int (Hashtbl.find indices name)))
    else
      AEInt (0)

let rec _sa_of_stmt_subst_aexp indices (e: aexp) : aexp =
  let raexp = _sa_of_stmt_subst_aexp indices in
    match e with
      | AEVar (id) -> AEVar (_sa_of_stmt_newname indices id)
      | AEBinop (b, exp1, exp2) -> AEBinop (b, raexp exp1, raexp exp2)
      | _ -> e
;;

let rec _sa_of_stmt_subst_lexp indices e : lexp =
  let raexp = _sa_of_stmt_subst_aexp indices in
  let rlexp = _sa_of_stmt_subst_lexp indices in
    match e with
      | LEBinop (b, exp1, exp2) ->
	  LEBinop (b, rlexp exp1, rlexp exp2)
      | LEReln (r, exp1, exp2) ->
	  LEReln(r, raexp exp1, raexp exp2)
      | _ -> e
;;

let _sa_of_stmt_subst_index indices v = if (Hashtbl.mem indices v) then (Hashtbl.find indices v) else -1;;

let rec _sa_of_stmt_subst_join s1 s2 indices1 indices2 =
  let set1 = ref [] in
  let set2 = ref [] in
  let newindices = Hashtbl.copy indices1 in

    Hashtbl.iter
      (fun varname index1 ->
	 let index2 = _sa_of_stmt_subst_index indices2 varname in
	   if index1 > index2 then set2 := varname :: (! set2))
      indices1;
    Hashtbl.iter
      (fun varname index2 ->
	 let index1 = _sa_of_stmt_subst_index indices1 varname in
	   if index1 < index2 then (
	     set1 := varname :: (! set1);
	     Hashtbl.replace newindices varname index2
	   ))
      indices2;
    let rets1 = ref s1 in
    let rets2 = ref s2 in
      List.iter
	(fun varname ->
	   rets1 := SSeq (! rets1,
			  SAssign (_sa_of_stmt_newname newindices varname,
				   _sa_of_stmt_new_aexp indices1 varname)))
	!set1;
      List.iter
	(fun varname ->
	   rets2 := SSeq (! rets2,
			  SAssign (_sa_of_stmt_newname newindices varname,
				   _sa_of_stmt_new_aexp indices2 varname)))
	!set2;
      (!rets1, !rets2, newindices)
;;      

let rec _sa_of_stmt_subst_stmt indices s =
  let raexp = _sa_of_stmt_subst_aexp indices in
  let rlexp = _sa_of_stmt_subst_lexp indices in
  let rstmt = _sa_of_stmt_subst_stmt indices in
    match s with
      | SAssign (name, varaexp) ->
	  let newvaraexp = raexp varaexp in
	  let newindices = Hashtbl.copy indices in
	    Hashtbl.replace newindices name (1 + (_sa_of_stmt_subst_index indices name));
	    (SAssign (_sa_of_stmt_newname newindices name, newvaraexp),
	     newindices)
      | SSkip -> (s, indices)
      | SSeq (s1, s2) ->
	  let (news1, newindices1) = rstmt s1 in
	  let (news2, newindices2) = _sa_of_stmt_subst_stmt newindices1 s2 in
	    (SSeq (news1, news2), newindices2)
      | SPSeq (s1, s2, p, n1, n2) -> 
	  let (news1, newindices1) = rstmt s1 in
	  let (news2, newindices2) = rstmt s2 in
	  let (finals1, finals2, finalindices) = _sa_of_stmt_subst_join news1 news2 newindices1 newindices2 in
	    (SPSeq (finals1, finals2, p, n1, n2), finalindices)
      | SIf (guardlexp, s1, s2) ->
	  let (news1, newindices1) = rstmt s1 in
	  let (news2, newindices2) = rstmt s2 in
	  let (finals1, finals2, finalindices) = _sa_of_stmt_subst_join news1 news2 newindices1 newindices2 in
	    (SIf (rlexp guardlexp, finals1, finals2), finalindices)
      | SWhile (guardlexp, bodystmt) -> 
	  let (newstmt, newindices) = rstmt bodystmt in
	    (SWhile (rlexp guardlexp, newstmt), newindices)
      | SUniform (varid, blower, bupper) ->
	  let newindices = Hashtbl.copy indices in
	    Hashtbl.replace newindices varid (1 + (_sa_of_stmt_subst_index indices varid));
	    (SUniform (_sa_of_stmt_newname newindices varid, blower, bupper),
	     newindices)
      | SDefine (varid, vartype) -> (s, indices)
      | _ -> raise (General_error "not implemented")
;;

let sa_of_stmt s inputs outputs =
  (* todo: last assignment of an output var needs not to be rewritten *)

  let indices = Hashtbl.create 8 in
    List.iter
      (fun v ->
	 Hashtbl.replace indices v 0;
      ) inputs;

  let itemp = Hashtbl.copy indices in
  let (ignore, itemp) = _sa_of_stmt_subst_stmt itemp s in

    List.iter
      (fun v ->
	 if (Hashtbl.mem itemp v) &&
	   (Hashtbl.find itemp v > 0) then
	     Hashtbl.replace indices v 0)
      outputs;

    let (nexts, nextindices) = _sa_of_stmt_subst_stmt indices s in

    let news = ref nexts in

    List.iter
	(fun v ->
	   (if (Hashtbl.mem nextindices v) && 
	      (Hashtbl.find nextindices v != 0) then 
	      news := SSeq (!news, (SAssign (v, AEVar (_sa_of_stmt_newname nextindices v))))))
	outputs;
      
      !news
;;

let rec fold_aexp f aaexp a =
  match aaexp with
    | AEInt (v) -> f aaexp a
    | AEVar (id) -> f aaexp a
    | AEBinop (b, exp1, exp2) -> (fold_aexp f exp2 (fold_aexp f exp1 (f aaexp a)))

let rec fold_lexp f alexp a =
    match alexp with
      | LEBool (v) -> f alexp a
      | LEBinop (b, exp1, exp2) -> fold_lexp f exp2 (fold_lexp f exp1 (f alexp a))
      | LEReln (r, exp1, exp2) -> f alexp a

let rec fold_stmt f astmt a =
  match astmt with
    | SAssign (name, varexp) -> f astmt a
    | SDefine (name, vartype) -> f astmt a
    | SSkip -> f astmt a
    | SSeq (s1, s2) ->  fold_stmt f s2 (fold_stmt f s1 (f astmt a))
    | SPSeq (s1, s2, p, n1, n2) -> fold_stmt f s2 (fold_stmt f s1 (f astmt a))
    | SIf (guardexp, s1, s2) -> fold_stmt f s2 (fold_stmt f s1 (f astmt a))
    | SWhile (guardexp, bodystmt) -> fold_stmt f bodystmt (f astmt a)
    | SUniform (varid, blower, bupper) -> f astmt a
    | SOutput (varid, agents) -> f astmt a

let rec fold_pstmt f apstmt a =
  match apstmt with
    | PSSubst (id, withaexp, inpstmt) -> fold_pstmt f inpstmt (f apstmt a)
    | PSInc (filename, includedfrom, inpstmt) -> fold_pstmt f inpstmt (f apstmt a)
    | PSStmt (astmt) -> f apstmt a

let find_outputs_for astmt anagent =
  list_unique (fold_stmt (fun astmt acc -> match astmt with
			    | SOutput (varid, agents) -> if List.mem anagent agents then varid :: acc else acc
			    | _ -> [])
		 astmt [])
;;

let get_output_agents astmt =
  list_unique (fold_stmt (fun astmt acc -> match astmt with
			    | SOutput (varid, agents) -> List.append agents acc
			    | _ -> [])
		 astmt [])
;;

exception Unchanged

let rec transform_aexp f aaexp =
  let r = transform_aexp f in
  let aaexp = 
    match aaexp with
      | AEBinop (b, exp1, exp2) -> AEBinop (b, r exp1, r exp2)
      | _ -> aaexp in
    try f aaexp 
    with Unchanged -> aaexp
;;

let rec transform_lexp flexp faexp alexp =
  let rlexp = transform_lexp flexp faexp in
  let raexp = transform_aexp faexp in
  let alexp =  match alexp with
    | LEBinop (b, exp1, exp2) -> LEBinop (b, rlexp exp1, rlexp exp2)
    | LEReln (r, exp1, exp2) -> LEReln (r, raexp exp1, raexp exp2)
    | _ -> alexp in
    try flexp alexp with
	Unchanged -> alexp  
;;

let rec transform_stmt fstmt flexp faexp astmt =
  let rstmt = transform_stmt fstmt flexp faexp in
  let rlexp = transform_lexp flexp faexp in
  let raexp = transform_aexp faexp in
  let astmt = match astmt with
    | SAssign (name, varexp) -> SAssign (name, raexp varexp)
    | SSeq (s1, s2) -> SSeq (rstmt s1, rstmt s2)
    | SPSeq (s1, s2, p, n1, n2) -> SPSeq (rstmt s1, rstmt s2, p, n1, n2)
    | SIf (guardexp, s1, s2) -> SIf (rlexp guardexp, rstmt s1, rstmt s2)
    | SWhile (guardexp, bodystmt) -> SWhile (rlexp guardexp, rstmt bodystmt)
    | _ -> astmt in
    try fstmt astmt with Unchanged -> astmt

