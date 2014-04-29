open Lang
open Printf

(*
let rec type_value (aval: Lang.value) =
  | VUnit -> TUnit
  | VBool _ -> TBool
  | VInt _ -> TInt
  | VReal _ -> TReal
  | VString _ -> TString
  | _ -> failwith "should not be here"

and type_exp tenv (aexp: Lang.exp) =
  match aexp with
  | EValue v -> type_value i
  | EIdent i -> Hashtbl.find tenv i
  | ERecord h ->
    let he = List.map
      (fun (fname, fexp) -> (fname, type_exp tenv fexp)) h in
    TRecord he
  | EMember (e, fname) -> 
    let arec = type_exp tenv e in
    match arec with
    | TRecord th ->
      (try List.assoc fname th
       with Not_found -> raise (Eval_error (sprintf "Record member %s not present" fname)))
  | EBinop ("and", e1, e2) -> (* for short circuiting conditionals *)
    let v1 = eval_exp astate aheap e1 in
    if bool_of_value v1 then eval_exp astate aheap e2
    else Lang.VBool false
  | EBinop ("or", e1, e2) -> (* for short circuiting conditionals *)
    let v1 = eval_exp astate aheap e1 in
    if bool_of_value v1 then Lang.VBool true else eval_exp astate aheap e2
  | EBinop ("<=", e1, e2) ->
    let v1 = eval_exp astate aheap e1 in
    let v2 = eval_exp astate aheap e2 in
    if v1 <= v2 then Lang.VBool true else Lang.VBool false
  | EBinop ("=", e1, e2) ->
    let v1 = eval_exp astate aheap e1 in
    let v2 = eval_exp astate aheap e2 in
    if Library.values_equal v1 v2 (fun i -> aheap#deref i) then Lang.VBool true else Lang.VBool false
  | EApp (i, explist) ->
    let newstate = astate#copy in
    let vf = eval_exp newstate aheap (EIdent i) in
    let vallist = List.map (fun e -> eval_exp astate aheap e) explist in
    let (inlist, funstmt) = fun_of_value vf in
    List.iter (fun (inname, inval) ->
      newstate#addvar inname;
      newstate#set inname inval)
      (List.combine inlist vallist);
    snd (eval_stmt newstate aheap funstmt)
  | EUnop (_, _) -> failwith "unop not implemented"
  | EBinop (_, _, _) -> failwith "other binops not implemented"
and const_stmt (astmt: Lang.stmt) =
  match astmt with
  | SSkip -> []
  | SSeq (s1, s2) -> List.append (const_stmt s1) (const_stmt s2)
  | SAssign (e1, e2) -> 
    let lv = eval_exp astate aheap e1 in
    let v = eval_exp astate aheap e2 in
    begin match lv with 
    | VDeclared i -> astate#set i v
    | _ -> failwith "single assignment required"
    end;
    (astate, VALUE.null) 
  | SDefineValue (id, dt) ->
    let v = VALUE.declared id in
    astate#addvar id;
    astate#set id v;
    (astate, v)
  | SDefineFun(fname, dtout, inlist, bodystmt) ->
    astate#addvar fname;
    astate#set fname (VFun (List.map (fun (a, _) -> a) inlist, bodystmt));
    (astate, VALUE.null)
  | SDefineType (ti, dt) ->
    (astate, VALUE.null) (* ignore types for now *)
  | SIf (gexp, s1, s2) ->
    eval_stmt astate aheap (if (bool_of_value (eval_exp astate aheap gexp)) then s1 else s2)
  | SWhile (gexp, _, bs) ->
    let gv = ref (bool_of_value (eval_exp astate aheap gexp)) in
    let v = ref VALUE.null in
    while (!gv) do
      let (_, nv) = eval_stmt astate aheap bs in
      v := nv;
      gv := (bool_of_value (eval_exp astate aheap gexp))
    done;
    (astate, !v)
  | SReturn e -> (astate, eval_exp astate aheap e)
  | SAssert e ->
    let bv = eval_exp astate aheap e in
    let v = bool_of_value bv in
    if v then (astate, bv)
    else begin
      raise Assertion_failed
    end 
(*  | _ -> failwith "statement not implemented"*)


let typecheck astmt = 
  
*)
