open Lang
open Printf
open Util
open Parserutil
open ExtList
open Stateconcrete

class state_empty = Stateconcrete.state_empty;;

let rec eval_exp
    (astate: state)
    (aexp: exp): value =
  (*printf "current state=\n%s\n%!" (string_indent " " (astate#to_string));
    printf "current exp=%s\n\n---\n\n%!" (Lang.string_of_exp aexp);*)
  match aexp with
    | EValue v -> v
    | EIdent i -> astate#get i
    | ERecord h ->
      let he = List.map
        (fun (fname, fexp) -> (fname, eval_exp astate fexp)) h in
      let i = astate#heap_alloc (VRecord he) in
      VRef i
    | EMember (e, fname) -> 
      let aref = eval_exp astate e in
      let i = ref_of_value aref in
      let v = record_of_value (astate#heap_deref i) in
      (try List.assoc fname v
       with Not_found -> raise (Eval.Eval_error (sprintf "Record member %s not present" fname)))
    | EBinop ("and", e1, e2) -> (* for short circuiting conditionals *)
      let v1 = eval_exp astate e1 in
      if bool_of_value v1 then eval_exp astate e2
      else Lang.VBool false
    | EBinop ("or", e1, e2) -> (* for short circuiting conditionals *)
      let v1 = eval_exp astate e1 in
      if bool_of_value v1 then Lang.VBool true else eval_exp astate e2
    | EBinop ("=", e1, e2) ->
      let v1 = eval_exp astate e1 in
      let v2 = eval_exp astate e2 in
      if Library.values_equal v1 v2 (fun i -> astate#heap_deref i) then Lang.VBool true else Lang.VBool false
    | EApp (i, explist) ->
      let vf = eval_exp astate (EIdent i) in
      let vallist = List.map (fun e -> eval_exp astate e) explist in
      let (inlist, funstmt) = fun_of_value vf in
      begin
        if not (List.length inlist = List.length vallist) then
          raise (Eval.Eval_error (sprintf "incorrect number of arguments for function %s" i)) end;
      astate#call;
      List.iter (fun (inname, inval) ->
        astate#alloc inname;
        astate#set inname inval)
        (List.combine inlist vallist);
      let astate = eval_stmt astate funstmt in
      astate#getreturn
    | EUnop (_, _) -> failwith "unop not implemented"
    | EBinop (fname, e1, e2) ->
      let v1 = eval_exp astate e1 in
      let v2 = eval_exp astate e2 in
      begin try (List.assoc fname Library.numeric_arithmetic) v1 v2
        with Not_found ->
          begin
            try (List.assoc fname Library.numeric_relations) v1 v2
            with Not_found -> raise (Eval.Eval_error (sprintf "library does not have implementation of %s" fname))
          end
      end
    | ERandInt (lower, upper) -> VInt ((Random.int (upper - lower + 1)) + lower)
    | ERandReal (lower, upper) -> VReal ((Random.float (upper -. lower +. 1.0)) +. lower)
    | ERandBool p -> VBool (Random.float 1.0 <= p)

and eval_stmt
    (astate: Stateconcrete.state)
    (astmt: Lang.stmt): Stateconcrete.state =
  (*  printf "current state=\n%s\n%!" (string_indent " " (astate#to_string));
      printf "current stmt=\n%s\n\n---\n\n%!" (string_indent " " (Lang.string_of_stmt astmt));*)
  match astmt with
    | SSkip -> astate
    | SSeq (s1, s2) ->
      eval_stmt (eval_stmt astate s1) s2
    | SAssign (e1, e2) ->
      let lv = eval_exp astate e1 in
      let v = eval_exp astate e2 in
      begin match lv with 
        | VDeclared i -> astate#set i v
        | _ -> failwith "single assignment required"
      end;
      astate
    | SDefineValue (id, dt) ->
      let v = Lang.VDeclared id in
      astate#alloc id;
      astate#set id v;
      astate
    | SDefineFun(fname, dtout, inlist, bodystmt) ->
      astate#alloc fname;
      astate#set fname (VFun (List.map (fun (a, _) -> a) inlist, bodystmt));
      astate
    | SDefineType (ti, dt) ->
      astate (* ignore types for now *)
    | SIf (gexp, s1, s2) ->
      eval_stmt astate (if (bool_of_value (eval_exp astate gexp)) then s1 else s2)
    | SWhile (gexp, _, bs) ->
      let gv = ref (bool_of_value (eval_exp astate gexp)) in
      while (!gv) do
        ignore (eval_stmt astate bs);
        gv := (bool_of_value (eval_exp astate gexp))
      done;
      astate
    | SReturn e -> 
      astate#return (eval_exp astate e);
      astate
    | SAssert e ->
      let bv = eval_exp astate e in
      let v = bool_of_value bv in
      if v then astate
      else begin
        raise Eval.Assertion_failed
      end 
;;
