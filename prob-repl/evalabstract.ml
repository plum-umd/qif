open Lang
open Printf
open Util
open Parserutil
open ExtList
open Ppl_ocaml
open Gmp

let split_rate = ref 2;;

module Lib = Librarylinear

module type ABSEVAL_TYPE = sig
  module AS: Stateabstract.ABSTRACT_STATE_TYPE

  type region

  class absstate_empty: AS.absstate

  val join_results: (Lang.value * AS.absstate) list -> AS.absstate
  val evalk_exp: AS.absstate -> Lang.exp -> (Lang.value * AS.absstate, Lang.value * AS.absstate) splitcontinue
  val eval_stmt: AS.absstate -> Lang.stmt -> AS.absstate
end;;

module ABSTRACT_EVAL_MAKER (PD: Ppldomain.PPLDOMAIN_TYPE)
  : (ABSEVAL_TYPE with type region = PD.region) = struct

    module AS = Stateabstract.ABSTRACT_STATE_MAKER (PD)

    type absstate = AS.absstate
    type substate = AS.substate
    type region = PD.region

    let join_results rl = new AS.join_many (List.map snd rl)

    class absstate_empty = AS.absstate_empty

    let rec eval_equalsk: 'a. absstate -> value -> value -> (value * absstate, 'a) splitcontinue = fun astate a b k ->
      match (a,b) with
        | (VRecord a1, VRecord b1) -> 
(*          printf "equating record\n%!";*)
          let rec temp astate vl1 vl2 k =
            match (vl1, vl2) with
              | ([], []) -> k (VBool true, astate)
              | ((fname1, h1) :: r1, (fname2, h2) :: r2) when fname1 = fname2 ->
(*                printf " equating %s %s\n%!" (Lang.string_of_value h1) (Lang.string_of_value h2);*)
                eval_equalsk astate h1 h2 (fun (v, astate) ->
                  match v with 
                    | VBool false -> k (VBool false, astate)
                    | VBool true -> temp astate r1 r2 k
                    | _ -> raise (Type_error (sprintf "bool expected, got %s" (Lang.string_of_value v))))
              | _ -> raise (Type_error
                              (sprintf "cannot equate differing records: %s and %s"
                                 (Lang.string_of_value a)
                                 (Lang.string_of_value b))) in temp astate b1 a1 k
        | (VRef i, VRef j) ->
(*          printf "equating references %d %d\n%!" i j;*)
          astate#heap_derefk i (fun (v1, astate) ->
            astate#heap_derefk j (fun (v2, astate) ->
              eval_equalsk astate v1 v2 k))
        | (VBool a, VBool b) -> k (VBool (a = b), astate)
        | _ -> astate#view_as_concretek (Lib.eval_binop "==" (Lib.RConcrete a) (Lib.RConcrete b)) k
    ;;

    let rec evalk_exp : 'a. AS.absstate -> Lang.exp ->  (value * AS.absstate, 'a) splitcontinue =
      fun astate aexp k ->
        (*printf "current absstate=\n%s\n%!" (string_indent " " (astate#to_string));
          printf "current exp=%s\n\n---\n\n%!" (Lang.string_of_exp aexp);*)
        match aexp with
          | EValue v -> k (v, astate)

          | EIdent i -> astate#getk i (fun (v,astate) -> k (v, astate))

          | ERecord h ->
            let rec temp astate h k = match h with
              | [] -> k (astate, [])
              | (fname, fexp) :: rest ->
                evalk_exp astate fexp
                  (fun (aval, astate) -> 
                    temp astate rest (fun (astate, bindings) -> k (astate, (fname, aval) :: bindings))) in
            temp astate h
              (fun (astate, bindings) ->
                astate#heap_allock (VRecord bindings) (fun (i, astate) ->
                  k (VRef i, astate)))

          | EMember (e, fname) ->
            evalk_exp astate e
              (fun (aref, astate) ->
                let i = ref_of_value aref in
                astate#heap_derefk i
                  (fun (arec, astate) ->
                    let v = record_of_value arec in
                    try k ((List.assoc fname v), astate)
                    with Not_found -> raise (Eval.Eval_error (sprintf "record member %s not present" fname))))

          | EBinop ("=", e1, e2) ->
(*            printf "in binop = \n%!";*)
            evalk_exp astate e1 (fun (v1, astate) ->
              evalk_exp astate e2 (fun (v2, astate) ->
                (eval_equalsk astate v1 v2
                   (fun (v, astate) -> k (v, astate)))))

          | EBinop (opname, e1, e2) ->
            evalk_exp astate e1 (fun (v1, astate) ->
              evalk_exp astate e2 (fun (v2, astate) ->
                astate#view_as_concretek (Lib.eval_binop opname (Lib.RConcrete v1) (Lib.RConcrete v2))
                  (fun (v, astate) -> k (v, astate))))

          | EApp (i, explist) -> 
            let rec temp astate h k = match h with
              | [] -> k (astate, [])
              | aexp :: rest ->
                evalk_exp astate aexp
                  (fun (aval, astate) -> temp astate rest (fun (astate, vals) -> k (astate, aval :: vals))) in

            evalk_exp astate (EIdent i) (fun (vf, astate) ->
              let (inlist, funstmt) = fun_of_value vf in
              temp astate explist
                (fun (astate, vallist) ->
                  begin if not (List.length inlist = List.length vallist)
                    then raise (Eval.Eval_error (sprintf "incorrect number of arguments for function %s" i)) end;
                  astate#call;
                  List.iter (fun (inname, inval) ->
                    astate#alloc inname;
                    astate#set_concrete inname inval) (List.combine inlist vallist);
                  let astate = eval_stmt astate funstmt in
                  astate#getreturnk (fun (aval, astate) -> k (aval, astate))))
              
          | ERandInt (lower, upper) ->
            astate#abs_allock
              (fun (i, astate) ->
                astate#splitrangek lower upper !split_rate
                  (fun (l, u, astate) ->
                    astate#prob_scale
                      (Q.from_ints 1 (upper - lower + 1))
                      (Q.from_ints (u - l + 1) (upper - lower + 1));
                    astate#prob_mult_points (Z.from_int (u - l + 1));
                    astate#abs_add_constraints [(Less_Or_Equal (Variable i, Coefficient (Z.of_int u)));
                                                (Greater_Or_Equal (Variable i, Coefficient (Z.of_int l)))];
                    (*astate#*)
                    k (VAbsRef i, astate)))
              
          | ERandReal (lower, upper) -> failwith "not implemented"

          | ERandBool p ->
            let temp1 = astate in
            let temp2 = astate#copy in
            temp1#prob_scale (Q.from_float p) (Q.from_float p);
            temp2#prob_scale (Q.from_float (1.0 -. p)) (Q.from_float (1.0 -. p));
            List.append
              (k (Lang.VBool true, temp1))
              (k (Lang.VBool false, temp2))

          | _ -> failwith "other operations not implemented"

    and eval_stmt (astate: AS.absstate) (astmt: Lang.stmt): AS.absstate =
      (*  printf "current state=\n%s\n%!" (string_indent " " (astate#to_string));
          printf "current stmt=\n%s\n\n---\n\n%!" (string_indent " " (Lang.string_of_stmt astmt));*)
      match astmt with
        | SSkip -> astate
        | SSeq (s1, s2) ->
          eval_stmt (eval_stmt astate s1) s2
        | SAssign (e1, e2) ->
          new AS.join_many (evalk_exp astate e1
                              (fun (lv, astate) ->
                                begin match lv with 
                                  | VDeclared i -> evalk_exp astate e2 (fun (v, astate) -> 
                                    astate#set_concrete i v; [astate])
                                  | _ -> failwith "single assignment required"
                                end))
        | SDefineValue (id, dt) ->
          let v = Lang.VDeclared id in
          astate#alloc id;
          astate#set_concrete id v;
          astate
        | SDefineFun(fname, dtout, inlist, bodystmt) ->
          astate#alloc fname;
          astate#set_concrete fname (VFun (List.map (fun (a, _) -> a) inlist, bodystmt));
          astate
        | SDefineType (ti, dt) ->
          astate (* ignore types for now *)
        | SIf (gexp, s1, s2) ->
          new AS.join_many (evalk_exp astate gexp (fun (v, astate) ->
            if bool_of_value v then
              [eval_stmt astate s1]
            else
              [eval_stmt astate s2]))
        | SWhile (gexp, _, bs) -> failwith "not implemented"

        (*          let gv = ref (bool_of_value (eval_exp astate gexp)) in
                    let v = ref Stateabstract.value_null in
                    while (!gv) do
                    let (_, nv) = eval_stmt astate bs in
                    v := nv;
                    gv := (bool_of_value (eval_exp astate gexp))
                    done;
                    (astate, !v)*)
        | SReturn e ->
          new AS.join_many
            (evalk_exp astate e (fun (aval, astate) ->
              astate#return aval; [astate]))
        | SAssert e ->
          new AS.join_many
            (evalk_exp astate e
               (fun (aval, astate) -> match aval with
                 | VBool true -> [astate]
                 | VBool false -> []
                 | _ -> raise (Type_error "cannot assert on non-bool")
               ))
  end
;;

(*
module type ABSEVAL_TYPE_MONADIC = sig
  module M: Stateabstract.ABSTRACT_STATE_TYPE_MONADIC

  type region

  val return_new: unit M.return

  (*val join_results: (Lang.value * AS.absstate) list -> AS.absstate*)

  val bind_eval_exp: Lang.exp -> Lang.value M.absbind
  val bind_eval_stmt: unit M.absbind
end;;

module ABSTRACT_EVAL_MAKER_MONADIC (PD: Ppldomain.PPLDOMAIN_TYPE)
  : (ABSEVAL_TYPE_MONADIC with type region = PD.region) = struct

    module M = Stateabstract.ABSTRACT_STATE_MAKER (PD)

    type absstate = AS.absstate
    type substate = AS.substate
    type region = PD.region

    (*let join_results rl = new AS.join_many (List.map snd rl)*)

    let return_new () = M.return ()

      (*
    let rec eval_equalsk: 'a. absstate -> value -> value -> (value * absstate, 'a) splitcontinue = fun astate a b k ->
      match (a,b) with
        | (VRecord a1, VRecord b1) -> 
(*          printf "equating record\n%!";*)
          let rec temp astate vl1 vl2 k =
            match (vl1, vl2) with
              | ([], []) -> k (VBool true, astate)
              | ((fname1, h1) :: r1, (fname2, h2) :: r2) when fname1 = fname2 ->
(*                printf " equating %s %s\n%!" (Lang.string_of_value h1) (Lang.string_of_value h2);*)
                eval_equalsk astate h1 h2 (fun (v, astate) ->
                  match v with 
                    | VBool false -> k (VBool false, astate)
                    | VBool true -> temp astate r1 r2 k
                    | _ -> raise (Type_error (sprintf "bool expected, got %s" (Lang.string_of_value v))))
              | _ -> raise (Type_error
                              (sprintf "cannot equate differing records: %s and %s"
                                 (Lang.string_of_value a)
                                 (Lang.string_of_value b))) in temp astate b1 a1 k
        | (VRef i, VRef j) ->
(*          printf "equating references %d %d\n%!" i j;*)
          astate#heap_derefk i (fun (v1, astate) ->
            astate#heap_derefk j (fun (v2, astate) ->
              eval_equalsk astate v1 v2 k))
        | (VBool a, VBool b) -> k (VBool (a = b), astate)
        | _ -> astate#view_as_concretek (Lib.eval_binop "==" (Lib.RConcrete a) (Lib.RConcrete b)) k
    ;;*)

    let rec bind_eval_exp : Lang.exp -> Lang.value M.absbind
      fun aexp astate ->
        (*printf "current absstate=\n%s\n%!" (string_indent " " (astate#to_string));
          printf "current exp=%s\n\n---\n\n%!" (Lang.string_of_exp aexp);*)

        match aexp with
          | EValue v -> M.bind (M.return v)
          | EIdent i -> M.bind_get i absstate
(*
          | ERecord h ->
            let rec temp astate h k = match h with
              | [] -> k (astate, [])
              | (fname, fexp) :: rest ->
                evalk_exp astate fexp
                  (fun (aval, astate) -> 
                    temp astate rest (fun (astate, bindings) -> k (astate, (fname, aval) :: bindings))) in
            temp astate h
              (fun (astate, bindings) ->
                astate#heap_allock (VRecord bindings) (fun (i, astate) ->
                  k (VRef i, astate)))

          | EMember (e, fname) ->
            evalk_exp astate e
              (fun (aref, astate) ->
                let i = ref_of_value aref in
                astate#heap_derefk i
                  (fun (arec, astate) ->
                    let v = record_of_value arec in
                    try k ((List.assoc fname v), astate)
                    with Not_found -> raise (Eval.Eval_error (sprintf "record member %s not present" fname))))

          | EBinop ("=", e1, e2) ->
(*            printf "in binop = \n%!";*)
            evalk_exp astate e1 (fun (v1, astate) ->
              evalk_exp astate e2 (fun (v2, astate) ->
                (eval_equalsk astate v1 v2
                   (fun (v, astate) -> k (v, astate)))))
*)
          | EBinop (opname, e1, e2) ->
            bind_eval_exp e1 astate
              (fun v1 astate ->
                bind_eval_exp e2 astate -> 
                  (fun v2 astate ->
                    bind_view (Lib.eval_binop opname (Lib.RConcrete v1) (Lib.RConcrete v2)) astate
                      (fun v astate -> M.return v)))

              (*
          | EApp (i, explist) -> 
            let rec temp astate h k = match h with
              | [] -> k (astate, [])
              | aexp :: rest ->
                evalk_exp astate aexp
                  (fun (aval, astate) -> temp astate rest (fun (astate, vals) -> k (astate, aval :: vals))) in

            evalk_exp astate (EIdent i) (fun (vf, astate) ->
              let (inlist, funstmt) = fun_of_value vf in
              temp astate explist
                (fun (astate, vallist) ->
                  begin if not (List.length inlist = List.length vallist)
                    then raise (Eval.Eval_error (sprintf "incorrect number of arguments for function %s" i)) end;
                  astate#call;
                  List.iter (fun (inname, inval) ->
                    astate#alloc inname;
                    astate#set_concrete inname inval) (List.combine inlist vallist);
                  let astate = eval_stmt astate funstmt in
                  astate#getreturnk (fun (aval, astate) -> k (aval, astate))))
              
          | ERandInt (lower, upper) ->
            astate#abs_allock
              (fun (i, astate) ->
                astate#splitrangek lower upper !split_rate
                  (fun (l, u, astate) ->
                    astate#prob_scale
                      (Q.from_ints 1 (upper - lower + 1))
                      (Q.from_ints (u - l + 1) (upper - lower + 1));
                    astate#prob_mult_points (Z.from_int (u - l + 1));
                    astate#abs_add_constraints [(Less_Or_Equal (Variable i, Coefficient (Z.of_int u)));
                                                (Greater_Or_Equal (Variable i, Coefficient (Z.of_int l)))];
                    (*astate#*)
                    k (VAbsRef i, astate)))
              
          | ERandReal (lower, upper) -> failwith "not implemented"

          | ERandBool p ->
            let temp1 = astate in
            let temp2 = astate#copy in
            temp1#prob_scale (Q.from_float p) (Q.from_float p);
            temp2#prob_scale (Q.from_float (1.0 -. p)) (Q.from_float (1.0 -. p));
            List.append
              (k (Lang.VBool true, temp1))
              (k (Lang.VBool false, temp2))
              *)
          | _ -> failwith "other operations not implemented"

    and eval_stmt (astate: AS.absstate) (astmt: Lang.stmt): AS.absstate =
      (*  printf "current state=\n%s\n%!" (string_indent " " (astate#to_string));
          printf "current stmt=\n%s\n\n---\n\n%!" (string_indent " " (Lang.string_of_stmt astmt));*)
      match astmt with
        | SSkip -> astate
        | SSeq (s1, s2) ->
          eval_stmt (eval_stmt astate s1) s2
        | SAssign (e1, e2) ->
          new AS.join_many (evalk_exp astate e1
                              (fun (lv, astate) ->
                                begin match lv with 
                                  | VDeclared i -> evalk_exp astate e2 (fun (v, astate) -> 
                                    astate#set_concrete i v; [astate])
                                  | _ -> failwith "single assignment required"
                                end))
        | SDefineValue (id, dt) ->
          let v = Lang.VDeclared id in
          astate#alloc id;
          astate#set_concrete id v;
          astate
        | SDefineFun(fname, dtout, inlist, bodystmt) ->
          astate#alloc fname;
          astate#set_concrete fname (VFun (List.map (fun (a, _) -> a) inlist, bodystmt));
          astate
        | SDefineType (ti, dt) ->
          astate (* ignore types for now *)
        | SIf (gexp, s1, s2) ->
          new AS.join_many (evalk_exp astate gexp (fun (v, astate) ->
            if bool_of_value v then
              [eval_stmt astate s1]
            else
              [eval_stmt astate s2]))
        | SWhile (gexp, _, bs) -> failwith "not implemented"

        (*          let gv = ref (bool_of_value (eval_exp astate gexp)) in
                    let v = ref Stateabstract.value_null in
                    while (!gv) do
                    let (_, nv) = eval_stmt astate bs in
                    v := nv;
                    gv := (bool_of_value (eval_exp astate gexp))
                    done;
                    (astate, !v)*)
        | SReturn e ->
          new AS.join_many
            (evalk_exp astate e (fun (aval, astate) ->
              astate#return aval; [astate]))
        | SAssert e ->
          new AS.join_many
            (evalk_exp astate e
               (fun (aval, astate) -> match aval with
                 | VBool true -> [astate]
                 | VBool false -> []
                 | _ -> raise (Type_error "cannot assert on non-bool")
               ))
  end
;;
*)
