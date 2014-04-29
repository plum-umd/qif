open Lang
open Parserutil

exception Preprocessor_error of string

let rec preprocess_includes pstmt = match pstmt with
  | PSInclude (filename, pstmt2) -> begin
    try 
      let temp = parse_file filename Parser.pstmt_eof in
      PSSeq (preprocess_includes temp, pstmt2)
    with 
      | Parse_message msg -> raise (Preprocessor_error ("preprocessor error: " ^ msg))
      | e -> raise (Preprocessor_error (Printexc.to_string e))
  end
  | PSDefineExp (id, exp, pstmt3) ->
    PSDefineExp (id, exp, preprocess_includes pstmt3)
  | PSStmt astmt -> PSStmt astmt
  | PSSeq (s1, s2) -> PSSeq (preprocess_includes s1, preprocess_includes s2)
  | PSUi s -> PSUi s

let rec pstmt_subst_exp id withexp inwhat = match inwhat with
  | PSDefineExp (id2, withexp2, inwhat2) ->
    if id = id2 then inwhat
    else PSDefineExp (
      id2,
      Lang.exp_subst_exp id withexp withexp2,
      pstmt_subst_exp id withexp inwhat2
    )
  | PSSeq (s1, s2) ->
    PSSeq (pstmt_subst_exp id withexp s1,
           pstmt_subst_exp id withexp s2)
  | PSStmt astmt -> PSStmt (Lang.stmt_subst_exp id withexp astmt)
  | _ -> failwith "includes should be taken care of first"

let rec remove_skips astmt = match astmt with
  | SSeq (SSkip, s)
  | SSeq (s, SSkip) -> remove_skips s
  | SDefineFun (a, b, c, s) -> SDefineFun (a,b,c, remove_skips s)
  | SIf (a, s1, s2) -> SIf (a, remove_skips s1, remove_skips s2)
  | SWhile (a, b, s) -> SWhile (a, b, remove_skips s)
  | _ -> astmt

let preprocess pstmt =
  let cmds = ref [] in
  let temp = preprocess_includes pstmt in
  let rec pre pstmt = match pstmt with
    | PSDefineExp (id, exp, pstmt3) -> pre (pstmt_subst_exp id exp pstmt3)
    | PSStmt astmt -> astmt
    | PSSeq (s1, s2) -> SSeq (pre s1, pre s2)
    | PSUi s ->
      cmds := s :: !cmds; SSkip
    | _ -> failwith "includes should not occur here" in
  (remove_skips (pre temp), !cmds)
