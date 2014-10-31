open Gmp

type tpolicy = {
  agent: string;
  aboutagents: string list;
  varlist: Lang.varid list;
  param: Q.t
};;

type tquerydef   = string * (Lang.varid list * Lang.varid list * Lang.pstmt);;
type tquerynamed = string * Lang.pstmt;;

type tsecret = (string * Lang.pstmt)
type tbelief = ((Lang.agent list) * (Lang.agent list) * Lang.pstmt)

type tsmcmocksetup = {secrets: tsecret list;
		      beliefs: tbelief list;
		      policies: tpolicy list;
		      querydefs: tquerydef list;
		      queries: tquerynamed list}
;;

let policy_record_label p = "" ^
  "vars=" ^
  (String.concat "-" (List.map Lang.varid_to_string p.varlist)) ^
  " param=" ^
  (Q.to_string p.param)
