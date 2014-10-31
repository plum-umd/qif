open Gmp
open Lang

type tpolicy = {name: string;
		varlist: Lang.varid list;
		param: Q.t};;

type tquerydef = string * (Lang.varid list * Lang.varid list * Lang.pstmt);;
type tquerynamed = string * Lang.pstmt;;

type tpmocksetup = {secret: Lang.pstmt;
		    belief: Lang.pstmt;
		    policies: tpolicy list;
		    querydefs: tquerydef list;
		    queries: tquerynamed list}
;;

type texpsetup = {expsecret: Lang.pstmt;
		  expbelief: Lang.pstmt;
		  expprog: Lang.prog;
		  expqueries: Lang.pstmt list}
;;

let policy_record_label p =
  "policy type=" ^
    p.name ^
    " vars=" ^
    (String.concat "-" (List.map Lang.varid_to_string p.varlist)) ^
    " param=" ^
    (Q.to_string p.param)
