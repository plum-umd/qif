#load "pa_extend.cmo";
#load "q_MLast.cmo";
open Pcaml;

EXTEND
  expr: LEVEL "expr1"
  [
      [ "ifdebug"; e1 = expr ->
          <:expr< Globals.do_ifdebug (fun () -> $e1$) >>
      ]
  |
      [ "ifverbose"; e1 = expr ->
          <:expr< Globals.do_ifverbose (fun () -> $e1$) >>
      ]
  |
      [ "ifbench"; e1 = expr ->
          <:expr< Globals.do_ifbench (fun () -> $e1$) >>
      ]
(*  |
      [ "letmem"; e1 = expr; fid = patt; argid = patt ->
	  <:expr< 
	    let rec $lid:fid$ $lid:argid$ =
	      (let h = Hashtbl.create 32 in
		 (fun $lid:argid$ ->
		    try Hashtbl.find h $lid:argid$
		    with Not_found ->
		      (let temp = 42 in
			 (*Hashtbl.replace h $lid:argid$ temp;*)
			 temp)))
	    in 
	      42
		    >>	    
      ]*)
  ]
;
END;
