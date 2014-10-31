open Printf
open Lang
open Preeval
open Pdefs
open Parser
open Parser_util

let render_latex asetup =
  let id_iter = (fun id ->
		   Lang.render_id_latex id
		) in 
    printf "$$ \\ssecretk: $$\n";
    Lang.print_pstmt_latex (Preeval.preeval_includes asetup.secret);
    printf "$$ \\sbeliefk: $$\n";
    Lang.print_pstmt_latex (Preeval.preeval_includes asetup.belief);
    printf "\n";
    List.iter
      (fun ((qname, (inlist, outlist, qstmt)): tquerydef) ->
	 printf "$$ \\squerydefk \\; %s \\; : \\;" (Lang.render_name_latex qname);
	 print_string (String.concat "\\;" (List.map id_iter inlist));
	 print_string " \\; \\ra \\; ";
	 print_string (String.concat "\\;" (List.map id_iter outlist));
	 print_string "$$\n";
	 Lang.print_pstmt_latex (Preeval.preeval_includes qstmt);
	 printf "\n";
      )
      asetup.querydefs;
    printf "\n";
    List.iter
      (fun (qname, argstmt) ->
	 printf "$$ \\squeryk \\; %s \\; : $$\n" (Lang.render_name_latex qname);
	 Lang.print_pstmt_latex (Preeval.preeval_includes argstmt);
       printf "\n";
      )
      asetup.queries
;;

let main () =
  let infile = ref "-" in
    Arg.parse [] (function s -> infile := s) "";

    Printexc.record_backtrace true;

    try
      render_latex (parse !infile Parser.pmock)
    with
      | e ->
	  Unix.chdir Globals.original_dir;
	  printf "%s\n" (Printexc.to_string e);
	  Printexc.print_backtrace stdout

;;

main ();;
