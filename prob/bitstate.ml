open Util
open Circuit
open Printf
open Lang

class type bitstate = object
  method get_circuit: circuit
  method set_circuit: circuit -> unit

  method set: Lang.varid -> bitint -> unit
  method get: Lang.varid -> bitint
  method vars: Lang.varid list
  method iter: (Lang.varid -> bitint -> unit) -> unit
  method to_string: string
  method print: unit
  method set_list: (Lang.varid * bitint) list -> unit
  method remove: Lang.varid -> unit
  method project: Lang.varid list -> unit
  method set_vals: (Lang.varid, bitvalue) Hashtbl.t -> unit
  method merge: bitstate -> unit

  method c_copy: bitstate
  method c_eq: bitstate -> bitbool
  method c_eq_on: bitstate -> bitbool
end;;

class state_hashed h c : bitstate = object (self)
  val mutable vals: (Lang.varid, bitint) Hashtbl.t = h
  val mutable c: circuit = c

  method get_circuit = c
  method set_circuit new_c = c <- new_c

  method set_vals h =
    ignore(vals <- h)

  method set varname varval = Hashtbl.replace vals varname varval

  method get varname: bitvalue = Hashtbl.find vals varname

  method vars: Lang.varid list = Hashtbl.fold (fun k v accum -> k :: accum ) vals []

  method iter f = Hashtbl.iter f vals

  method to_string =
    "<bistate"
    ^ (String.concat "," (List.map
			    (fun (k,v) -> sprintf "%s=" (varid_to_string k) )
			    (list_of_hash vals)))
    ^ ">"
      
  method print = print_string self#to_string

  method set_list sl =
    List.iter (fun (vname, vval) -> ignore (self#set vname vval)) sl

  method c_copy = new state_hashed (Hashtbl.copy vals) c

  method c_eq (s: bitstate) = raise Not_implemented
(*    let s1 = self#canon in
    let s2 = s#canon in
      (List.length
	 (List.filter
	    (fun e1 -> let (id1, val1) = e1 in
	       ((List.mem_assoc id1 s2) && (List.assoc id1 s2) = val1))
	    s1))
      = 
	(List.length s2)*)

  method c_eq_on (agreeon: bitstate) = raise Not_implemented
    (*List.for_all (fun (varname, varval) -> self#get varname = varval)
      agreeon#canon*)
	
  method merge (s: bitstate) =
    s#iter (fun varname varval -> ignore (self#set varname varval))

  method remove var =
    Hashtbl.remove vals var

  method project vars =
    let varsremove = list_subtract (self#vars) vars in
      List.iter (fun vname -> ignore(self#remove vname)) varsremove
end;;

class state_empty c = object
  inherit state_hashed (Hashtbl.create 4) c
end;;

(*
class state_default vars =
  let h = (
    let h = Hashtbl.create (List.length vars) in
      (List.iter (fun varid -> ignore (Hashtbl.replace h varid 0)) vars);
       h) in
object
  inherit state_hashed h
end;;
*)

let rec states_merge sl1 sl2 =
  match sl1 with
    | [] -> sl2
    | sh :: st ->
	if not (List.exists (fun s -> s#eq sh) sl2) then
	  sh :: states_merge st sl2
	else
	  states_merge st sl2

let bool_mux_states b s1 s2 =
  let ret = new state_empty s1#get_circuit in
    s1#iter
      (fun varid v1 ->
	 ret#set varid (Circuit.bool_mux_ints b v1 (s2#get varid)));
    ret
