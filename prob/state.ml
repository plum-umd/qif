open Util

class type state = object
  method canon: (Lang.varid * int) list
  method addvar: Lang.varid -> unit
  method set: Lang.varid -> int -> unit
  method get: Lang.varid -> int
  method vars: Lang.varid list
  method iter: (Lang.varid -> int -> unit) -> unit
  method to_string: string
  method print: unit
  method set_list: (Lang.varid * int) list -> unit
  method copy: state
  method eq: state -> bool
  method eq_on: state -> bool
  method merge: state -> unit
  method remove: Lang.varid -> unit
  method project: Lang.varid list -> unit
  method set_vals: (Lang.varid, int) Hashtbl.t -> unit
end;;

class state_hashed h : state = object (self)
  val mutable vals: (Lang.varid, int) Hashtbl.t = h

  method set_vals h =
    ignore(vals <- h)

  method canon: (Lang.varid * int) list =
    Hashtbl.fold (fun k v accum -> (k, v) :: accum) vals []

  method addvar varname =
    Hashtbl.replace vals varname 0

  method set varname varval =
    if Hashtbl.mem vals varname then
      Hashtbl.replace vals varname varval
    else
      raise (General_error ("undefined variable " ^ (Lang.varid_to_string varname)))

  method get varname: int =
    try
      Hashtbl.find vals varname
    with
      | Not_found -> raise (General_error ("undefined variable " ^ (Lang.varid_to_string varname)))

  method vars: (Lang.varid list) = Hashtbl.fold (fun k v accum -> k :: accum ) vals []

  method iter f = Hashtbl.iter f vals

  method to_string =
    "[" ^
      (String.concat ", "
	 (List.map
	    (fun (id, v) -> (Lang.varid_to_string id) ^ "=" ^ string_of_int(v))
	    self#canon)) ^
      "]"

  method print = print_string self#to_string

  method set_list sl =
    List.iter (fun (vname, vval) -> ignore (self#addvar vname; self#set vname vval)) sl

  method copy = new state_hashed (Hashtbl.copy vals)

  method eq (s: state) = 
    let s1 = self#canon in
    let s2 = s#canon in
      (List.length
	 (List.filter
	    (fun e1 -> let (id1, val1) = e1 in
	       ((List.mem_assoc id1 s2) && (List.assoc id1 s2) = val1))
	    s1))
      = 
	(List.length s2)

  method eq_on (agreeon: state) = 
    List.for_all (fun (varname, varval) -> self#get varname = varval)
      agreeon#canon
	
  method merge (s: state) =
    s#iter (fun varname varval -> ignore (self#addvar varname; self#set varname varval))

  method remove var =
    Hashtbl.remove vals var

  method project vars =
    let varsremove = list_subtract (self#vars) vars in
      List.iter (fun vname -> ignore(self#remove vname)) varsremove
end;;

class state_empty = object
  inherit state_hashed (Hashtbl.create 4)
end;;

class state_default vars =
  let h = (
    let h = Hashtbl.create (List.length vars) in
      (List.iter (fun varid -> ignore (Hashtbl.replace h varid 0)) vars);
       h) in
object
  inherit state_hashed h
end;;

let rec states_merge sl1 sl2 =
  match sl1 with
    | [] -> sl2
    | sh :: st ->
	if not (List.exists (fun s -> s#eq sh) sl2) then
	  sh :: states_merge st sl2
	else
	  states_merge st sl2

module TEMP: Hashtbl.HashedType = struct
  type t = state
  let equal (s1: t) (s2: t) = s1#eq s2
  let hash (s: t) = Hashtbl.hash s#canon
end;;

module STATE_HASHABLE = Hashtbl.Make (TEMP);;
