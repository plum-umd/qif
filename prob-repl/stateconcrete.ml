open ExtHashtbl
open ExtList
open ExtString
open Printf

type ident = Lang.valueident
type value = Lang.value

let value_null = Lang.VUnit

type heap_type = value DynArray.t

class heap initheap = object (self)
  val mutable heap_data: heap_type = initheap

  method print = print_string self#to_string

  method copy = new heap (DynArray.copy heap_data)

  method to_string =
    String.concat "\n"
      (DynArray.to_list
         (DynArray.mapi (fun i v ->
           sprintf "%d: %s" i (Lang.string_of_value v))
            heap_data))

  method deref i = DynArray.get heap_data i
  method set i v = DynArray.set heap_data i v
  method alloc v =
    DynArray.add heap_data v;
    (DynArray.length heap_data) - 1      
end

class heap_empty = object
  inherit heap (DynArray.create ())
end

type stackval =
  | StCall
  | StReturn of value
  | StAlloc of (ident * (value option))

let string_of_stackval sv =
  match sv with
    | StCall -> "call"
    | StReturn aval -> sprintf "return %s" (Lang.string_of_value aval)
    | StAlloc (varname, None) -> sprintf "alloc %s" varname
    | StAlloc (varname, Some oldval) -> sprintf "alloc %s (was %s)" varname (Lang.string_of_value oldval)
;;

let string_of_stackval_stack s =
  let s = Stack.copy s in
  let temp = ref [] in
  while (not (Stack.is_empty s)) do
    temp := (string_of_stackval (Stack.pop s)) :: !temp
  done;
  String.concat "\n" !temp
;;

class state initvals initheap initstack = object (self)
  val mutable vals: (ident, value) Hashtbl.t = initvals
  val mutable heap: heap = initheap
  val mutable stack: stackval Stack.t = initstack

  method canon: (ident * value) list =
    Hashtbl.fold (fun k v accum -> (k, v) :: accum) vals []

  method get varname: value =
    try
      Hashtbl.find vals varname
    with 
      | Not_found -> raise (Eval.Undefined_variable (Lang.string_of_ident varname))

  method alloc varname =
    Stack.push (StAlloc (varname, try Some (Hashtbl.find vals varname) with Not_found -> None)) stack;
    Hashtbl.replace vals varname value_null

  method call = Stack.push StCall stack

  method return aval =
    while (match Stack.top stack with | StCall -> false | _ -> true) do
      match Stack.pop stack with
        | StAlloc (varname, None) -> Hashtbl.remove vals varname
        | StAlloc (varname, Some oldval) -> Hashtbl.replace vals varname oldval
        | _ -> failwith "unexpected elements in stack"
    done;
    ignore (Stack.pop stack);
    Stack.push (StReturn aval) stack

  method getreturn = match Stack.pop stack with
    | StReturn aval -> aval
    | _ -> failwith "return expected in stack"

  method set varname varval =
    if Hashtbl.mem vals varname then
      Hashtbl.replace vals varname varval
    else
      failwith ("undefined variable [" ^ (Lang.string_of_ident varname) ^ "]")

  method copy = new state (Hashtbl.copy vals) (heap#copy) (Stack.copy stack)

  method heap_deref i = heap#deref i
  method heap_set i v = heap#set i v
  method heap_alloc v = heap#alloc v

  method to_string =
    "bindings:\n" ^
      (Util.string_indent "  "
         (String.concat "\n"
	    (List.map
	       (fun (id, v) -> (Lang.string_of_ident id) ^ "=" ^ (Lang.string_of_value v))
	       self#canon))) ^ "\n" ^
      "stack:\n" ^ (Util.string_indent "  " (string_of_stackval_stack stack)) ^ "\n" ^
      "heap:\n" ^ (Util.string_indent "  " heap#to_string)

  method print = print_string self#to_string

  method eval_lvalue aexp = match aexp with
    | Lang.EIdent i -> self#get i
(*    | Lang.EMember (Lang.EIdent recname, afield) ->
      List.assoc afield (Lang.record_of_value (self#heap_deref (Lang.ref_of_value (self#get recname))))*)
    | Lang.EMember (lv, afield) ->
      let arec = Lang.record_of_value (self#heap_deref (Lang.ref_of_value (self#eval_lvalue lv))) in
      List.assoc afield arec
    | _ -> failwith (sprintf "lvalue expected: %s" (Lang.string_of_exp aexp))

  method dump_view aview =
    Array.map (fun aexp -> 
      try let v = self#eval_lvalue aexp in
          match v with
            | Lang.VInt x -> (float_of_int x)
            | Lang.VDeclared _ -> 0.0
            | _ -> failwith "can only visualize integers"
      with Eval.Undefined_variable _ -> 0.0) aview


end and  state_empty = object
  inherit state (Hashtbl.create 4) (new heap_empty) (Stack.create ())
end;;

let rec states_merge sl1 sl2 =
  match sl1 with
    | [] -> sl2
    | sh :: st ->
      if not (List.exists (fun s -> s#eq sh) sl2) then
	sh :: states_merge st sl2
      else
	states_merge st sl2
