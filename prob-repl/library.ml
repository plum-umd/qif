open Printf
open Lang
open ExtHashtbl

let num2tonum fint freal a b =
  match (a,b) with
    | (VInt  a, VInt b) -> VInt (fint  a b)
    | (VReal a, VReal b) -> VReal (freal a b)
    | (VReal a, VInt b)
    | (VInt b, VReal a) -> VReal (freal a (float_of_int b))
    | _ -> raise (Type_error "same numeric value expected")

let num1tonum fint freal a =
  match (a) with
    | (VInt a) -> VInt (fint a)
    | (VReal a) -> VReal (freal a)
    | _ -> raise (Type_error "numeric value expected")

let bool2tobool fbool a b =
  match (a,b) with
    | (VBool a, VBool b) -> VBool (fbool a b)
    | _ -> raise (Type_error "boolean values expected")
;;

let num2tobool fint freal a b =
  match (a,b) with
    | (VInt  a, VInt b) -> VBool (fint a b)
    | (VReal a, VReal b) -> VBool (freal a b)
    | (VReal a, VInt b)
    | (VInt b, VReal a) -> VBool (freal a (float_of_int b))
    | _ -> raise (Type_error "same numeric value expected")

let rec values_numeric_equal v1 v2 =
  match (v1, v2) with
    | (VInt a, VInt b) -> a = b
    | (VReal a, VReal b) -> a = b
    | (VInt a, VReal b)
    | (VReal b, VInt a) -> (float_of_int a) = b
    | _ -> false

let rec values_numeric_less_or_equal v1 v2 =
  match (v1, v2) with
  | (VInt a, VInt b) -> a <= b
  | (VReal a, VReal b) -> a <= b
  | (VInt a, VReal b)
  | (VReal b, VInt a) -> (float_of_int a) <= b
  | _ -> false

let rec values_equal v1 v2 deref =
  match (v1, v2) with
  | (VUnit, VUnit) -> true
  | (VBool a, VBool b) -> a = b
  | (VInt a, VInt b) -> a = b
  | (VReal a, VReal b) -> a = b
  | (VString a, VString b) -> a = b
  | (VRef a, VRef b) -> values_equal (deref a) (deref b) deref
  | (VRecord a, VRecord b) -> 
    (List.length a = List.length b) &&
      begin List.for_all
          (fun (fname, fval) ->
            try let fval2 = List.assoc fname b in
                values_equal fval fval2 deref
            with _ -> false) a
      end
  | (VFun _, VFun _) -> raise (Type_error "functions cannot be compared")
  | (_, _) -> false

(*
let rec dump_value v1 deref =
  match v1 with
  | VRef a -> dump_value (deref a) deref
  | VRecord fl ->
    List.fold_left (fun a (fname, fval) ->
      List.append a
        (List.map (fun (pre, s) ->
          (sprintf ".%s%s" fname pre, s)) (dump_value fval deref))
    ) [] fl 
  | _ -> [("", v1)]
;;

let dump_state_values astate = 
  let pairs = astate#canon in
  List.fold_left (fun a (id, aval) ->
    List.append a
      (List.map
         (fun (pre, s) -> (sprintf "%s%s" id pre, s))
         (dump_value aval (fun i -> astate#heap_deref i))))
    [] pairs
;;
*)  

let logical_relations =
  [("and", bool2tobool (&&));
   ("or", bool2tobool (||))];;

let numeric_arithmetic =
  [("+", num2tonum (+) (+.));
   ("-", num2tonum (-) (-.));
   ("*", num2tonum ( * ) ( *. ));
   ("/", num2tonum (/) (/.))
  ];;

let numeric_relations =
  [("==", num2tobool ( = ) ( = ));
   ("<=", num2tobool ( <= ) ( <= ));
   ("<", num2tobool ( < ) ( < ));
   (">=", num2tobool ( >= ) ( >= ));
   (">", num2tobool ( > ) ( > ));
  ];;

let imp_hash =
  let temp_hash = Hashtbl.create
    ((List.length logical_relations) +
        (List.length numeric_arithmetic) +
        (List.length numeric_relations)
    ) in
  List.iter (fun (oname, oimp) -> Hashtbl.replace temp_hash oname oimp) logical_relations;
  List.iter (fun (oname, oimp) -> Hashtbl.replace temp_hash oname oimp) numeric_arithmetic;
  List.iter (fun (oname, oimp) -> Hashtbl.replace temp_hash oname oimp) numeric_relations;
  temp_hash
;;

let eval_binop opname a b = (Hashtbl.find imp_hash opname) a b
;;
  
let internal_int_to_int =
  [("sin", fun i -> int_of_float (sin (float_of_int i)));
   ("cos", fun i -> int_of_float (cos (float_of_int i)));
   ("tan", fun i -> int_of_float (tan (float_of_int i)));
   ("sqrt", fun i -> int_of_float (sqrt (float_of_int i)))
  ]
;;
