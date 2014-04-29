open Printf
open Lang
open ExtHashtbl
open Ppl_ocaml
open Gmp
open Gmp.Z.Infixes
open Lang

exception Non_linear of string

type result =
  | RConcrete of Lang.value
  | RLinear of linear_expression
  | RLinearConstraints of (linear_constraint list list) * (linear_constraint list list)

let concrete_of_result r = match r with
  | RConcrete v -> v
  | _ -> failwith "concrete value expected"
;;

let linear_of_result r = match r with
  | RLinear v -> v
  | _ -> failwith "linear expression expected"
;;

let linearconstraints_of_result r = match r with
  | RLinearConstraints (le1, le2) -> (le1, le2)
  | _ -> failwith "linear constraints expected"
;;

let _base_and lc1 lc2 = List.append lc1 lc2
let _and lc1 lc2 =
  List.fold_left
    (fun a l1 ->
      List.append a
        (List.map (fun l2 -> _base_and l1 l2) lc2))
    [] lc1

let linear_logical_relations =
  [("and",
    fun a b ->
      let ((pos1, neg1), (pos2, neg2)) = (linearconstraints_of_result a, linearconstraints_of_result b) in
      RLinearConstraints (_and pos1 pos2,
                          List.flatten [_and pos1 neg2;
                                        _and neg1 pos2;
                                        _and neg2 neg2]));
   ("or",
    fun a b ->
      let ((pos1, neg1), (pos2, neg2)) = (linearconstraints_of_result a, linearconstraints_of_result b) in
      RLinearConstraints (List.flatten [_and pos1 pos2;
                                        _and pos1 neg2;
                                        _and neg1 pos2],
                          _and neg1 neg2))]

;;

let linear_numeric_arithmetic = 
  [("+", fun le1 le2 -> let (le1, le2) = (linear_of_result le1, linear_of_result le2) in 
                        RLinear (Plus (le1, le2)));
   ("-", fun le1 le2 -> let (le1, le2) = (linear_of_result le1, linear_of_result le2) in 
                        RLinear (Minus (le1, le2)));
   ("*", fun le1 le2 -> let (le1, le2) = (linear_of_result le1, linear_of_result le2) in 
                        RLinear (
                          match (le1, le2) with
                            | (le1, Coefficient c2) 
                            | (Coefficient c2, le1) -> Times (c2, le1)
                            | _ -> raise (Non_linear (sprintf "non-linearity encountered: %s * %s"
                                                        (Pplutil.string_of_linear_expression le1)
                                                        (Pplutil.string_of_linear_expression le2)))));
   ("/", fun le1 le2 -> let (le1, le2) = (linear_of_result le1, linear_of_result le2) in 
                        raise (Non_linear (sprintf "non-linearity encountered: %s / %s"
                                             (Pplutil.string_of_linear_expression le1)
                                             (Pplutil.string_of_linear_expression le2))))]
;;

let _pone le = Plus (le, Coefficient Gmputil.zone);;
let _mone le = Minus (le, Coefficient Gmputil.zone);;

let linear_numeric_relations =
  [("==", 
    fun le1 le2 -> let (le1, le2) = (linear_of_result le1, linear_of_result le2) in 
                   RLinearConstraints ([[Equal (le1, le2)]],
                                       [[Less_Or_Equal (_pone le1, le2)];
                                        [Greater_Or_Equal (_mone le1, le2)]]));
   ("!=",
    fun le1 le2 -> let (le1, le2) = (linear_of_result le1, linear_of_result le2) in 
                   RLinearConstraints ([[Less_Or_Equal (_pone le1, le2)];
                                        [Greater_Or_Equal (_mone le1, le2)]],
                                       [[Equal (le1, le2)]]));
   ("<=",
    fun le1 le2 -> let (le1, le2) = (linear_of_result le1, linear_of_result le2) in 
                   RLinearConstraints ([[Less_Or_Equal (le1, le2)]],
                                       [[Greater_Or_Equal (_mone le1, le2)]]));
   ("<", 
    fun le1 le2 -> let (le1, le2) = (linear_of_result le1, linear_of_result le2) in 
                   RLinearConstraints ([[Less_Or_Equal (_pone le1, le2)]],
                                       [[Greater_Or_Equal (le1, le2)]]));
   (">=", 
    fun le1 le2 -> let (le1, le2) = (linear_of_result le1, linear_of_result le2) in 
                   RLinearConstraints ([[Greater_Or_Equal (le1, le2)]],
                                       [[Less_Or_Equal (_pone le1, le2)]]));
   (">",
    fun le1 le2 -> let (le1, le2) = (linear_of_result le1, linear_of_result le2) in 
                   RLinearConstraints ([[Greater_Or_Equal (_mone le1, le2)]],
                                       [[Less_Or_Equal (le1, le2)]]));
  ];;

let linear_imp_hash =
  let temp_hash = Hashtbl.create
    ((List.length linear_logical_relations) +
        (List.length linear_numeric_arithmetic) +
        (List.length linear_numeric_relations)
    ) in
  List.iter (fun (oname, oimp) -> Hashtbl.replace temp_hash oname oimp) linear_logical_relations;
  List.iter (fun (oname, oimp) -> Hashtbl.replace temp_hash oname oimp) linear_numeric_arithmetic;
  List.iter (fun (oname, oimp) -> Hashtbl.replace temp_hash oname oimp) linear_numeric_relations;
  temp_hash
;;

let _constraint_true = Equal (Coefficient (Z.of_int 0), Coefficient (Z.of_int 0));;
let _constraint_false = Equal (Coefficient (Z.of_int 0), Coefficient (Z.of_int 1));;

let promote x = 
  match x with
    | VInt v -> RLinear (Coefficient (Z.of_int v))
    | VAbsRef i -> RLinear (Variable i)
    | VBool true -> RLinearConstraints ([[_constraint_true]],
                                        [[_constraint_false]])
    | VBool false -> RLinearConstraints ([[_constraint_false]],
                                         [[_constraint_true]])
    | _ -> failwith (sprintf "don't know how to handly this kind of value: %s" (Lang.string_of_value x))
;;

let rec eval_binop opname a b =
  match (a, b) with
    | (RConcrete a, RConcrete b) -> begin
      match (a, b) with
        | (VInt _, VInt _) 
        | (VBool _, VBool _)
        | (VReal _, VReal _) -> RConcrete (Library.eval_binop opname a b)
        | (VAbsRef _, VInt _)
        | (VInt _, VAbsRef _)
        | (VAbsRef _, VAbsRef _) -> (Hashtbl.find linear_imp_hash opname) (promote a) (promote b)
        | _ -> failwith (sprintf "do not know how to evalute this abstractly: %s %s %s" (Lang.string_of_value a) opname (Lang.string_of_value b)) end
(*   
    | (RLinear _, RConcrete b) -> eval_binop opname a (promote b)
    | (RConcrete a, RLinear _) -> eval_binop opname (promote a) b
    | (RLinear _, RLinear _) -> (Hashtbl.find linear_imp_hash opname) a b
    | (RLinearConstraints _, RConcrete b) -> eval_binop opname a (promote b)
    | (RConcrete a, RLinearConstraints _) -> eval_binop opname (promote a) b
    | (RLinearConstraints _, RLinearConstraints _) ->
      (Hashtbl.find linear_imp_hash opname) a b*)
    | _ -> failwith (sprintf "cannot handle binary operation %s on these inputs" opname)
;;
  
