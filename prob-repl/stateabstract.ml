open ExtHashtbl
open ExtList
open ExtString
open Printf
open Ppl_ocaml
open Util
open Librarylinear
open Gmp
open Gmp.Q.Infixes
open Gmp.Z.Infixes
open Gmputil

type ident = Lang.valueident

type value = Lang.value
let value_null = Lang.VUnit

type heap_type = value DynArray.t

type coord = float array
type face = coord list
type shape = float * float * (face list)

class heap initheap = object (self)
  val mutable heap_data: heap_type = initheap

  method print = print_string self#to_string

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

module type ABSTRACT_STATE_TYPE = sig
  type region
  type substate

  class type absstate = object
    method call: unit
    method return: Lang.value -> unit
    method getreturnk: 'a. (Lang.value * absstate, 'a) splitcontinue

    method get_substates: substate list
    method alloc: ident -> unit

    method set_concrete: ident -> value -> unit
    method setk: 'a. ident -> result -> (value * absstate, 'a) splitcontinue
    method view_as_concretek: 'a. result -> (value * absstate, 'a) splitcontinue

    method get: ident -> (value * absstate) list
    method getk: 'a. ident -> (value * absstate, 'a) splitcontinue
    method to_string: string
    method print: unit
    method copy: absstate

    method iter_substates: (substate -> unit) -> unit
    method map_substates: 'a. (substate -> 'a) -> 'a list

    method heap_deref: int -> (value * absstate) list
    method heap_derefk: 'a. int -> (value * absstate, 'a) splitcontinue

    method heap_allock: 'a. value -> (int * absstate, 'a) splitcontinue
    method heap_set: int -> value -> unit

    method abs_set: int -> linear_expression -> unit
    method abs_allock: 'a. (int * absstate, 'a) splitcontinue
    method abs_add_constraint: linear_constraint -> unit
    method abs_add_constraints: linear_constraint list -> unit

    method remove_impossible: unit

    method dump_view: Lang.exp array -> shape list

    method splitk: 'a. int -> (int * absstate, 'a) splitcontinue
    method splitrangek: 'a. int -> int -> int -> (int * int * absstate, 'a) splitcontinue

    method prob_scale: Q.t -> Q.t -> unit
    method prob_mult_points: Z.t -> unit
    (*method prob_setpoints: Z.t -> unit*)
    method prob_fixtotals: unit
  end;;

  class join_many: absstate list -> absstate
  class absstate_empty: absstate
end

type estimator =
    {pmin: Q.t;
     pmax: Q.t;
     smin: Z.t;
     smax: Z.t;
     mmin: Q.t;
     mmax: Q.t};;

let estimator_empty = {
  pmin = qone;
  pmax = qone;
  smin = zone;
  smax = zone;
  mmin = qone;
  mmax = qone
}

let estimator_prob_scale est p1 p2 = {
  pmin = est.pmin */ p1;
  pmax = est.pmax */ p1;
  smin = est.smin;
  smax = est.smax;
  mmin = est.mmin */ p2;
  mmax = est.mmax */ p2
}

let string_of_estimator e =
  sprintf "p = [%s,%s], m = [%s,%s], s = [%s,%s]"
    (Q.to_string e.pmin)
    (Q.to_string e.pmax)
    (Q.to_string e.mmin)
    (Q.to_string e.mmax)
    (Z.to_string e.smin)
    (Z.to_string e.smax)
;;

module ABSTRACT_STATE_MAKER
  (PPL: Ppldomain.PPLDOMAIN_TYPE)
  : ABSTRACT_STATE_TYPE with type region = PPL.region = struct
    type region = PPL.region
    type memory = heap
    type bindings = (ident, value) Hashtbl.t
    type state = Stateconcrete.state
    type substate = state * region * estimator

    let substate_copy ((astate, aregion, aest): substate) =
      (astate#copy,
       PPL.copy_region aregion,
       aest)

    class absstate (initsubstates: substate list) = object (self)
      val mutable substates: substate list = initsubstates

      method get_substates = substates

      method iter_substates (f: substate -> unit): unit = List.iter f substates
      method map_substates: 'a. (substate -> 'a) -> 'a list = fun f -> List.map f substates

      method call =
        self#iter_substates (fun (s, _, _) -> s#call)

      method getreturnk : 'a. (Lang.value * absstate, 'a) splitcontinue = fun k ->
        List.flatten (self#map_substates (fun (s, r, e) -> k (s#getreturn, new absstate [(s,r,e)])))
          
      method return ret =
        self#iter_substates (fun (s, _, _) -> s#return ret)

      method alloc varname =
	self#iter_substates (fun (s, _, _) -> s#alloc varname)

      method get varname =
        self#map_substates (fun (s, a, e) ->
	  let temp = try s#get varname
	    with Not_found -> failwith ("undefined variable [" ^ (Lang.string_of_ident varname) ^ "]") in
          (temp, new absstate [(s,a,e)]))

      method getk : 'a. Lang.valueident -> (Lang.value * absstate, 'a) splitcontinue = fun varname k ->
        List.flatten (List.map k (self#get varname))
	  
      method copy = new absstate (self#map_substates substate_copy)

      method heap_deref i = 
        self#map_substates (fun (s,a,e) -> (s#heap_deref i, new absstate [(s,a,e)]))

      method heap_derefk : 'a. int -> (Lang.value * absstate, 'a) splitcontinue = fun i k ->
        List.flatten (List.map k (self#heap_deref i))

      method heap_set i v = self#iter_substates (fun (s,_,_) -> s#heap_set i v)

      method heap_allock : 'a . value -> (int * absstate, 'a) splitcontinue = fun v k ->
        List.flatten (self#map_substates
                        (fun (s,r,e) ->
                          let astate = new absstate [(s,r,e)] in
                          k (s#heap_alloc v, astate)))
        
      method to_string =
        (sprintf "absstate with %d substates" (List.length substates)) ^
          (String.concat (sprintf "\n--- 1 of %d\n" (List.length substates))
             (List.map (fun (s, r, e) ->
               "prob: " ^ (string_of_estimator e) ^ "\n" ^
               "state:\n" ^ (Util.string_indent "  " (s#to_string)) ^ "\n" ^
                 "region:\n" ^ (Util.string_indent "  " (PPL.string_of_region r)) ^ "\n")
                substates))

      method print = print_string self#to_string
        
      method abs_set i le =
        self#iter_substates (fun (s, r,e) -> PPL.affine_image r i le)

      method abs_allock : 'a. (int * absstate, 'a) splitcontinue = fun k ->
        List.flatten
          (self#map_substates
             (fun (s,r,e) ->
               PPL.add_dimensions r 1; 
               k ((PPL.get_dimensions r) - 1, new absstate [(s,r,e)])))

      method set_concrete: Lang.valueident -> Lang.value -> unit = fun varname varval ->
        self#iter_substates (fun (s, _, _) -> s#set varname varval)

      method setk: 'a. Lang.valueident -> result -> ((Lang.value * absstate), 'a) splitcontinue =
        fun varname rvarval k -> match rvarval with
          | RConcrete varval ->
            self#iter_substates (fun (s, _, _) -> s#set varname varval);
            k (varval, new absstate self#get_substates)
          | RLinear le ->
            self#abs_allock (fun (i, astate) ->
              astate#iter_substates
                (fun (s, r, _) -> s#set varname (Lang.VAbsRef i));
              astate#abs_set i le;
              k (Lang.VAbsRef i, astate))
          | RLinearConstraints (pos, neg) ->
            let spos = new join_many (List.flatten (self#map_substates (fun (s,r,e) ->
              List.map (fun newconst ->
                let temp = new absstate [(s#copy,PPL.copy_region r,e)] in
                temp#abs_add_constraints newconst; temp) pos))) in
            let sneg = new join_many (List.flatten (self#map_substates (fun (s,r,e) ->
              List.map (fun newconst ->
                let temp = new absstate [(s#copy,PPL.copy_region r,e)] in
                temp#abs_add_constraints newconst; temp) neg)))
            in
            spos#set_concrete varname (Lang.VBool true);
            sneg#set_concrete varname (Lang.VBool false);
            List.append
              (k (Lang.VBool true, spos))
              (k (Lang.VBool false, sneg))

      method remove_impossible =
        substates <- List.filter (fun (s,r,e) -> PPL.region_is_nonempty r) substates

      method prob_fixtotals =
        substates <- self#map_substates (fun (s,r,e) ->
          let size = PPL.region_size r in
          let newsmin = min e.smin size in
          let newsmax = min e.smax size in
          let newmmin = max e.mmin (e.pmin */ (Q.from_z newsmin)) in
          let newmmax = min e.mmax (e.pmax */ (Q.from_z newsmax)) in
          (s,r,{e with
            smin = newsmin;
            smax = newsmax;
            mmin = newmmin;
            mmax = newmmax}))

      method abs_add_constraint (lec: linear_constraint): unit =
        substates <- self#map_substates (fun (s, r, e) -> (s, PPL.of_constraints_poly r [lec], e));
        self#remove_impossible;
        self#prob_fixtotals

      method abs_add_constraints (lec: linear_constraint list): unit =
        substates <- self#map_substates (fun (s, r, e) -> (s, PPL.of_constraints_poly r lec, e));
        self#remove_impossible;
        self#prob_fixtotals

      method view_as_concretek: 'a. result -> (Lang.value * absstate, 'a) splitcontinue = fun r k -> match r with
        | RConcrete v -> k (v, new absstate self#get_substates)
        | RLinear le ->
          self#abs_allock (fun (i,astate) ->
            astate#abs_set i le;
            k (Lang.VAbsRef i, astate))
        | RLinearConstraints (pos, neg) ->
          let spos = new join_many (List.flatten (self#map_substates (fun (s,r,e) ->
            List.map (fun newconst ->
              let temp = new absstate [(s#copy,PPL.copy_region r,e)] in
              temp#abs_add_constraints newconst; temp) pos))) in
          let sneg = new join_many (List.flatten (self#map_substates (fun (s,r,e) ->
            List.map (fun newconst ->
              let temp = new absstate [(s#copy,PPL.copy_region r,e)] in
              temp#abs_add_constraints newconst; temp) neg))) in
          List.append
            (k (Lang.VBool true, spos))
            (k (Lang.VBool false, sneg))

      method dump_view: Lang.exp array -> shape list = 
        self#remove_impossible;
        let _dump_substate_view aview (s,r,e) =
          let bounds = PPL.region_bounds r in
          let b = Array.map
            (fun aexp -> try match s#eval_lvalue aexp with
              | Lang.VDeclared _ -> (0.0, 1.0)
              | Lang.VInt i -> (float_of_int i, 1.0 +. (float_of_int i)) 
              | Lang.VAbsRef i -> (float_of_int (fst bounds.(i)), 1.0 +. (float_of_int (snd bounds.(i))))
              | _ -> failwith "don't know how to visualize things other than integers"
              with
                | Eval.Undefined_variable _ 
                | Not_found -> (0.0,1.0)) aview in
          let (c0,c1,c2,c3,c4,c5,c6,c7) =
            ([|fst b.(0); fst b.(1); fst b.(2)|],
             [|snd b.(0); fst b.(1); fst b.(2)|],
             [|snd b.(0); snd b.(1); fst b.(2)|],
             [|fst b.(0); snd b.(1); fst b.(2)|],
             [|fst b.(0); fst b.(1); snd b.(2)|],
             [|snd b.(0); fst b.(1); snd b.(2)|],
             [|snd b.(0); snd b.(1); snd b.(2)|],
             [|fst b.(0); snd b.(1); snd b.(2)|]) in
          (Q.to_float e.pmax, 
           Q.to_float e.mmin,
           [[c0;c3;c2;c1];
            [c4;c5;c6;c7];
            [c0;c4;c7;c3];
            [c1;c2;c6;c5];
            [c0;c1;c5;c4];
            [c3;c7;c6;c2]])
        in fun aview -> self#map_substates (_dump_substate_view aview)

      method splitk : 'a . int -> (int * absstate, 'a) splitcontinue = fun into k ->
        List.flatten (List.map (fun i -> k (i, self#copy)) (Util.list_range 0 into))

      method splitrangek : 'a . int -> int -> int -> (int * int * absstate, 'a) splitcontinue =
        fun lower upper factor k ->
          let pieces = Util.make_split lower upper factor in
          List.flatten (List.map (fun (l,u) -> k (l, u, self#copy)) pieces)

      method prob_scale p1 p2 =
        substates <- self#map_substates (fun (s,r,e) ->
          (s,r, estimator_prob_scale e p1 p2))

      method prob_mult_points m =
        substates <- self#map_substates (fun (s,r,e) ->
          (s,r, {e with smin = e.smin *! m; smax = e.smax *! m}))

    end and join_many (sl: absstate list) = object
      inherit absstate (List.fold_left (fun a s -> List.append a (s#get_substates)) [] sl)

    end and absstate_empty = object
      inherit absstate [(new Stateconcrete.state_empty, PPL.make_new 0, estimator_empty)]
    end
  end

(*
module type ABSTRACT_STATE_TYPE_MONADIC = sig
  type absstate
  type substate 

  type 'a bind = absstate -> ('a -> absstate) -> absstate
  type 'a subbind = absstate -> ('a -> substate -> absstate) -> absstate
  type 'a absbind = absstate -> ('a -> absstate -> absstate) -> absstate
  type 'a return = 'a -> astate
  type 'a val_return = 'a -> Lang.value

  method val_return: Lang.value val_return
  method return: Lang.value return

  method bind: 'a. 'a absbind

  method bind_call: unit bind

  method bind_return: Lang.value absbind
  method bind_alloc: ident -> unit bind

  method bind_get: ident -> Lang.value absbind

  method bind_set_concrete: ident -> value -> unit absbind

  method bind_view: Lang.value -> Lang.value absbind
end

module ABSTRACT_STATE_MAKER_MONADIC
  (PPL: Ppldomain.PPLDOMAIN_TYPE)
  : ABSTRACT_STATE_TYPE_MONADIC with type region = PPL.region = struct
    type region = PPL.region
    type memory = heap
    type state = Stateconcrete.state

    type substate = {state: state;
                     region: region;
                     estimator: estimator;
                     memory: memory}

    type absstate = {substates: substate list}

    let _substate_new () = 
      {state = new Stateconcrete.state_empty ();
       region = PPL.make_new 0, estimator_empty}

    let _absstate_new () =
      {substates = [_substate_new ()]}

    let val_return v = v

    let bind astate f = f astate
      
    let bind_call astate f =
      
      
  end
;;
*)
