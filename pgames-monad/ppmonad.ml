open ExtList
open Printf
open Option
open Glpk
open Util

type prob = float
let is_one p = abs_float(p -. 0.1) <= 0.0000001
let is_zero p = abs_float(p) <= 0.0000001
    
module NATURE = struct
  type 'a dist = ('a, prob) PMap.t

  let union d1 d2 = Util.maps_merge d1 d2 (+.)

  let prob_of v d =
    try PMap.find v d
    with Not_found -> 0.0

  let scale p d = PMap.map (fun p2 -> p *. p2) d

  let iter d f = PMap.iter f d

  let bind_choice k = failwith "nature does not have a choice"

  let return v = PMap.add v 1.0 PMap.empty

  let to_string f (d : 'a dist) = PMap.foldi
    (fun v p acc -> acc ^ "\n" ^
      (sprintf "Pr[%s] = %0.3f" (f v) p)) d ""

  let of_list (choices: ('a * prob) list): 'a dist =
    List.fold_left
      (fun a (v, p) -> Util.map_insert_with (+.) v p a)
      PMap.empty choices

  let bind d k =
    PMap.foldi (fun v p a ->
      Util.maps_merge a (scale p (k v)) (+.))
      d PMap.empty

  let bind_flip prob k =
    if is_one prob then bind (of_list [(true,1.0)]) k
    else if is_zero prob then bind (of_list [(false,1.0)]) k
    else
      bind (of_list [(true, prob);
                     (false, 1.0 -. prob)]) k

  let map f d = PMap.map f d;;

  let expect d =
    PMap.foldi (fun x sp acc -> (acc +. (sp *. x))) d 0.0

  let expect_of d f =
    PMap.foldi (fun x sp acc -> (acc +. (sp *. (f x)))) d 0.0

end;;

type choice_counter = (string, int) PMap.t

module TWO_PLAYERS = struct
  type ('a,'b) choice =
      (choice_counter * choice_counter) *
      ((('a list) * ('a list), 'b NATURE.dist) PMap.t)

  type player = {app : 'a. ('a -> 'a) -> ('a * 'a) -> ('a * 'a)}

  let player1 : player = {app = fun f (a,b) -> (f a, b)}
  let player2 : player = {app = fun f (a,b) -> (a, f b)}

  let _dump_list_to_string cp = 
    String.concat "," (List.map Std.dump cp)

  let _count_to_string m =
    String.concat "," (List.map
                       (fun (n,c) -> sprintf "%s(%d)" (Std.dump n) c)
                       (List.of_enum (PMap.enum m)))
                       
  let to_string f (((cnt1, cnt2), amap) : ('a,'b) choice) = 
    PMap.foldi (fun (cp1, cp2) n acc ->
      acc ^ "\n" ^ "player1 choices: [" ^
        (_dump_list_to_string cp1) ^ "], player2 choices: [" ^
        (_dump_list_to_string cp2) ^ "]\n" ^ (Util.string_indent "  " (NATURE.to_string f n)) ^ "")
      amap (sprintf "TWO_PLAYERS (counts p1 = %s, counts p2 = %s): "
              (_count_to_string cnt1) (_count_to_string cnt2))

  (* let _counts_maxes (a1, a2) (b1, b2) = (max a1 b1, max a2 b2)
     let _counts_sums (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)
     let _counts_mults (a1, a2) (b1, b2) = (a1 * b1, a2 * b2) *)

  let _new_name =
    let temp = ref 0 in
    fun () ->
      temp := !temp + 1;
      !temp

  let _counts_new = (PMap.empty, PMap.empty)
  let _count_add cnt_name cnt_count cnt =

    Util.map_insert_with (+) cnt_name cnt_count cnt
  let _count_merge a b = Util.maps_merge a b (fun c1 c2 -> max c1 c2)
  let _counts_merge (a1, a2) (b1, b2) = (_count_merge a1 b1, _count_merge a2 b2)
  let _count_count a =
    PMap.foldi (fun _ c a -> a + c) a 0

  let union (cnt1, map1) (cnt2, map2) =
    (_counts_merge cnt1 cnt2,
     Util.maps_merge map1 map2 NATURE.union)

  let rec union_list sl = match sl with
    | [] -> failwith "cannot union an empty list"
    | a :: [] -> a
    | a :: b :: r -> union_list ((union a b) :: r)

  let mapn f (cnt, amap) =
    (cnt, PMap.mapi (fun (s1, s2) nat -> (f nat)) amap)

  let mapi f (cnt, amap) = (cnt, PMap.mapi f amap)

  let mapc f (cnt, amap) =
    (cnt,
     PMap.foldi
       (fun choices nature acc ->
         PMap.add (f choices) nature acc) amap PMap.empty)

  let _addchoice
      (who: player) (c:'a)
      (opt_name: string) (opt_count: int)
      (cnts, amap) =
    let (_, amap) = mapc
      (who.app (fun a -> c :: a))
      (cnts, amap) in
    (who.app (_count_add opt_name opt_count) cnts, amap)

  let bind ((cnts_outer, outer) : ('a,'b) choice)
      (k : 'b -> ('a,'c) choice) : ('a,'c) choice =
      union_list begin
        List.map begin
        fun ((s1_outer, s2_outer), nat_outer) ->
          let (cnts_inner, inner) =
            union_list (List.map
                          (fun (v,p) -> mapn (NATURE.scale p) (k v))
                          (List.of_enum (PMap.enum nat_outer))) in
          let cnts = _counts_merge cnts_outer cnts_inner in
          union_list begin
            List.map (fun ((s1_inner, s2_inner), nat_inner) ->
              (cnts,
               PMap.add
                 (List.append s1_inner s1_outer,
                  List.append s2_inner s2_outer)
                 nat_inner
                 PMap.empty))
              (List.of_enum (PMap.enum inner))
          end
      end (List.of_enum (PMap.enum outer))
      end

  let rec bind_nature (clist : ('b * float) list) k =
    let nats = List.map (fun (c, p) -> mapn (fun n -> NATURE.scale p n) (k c)) clist in
    union_list nats

  let rec bind_choice (who: player) (opt_name: string) (clist: 'a list) k =
    let opt_count = List.length clist in
    let pieces = List.map (fun c -> _addchoice who c opt_name opt_count (k c)) clist in
    union_list pieces

  let return v =
    (_counts_new,
     PMap.add
       ([], [])
       (NATURE.return v)
       PMap.empty)

  let prob_of cll v (cnt, amap) = match cll with
    | p1l :: p2l :: [] -> begin
      try NATURE.prob_of v (PMap.find (p1l, p2l) amap)
      with Not_found -> 0.0
    end
    | _ -> failwith "expected two player's worth of choices"

  let _hindex h i a =
    try Hashtbl.find h a with
      | Not_found ->
        let temp = !i in
        i := temp + 1;
        Hashtbl.replace h a temp;
        temp

  let nash_zerosum_select (((m,n),amap)) sel =
    (* http://ocaml-glpk.sourceforge.net/ocamldoc/Glpk.html *)
    (* http://en.wikipedia.org/wiki/Zero-sum_game#Solving *)
    let m = _count_count m in
    let n = _count_count n in
    let direction = Minimize in
    let zcoeffs = Array.make m 1.0 in
    let xbounds = Array.make m (0.0, infinity) in
    let pbounds = Array.make n (1.0, infinity) in
    let constrs = Array.make_matrix m n 0.0 in
    let hindexm = Hashtbl.create m in
    let hindexm_reverse = Hashtbl.create m in
    let indexm = ref 0 in
    let hindexn = Hashtbl.create n in
    let indexn = ref 0 in
    let slist = List.of_enum (PMap.enum amap) in
    List.iter (fun ((s1, s2), nature) ->
      let idxm = _hindex hindexm indexm s1 in
      Hashtbl.replace hindexm_reverse idxm s1;
      let idxn = _hindex hindexn indexn s2 in
      let e = NATURE.expect_of nature sel in
      constrs.(idxm).(idxn) <- e)
      slist;
    let lp = make_problem direction zcoeffs constrs pbounds xbounds in
    write_cplex lp "temp.cplex";
    set_message_level lp 3;
    scale_problem lp;
    use_presolver lp true;
    simplex lp;
    let prim = get_col_primals lp in
    let objective = (get_obj_val lp) in
    let value = 1.0 /. objective in
    let probs = Array.mapi (fun i x ->
      let s1 = Hashtbl.find hindexm_reverse i in
      (s1, x *. value)) prim in
    (value, probs)

  let nash_zerosum (((m,n),amap) : ('a, float) choice) =
    (* http://ocaml-glpk.sourceforge.net/ocamldoc/Glpk.html *)
    (* http://en.wikipedia.org/wiki/Zero-sum_game#Solving *)
    let m = _count_count m in
    let n = _count_count n in
    let direction = Minimize in
    let zcoeffs = Array.make m 1.0 in
    let xbounds = Array.make m (0.0, infinity) in
    let pbounds = Array.make n (1.0, infinity) in
    let constrs = Array.make_matrix m n 0.0 in
    let hindexm = Hashtbl.create m in
    let hindexm_reverse = Hashtbl.create m in
    let indexm = ref 0 in
    let hindexn = Hashtbl.create n in
    let indexn = ref 0 in
    let slist = List.of_enum (PMap.enum amap) in
    List.iter (fun ((s1, s2), nature) ->
      let idxm = _hindex hindexm indexm s1 in
      Hashtbl.replace hindexm_reverse idxm s1;
      let idxn = _hindex hindexn indexn s2 in
      let e = NATURE.expect nature in
      constrs.(idxm).(idxn) <- e)
      slist;
    let lp = make_problem direction zcoeffs constrs pbounds xbounds in
    write_cplex lp "temp.cplex";
    set_message_level lp 3;
    scale_problem lp;
    use_presolver lp true;
    simplex lp;
    let prim = get_col_primals lp in
    let objective = (get_obj_val lp) in
    let value = 1.0 /. objective in
    let probs = Array.mapi (fun i x -> 
      let s1 = Hashtbl.find hindexm_reverse i in
      (s1, x *. value)) prim in
    (value, probs)

end
