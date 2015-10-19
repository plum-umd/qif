open Printf
open ExtList
open ExtArray
open Glpk

module G = Ppmonad.TWO_PLAYERS 

(* price competition games:
   http://faculty.lebow.drexel.edu/McCainR/top/eco/game/zerosum.html
*)

let () =
  let hpayoffs = Hashtbl.create 16 in
  List.iter
    (fun (c1, c2, p1) -> Hashtbl.replace hpayoffs (c1, c2) p1)
    [(true, true, 0.5);
     (true, false, 0.0);
     (false, true, 1.0);
     (false, false, 0.5)];

  let dist_payoffs = G.bind_choice G.player1 "choice1" [true; false]
    (fun p1ratsout -> 
      G.bind_choice G.player2 "choice2" [true; false]
        (fun p2ratsout -> 
          G.return (Hashtbl.find hpayoffs (p1ratsout, p2ratsout)))) in

  printf "%s\n" (G.to_string string_of_float dist_payoffs);

  let (value, probs) = G.nash_zerosum dist_payoffs in
  Printf.printf "game value = %g\n" value;
  Printf.printf "player1 strategy:\n";
  Array.iteri (fun i (v,p) ->
    Printf.printf "  Pr[%s] = %f\n" (Std.dump v) p
  ) probs
