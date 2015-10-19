open Printf
open ExtList
open ExtArray
open Glpk

module G = Ppmonad.TWO_PLAYERS 

(* rock/paper/scissors *)
  
let winner a b = match (a,b) with
  | (a,b) when a = b -> 1.0
  | ("scissors", "paper") -> 2.0
  | ("rock", "scissors") -> 2.0
  | ("paper", "rock") -> 2.0
  | _ -> (0.0)
;;

let () =
  let dist_payoffs = G.bind_choice G.player1 "p1" ["rock"; "paper"; "scissors"]
    (fun p1choice -> 
      G.bind_choice G.player2 "p2" ["rock"; "paper"; "scissors"]
        (fun p2choice -> 
          let res = winner p1choice p2choice in
          G.return res)) in

  printf "%s\n" (G.to_string string_of_float dist_payoffs);

  let (value, probs) = G.nash_zerosum dist_payoffs in
  Printf.printf "game value = %g\n" value;
  Printf.printf "player1 strategy:\n";
  Array.iteri (fun i (v,p) ->
    Printf.printf "  Pr[%s] = %f\n" (Std.dump v) p
  ) probs
