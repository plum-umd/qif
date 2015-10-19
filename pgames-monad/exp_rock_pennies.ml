open Printf
open ExtList
open ExtArray
open Glpk

module G = Ppmonad.TWO_PLAYERS 

(* one of two games, decided randomly to start with *)

let rock_winner a b = match (a,b) with
  | (a,b) when a = b -> 1.0
  | ("scissors", "paper") -> 2.0
  | ("rock",     "scissors") -> 2.0
  | ("paper",    "rock") -> 2.0
  | _ -> (0.0)
;;
let xor a b = match (a,b) with
  | ("heads", "heads") -> false
  | ("tails", "tails") -> false
  | _ -> true

let () = 
  let play_either = G.bind_nature [("rock", 0.5);
                                   ("pennies", 0.5)]
    (fun game ->
      if game = "rock" then
        G.bind_choice G.player1 "p1rock" ["rock"; "paper"; "scissors"]
          (fun p1choice -> 
            G.bind_choice G.player2 "p2rock" ["rock"; "paper"; "scissors"]
              (fun p2choice -> 
                let res = rock_winner p1choice p2choice in
                G.return ("played_rock", res)))
      else 
        G.bind_choice G.player1 "p1penny" ["heads"; "tails"]
          (fun penny1 -> 
            G.bind_choice G.player2 "p2penny" ["heads"; "tails"]
              (fun penny2 -> 
                let res = xor penny1 penny2 in
                G.return ("played_pennies", (if res then 1.0 else 0.0))))) in
 
  printf "%s\n" (G.to_string Std.dump play_either);

  let (value, probs) = G.nash_zerosum_select play_either snd in
  Printf.printf "game value = %g\n" value;
  Printf.printf "player1 strategy:\n";
  Array.iteri (fun i (v,p) ->
    Printf.printf "  Pr[%s] = %f\n" (Std.dump v) p
  ) probs
