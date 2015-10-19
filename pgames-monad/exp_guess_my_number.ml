open Printf
open ExtList
open ExtArray
open Glpk

module G = Ppmonad.TWO_PLAYERS 

(* guess my number *)
let numbers = [1;2;3;4;5;6;7;8;9;10]
  
let () =
  let dist_payoffs = G.bind_choice G.player1 "pick" numbers
    (fun pick -> 
      G.bind_choice G.player2 "guess" numbers
        (fun guess -> 
          let res = pick == guess in
          G.return (if res then 0.0 else 1.0))) in

  printf "%s\n" (G.to_string string_of_float dist_payoffs);

  let (value, probs) = G.nash_zerosum dist_payoffs in
  Printf.printf "game value = %g\n" value;
  Printf.printf "player1 strategy:\n";
  Array.iteri (fun i (v,p) ->
    Printf.printf "  Pr[%s] = %f\n" (Std.dump v) p
  ) probs
