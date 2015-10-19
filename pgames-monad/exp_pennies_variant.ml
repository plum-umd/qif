open Printf
open ExtList
open ExtArray
open Glpk

module G = Ppmonad.TWO_PLAYERS 

(* matching pennies with different payoffs *)

let xor a b = (a && not b) || ((not a) && b)

let () =
  let dist_payoffs = G.bind_choice G.player1 "choice1" [true; false]
    (fun penny1 -> 
      G.bind_choice G.player2 "choice2" [true; false]
        (fun penny2 -> 
          let res = xor penny1 penny2 in
          G.return (if penny1 then
              begin if res then (2.0) else 3.0 end
            else
              begin if res then (1.0) else 3.0 end))) in

  printf "%s\n" (G.to_string string_of_float dist_payoffs);

  let (value, probs) = G.nash_zerosum dist_payoffs in
  Printf.printf "game value = %g\n" value;
  Printf.printf "player1 strategy:\n";
  Array.iteri (fun i (v,p) ->
    Printf.printf "  Pr[%s] = %f\n" (Std.dump v) p
  ) probs


