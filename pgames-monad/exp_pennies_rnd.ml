open Printf
open ExtList
open ExtArray
open Glpk

module G = Ppmonad.TWO_PLAYERS 

(* matching pennies with some added randomness *)

let xor a b = (a && not b) || ((not a) && b)
let () =
  let dist_payoffs = G.bind_choice G.player1 "p1" [true; false]
    (fun penny1 -> 
      G.bind_choice G.player2 "p2" [true; false]
        (fun penny2 -> 
          let res = xor penny1 penny2 in
          if res then
            G.bind_nature [(true, 0.25); (false, 0.75)]
              (fun c -> G.return (if c then 1.0 else 0.0))
          else
            G.bind_nature [(true, 0.75); (false, 0.25)]
              (fun c -> G.return (if c then 1.0 else 0.0)))) in
 
  printf "%s\n" (G.to_string string_of_float dist_payoffs);

  let (value, probs) = G.nash_zerosum dist_payoffs in
  Printf.printf "game value = %g\n" value;
  Printf.printf "player1 strategy:\n";
  Array.iteri (fun i (v,p) ->
    Printf.printf "  Pr[%s] = %f\n" (Std.dump v) p
  ) probs

