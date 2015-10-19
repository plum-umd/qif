open Printf
open ExtList
open ExtArray
open Glpk

module G = Ppmonad.TWO_PLAYERS 

(* guess my number 2; note that the linear programming solver does not
   work here, and the construction is incomplete as observation is not
   yet supported *)
  
type restrict =
  | LT5
  | Even

let numbers = [1;2;3;4;5;6;7;8;9;10]
let lt5 = [1;2;3;4;5]
let even = [2;4;6;8;10]
  
let () =
  let dist_payoffs =
    (G.bind_nature [(LT5, 0.4); (Even, 0.6)]
       (fun r -> G.bind_choice G.player1 "pick" (if r == LT5 then lt5 else even)
         (fun pick -> 
           G.bind_choice G.player2 "guess" numbers
             (fun guess -> 
               let res = pick == guess in
               G.return (if res then 0.0 else 1.0))))) in

  printf "%s\n" (G.to_string string_of_float dist_payoffs);

  let (value, probs) = G.nash_zerosum dist_payoffs in
  Printf.printf "game value = %g\n" value;
  Printf.printf "player1 strategy:\n";
  Array.iteri (fun i (v,p) ->
    Printf.printf "  Pr[%s] = %f\n" (Std.dump v) p
  ) probs
