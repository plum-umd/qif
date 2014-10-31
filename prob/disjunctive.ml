open State
open Printf
open Util
open Stateset
open Pstateset

module type DISJUNCTIVE_TYPE = sig
  module SS: STATESET_TYPE
  module PSS: PSTATESET_TYPE

  type stateset
  type pstateset

  type t = (state * pstateset)
end

module MakeDisjunctive
  (SSM: STATESET_TYPE)
  (PSSM: PSTATESET_TYPE with type stateset = SSM.stateset
  )
  : (DISJUNCTIVE_TYPE with type stateset = SSM.stateset
		      and type pstateset = PSSM.pstateset
    ) =
struct
  module SS = SSM
  module PSS = PSSM

  type stateset = SS.stateset
  type pstateset = PSS.pstateset

  type t = (state * pstateset)

  let add_states_for_belief dp (stl: state list) (b: pstateset) =
    List.append dp
      (List.map (fun s -> (s#copy, PSS.copy b)) stl)


end
