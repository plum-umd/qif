open Lang
open State
open Pstateset

module type EVALPSTATESET_TYPE = sig
  module PSS: PSTATESET_TYPE
  type pstateset

  val peval: stmt -> pstateset -> pstateset
  val peval_start: stmt -> pstateset
end;;
