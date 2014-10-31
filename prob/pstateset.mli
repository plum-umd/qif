open State
open Stateset
open Lang
open Gmp
open Gmp_util
open Printf
open Util
(*open Precise*)
open Ppl_ocaml

module type PSTATESET_TYPE =
  sig
    type stateset
    type pstateset
    type splitter

    module SS: STATESET_TYPE

    val copy: pstateset -> pstateset

    val make_empty: unit -> pstateset

    val make_point: state -> pstateset
    val make_point_of_stateset: stateset -> pstateset

    val make_new: Lang.varid list -> pstateset 

    val addvar: pstateset -> Lang.varid -> pstateset

    val print: pstateset -> unit

    val size: pstateset -> Z.t

    val slack: pstateset -> Q.t

    val prod: pstateset -> pstateset -> pstateset

    val make_uniform: Lang.varid -> Z.t -> Z.t -> pstateset
    val transform: pstateset -> stmt -> pstateset

    val intersect: pstateset -> stateset -> pstateset

    val exclude: pstateset -> pstateset -> pstateset list

    val is_empty: pstateset -> bool

    val make_splitter: pstateset -> lexp -> (splitter list * splitter list)
    val split: pstateset -> lexp -> (pstateset * pstateset)
    val split_many: pstateset -> lexp -> ((pstateset list) * (pstateset list))
    val split_many_with_splitter: pstateset -> Lang.varid list -> (splitter list * splitter list) ->
      (pstateset list * pstateset list)

    val set_all: pstateset -> (Lang.varid * int) list -> pstateset

    val project: pstateset -> Lang.varid list -> pstateset

    (*val revise: pstateset -> state -> pstateset *)

    val vars: pstateset -> Lang.varid list
    val enum: pstateset -> state list
    val enum_on_vars: pstateset -> Lang.varid list -> state list

    val abstract_plus: pstateset -> pstateset -> pstateset

    val relative_entropy: pstateset -> pstateset -> float

    val prob_scale: pstateset -> Q.t -> pstateset

    val prob_max_in_min_out: pstateset -> state -> (Q.t * Q.t)

    val prob_max_norm: pstateset -> state -> Q.t (* returns the max prob of a point normalized *)

    val prob_max_min: pstateset -> (Q.t * Q.t)

    val min_mass: pstateset -> Q.t

    val max_belief: pstateset -> Q.t (* returns the maximal normalized max prob over all points *)

    val is_possible: pstateset -> bool

    val stateset_hull: pstateset -> stateset
  end;;
