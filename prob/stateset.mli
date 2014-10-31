open State
open Lang
open Gmp

module type STATESET_TYPE =
  sig
    type stateset
    type splitter

    val stateset_empty: unit -> stateset

    val stateset_point: state -> stateset
    val stateset_new: Lang.varid list -> stateset

    val stateset_addvar: stateset -> Lang.varid -> stateset

    val print_stateset: stateset -> unit

    val stateset_copy: stateset -> stateset

    val stateset_size: stateset -> Z.t

    val stateset_prod: stateset -> stateset -> stateset

    val stateset_uniform: Lang.varid -> Z.t -> Z.t -> stateset
    val stateset_transform: stateset -> stmt -> stateset
    val stateset_intersect: stateset -> stateset -> stateset

    val stateset_is_empty: stateset -> bool

    val statesets_intersect_partition: ((stateset * 'a) list) -> ((stateset * 'a) list) ->
      ((stateset * 'a) list) * ((stateset * 'a) list) * ((stateset * 'a * 'a) list)

    val stateset_intersect_partition: stateset -> stateset -> (stateset list * stateset list * stateset)

    val stateset_split:
      stateset ->
      Lang.varid list ->
      (splitter list * splitter list) ->
      ((stateset list) * (stateset list))

    val statesets_union_list_nocomp: stateset list -> stateset
    val statesets_union_on_vars_list_nocomp: stateset list -> Lang.varid list -> stateset

    val stateset_set_all: stateset -> (Lang.varid * int) list -> stateset

    val stateset_on_vars: stateset -> Lang.varid list -> stateset
    val stateset_on_vars_nocomp: stateset -> Lang.varid list -> stateset

    val stateset_given_state: stateset -> state -> stateset

    val stateset_vars: stateset -> Lang.varid list

    val stateset_enum: stateset -> state list

    val stateset_union: stateset -> stateset -> stateset

    val statesets_approx_intersections: ((stateset * 'a) list) -> ('a list) list
    val statesets_exact_intersections: ((stateset * 'a) list) -> ('a list) list

    val stateset_make_splitter: stateset -> lexp -> (splitter list * splitter list)

    val stateset_min_max_height: stateset -> Lang.varid -> (Z.t * Z.t)
  end;;
