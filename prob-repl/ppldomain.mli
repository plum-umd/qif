open ExtList
open ExtString
open Gmp
open Ppl_ocaml
open Pplutil
open Lang

module type PPLDOMAIN_TYPE =
sig
  type region

  val make_empty: int -> region
  val make_new: int -> region
  val make_point: (int * int) list -> region
  val make_rational_point: (int * Q.t) list -> region
  val make_range: Z.t -> Z.t -> region

  val region_of_constraints: int -> linear_constraint list -> region
  val poly_of_region: region -> polyhedron

  val string_of_region: region -> string
  val print_region: region -> unit

  val region_size: region -> Z.t

  val get_constraints: region -> linear_constraint list
  val add_constraint: region -> linear_constraint -> unit
  val of_constraints_poly: region -> linear_constraint list -> region

  val copy_region: region -> region

  val get_dimensions: region -> int
  val add_dimensions: region -> int -> unit
  val add_dimensions_and_set: region -> (int * Z.t) list -> unit
  val map_dimensions: region -> (int * int) list -> unit
  val remove_higher_dimensions: region -> int -> unit
  val duplicate_dimension: region -> int -> unit

  val intersect_region_poly: region -> polyhedron -> region
  val intersect_regions_assign: region -> region -> unit
  val union_regions_assign: region -> region -> unit
  val product_regions_assign: region -> region -> unit

  val regions_are_disjoint: region -> region -> bool
  val region_is_nonempty: region -> bool

  val affine_image: region -> int -> linear_expression -> unit

  val partition_regions: region -> region -> (region * (region list))

  val region_bounds: region -> (int * int) array
  val region_min_max_height: region -> int -> (Z.t * Z.t)

  val enum_region: region -> int list list

end;;
