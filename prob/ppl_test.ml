open Printf
open Gmp
open Ppl_ocaml

let v1 = Variable 0;;

let p1 = ppl_new_C_Polyhedron_from_space_dimension 1 Universe;;
ppl_Polyhedron_add_constraint p1 (Less_Or_Equal (v1, (Coefficient (Z.of_int 2))));;
ppl_Polyhedron_add_constraint p1 (Greater_Or_Equal (v1, (Coefficient (Z.of_int 1))));;

let p2 = ppl_new_C_Polyhedron_from_space_dimension 1 Universe;;
ppl_Polyhedron_add_constraint p2 (Less_Or_Equal (v1, (Coefficient (Z.of_int 3))));;
ppl_Polyhedron_add_constraint p2 (Greater_Or_Equal (v1, (Coefficient (Z.of_int 2))));;

let (pinter, parts_of_p2) = ppl_Polyhedron_linear_partition p1 p2 in

let it = ppl_Pointset_Powerset_NNC_Polyhedron_begin_iterator parts_of_p2 in
let itend = ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator parts_of_p2 in

  while (not (ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equals_iterator it itend)) do

    let p = ppl_Pointset_Powerset_NNC_Polyhedron_get_disjunct it in
      printf "ok = %s\n" (string_of_bool (ppl_Polyhedron_OK p));
      flush stdout;

      ignore (ppl_Polyhedron_get_minimized_constraints p); (* Out_of_memory exception here *);

      ppl_Pointset_Powerset_NNC_Polyhedron_increment_iterator it
  done
;;
