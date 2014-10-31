open Util
open Printf
open Globals

module type NUMBER_TYPE = sig
  type t

  val of_int: int -> t
  val to_int: t -> int

  val negate: t -> t

  val plus: t -> t -> t
  val sum: t list -> t
  val minus: t -> t -> t
  val times: t -> t -> t 
  val prod: t list -> t
  val div: t -> t -> t

  val zero: t
  val one: t
  val none: t

  val to_string: t -> string

  val greater: t -> t -> bool

  val sign: t -> int

  val is_zero: t -> bool
end;;

module INT: (NUMBER_TYPE with type t = int) = struct
  type t = int

  let of_int i = i
  let to_int i = i

  let zero = 0
  let one = 1
  let none = -1

  let negate a = -1 * a
  let plus a b = a + b
  let sum = List.fold_left plus zero
  let minus a b = a - b
  let times a b = a * b
  let prod = List.fold_left times one
  let div a b = a / b

  let greater a b = a > b
  let sign n = compare n zero

  let to_string = string_of_int

  let is_zero i = (sign i = 0)
end;;

module type VECTOR_TYPE = sig
  module NM: NUMBER_TYPE
  type nt
  type t = nt array

  val to_string: t -> string
  val of_ints: int list -> t
  val to_ints: t -> int list
  val create: int -> t
  val size: t -> int
  val copy: t -> t

  val is_zero: t -> bool
end;;

module VECTOR(NM: NUMBER_TYPE) : (VECTOR_TYPE with type nt = NM.t) = struct
  module NM = NM
  type nt = NM.t
  type t = nt array
  let to_string v = String.concat " " (List.map NM.to_string (Array.to_list v))
  let of_ints il = Array.of_list (List.map NM.of_int il)
  let to_ints v = List.map NM.to_int (Array.to_list v)
  let create n = Array.create n NM.zero
  let size v = Array.length v
  let copy v = Array.copy v

  let is_zero v = List.for_all NM.is_zero (Array.to_list v)
end;;

module type MATRIX_TYPE = sig
  module NM: NUMBER_TYPE
  module VM: VECTOR_TYPE
  type nt
  type vt = nt array
  type t = nt array array

  val to_string: t -> string
  val of_ints: int list list -> t
  val to_ints: t -> int list list
  val of_vectors: vt list -> t
  val create: int -> int -> t
  val determinant: t -> nt
  val blit_vector: vt -> int -> t -> int -> int -> int -> unit
  val reduced_form: t -> t
  val rank: t -> int
  val is_zero: t -> bool
end;;

module MATRIX
  (NM: NUMBER_TYPE)
  : (MATRIX_TYPE with type nt = NM.t) = struct
    module NM = NM
    module VM : (VECTOR_TYPE with type nt = NM.t) = VECTOR(NM)
    type nt = NM.t
    type vt = nt array
    type t = nt array array

  let copy m = Array.map Array.copy m
  let size m = if Array.length m = 0 then (0,0) else (Array.length m, Array.length (m.(0)))
  let create n m = Array.create_matrix n m NM.zero

  let of_vectors vl = Array.of_list vl

  let blit_vector vec jsource mat itarget jtarget len =
    let j2 = ref jtarget in
      for j = jsource to jsource + len - 1 do
	mat.(itarget).(!j2) <- vec.(j);
	j2 := !j2 + 1
      done;
      ()

  let of_ints il =
    let n = List.length il in
      if n = 0 then create 0 0
      else
	(let m = List.length (List.hd il) in
	   if m = 0 then create 0 0 else
	     Array.of_list (List.map (fun row -> Array.of_list (List.map NM.of_int row)) il)
	)

  let to_ints m = List.map VM.to_ints (Array.to_list m)

  let assert_square m =
    let (n, m) = size m in
    if n <> m then
      raise (General_error "matrix is not square, cannot compute determinant")
    else
      ()

  let to_string mat =
    let (n, m) = size mat in
      sprintf "matrix (%d,%d):\n%s\n" n m
	(String.concat "\n"
	   (List.map
	      (fun row -> (String.concat "\t" (List.map (fun v -> NM.to_string v) (Array.to_list row))))
	      (Array.to_list mat)))

  let except_cross i j mat =
    let (n, m) = size mat in
    let ret = create (n-1) (m-1) in
      for y = 0 to i-1 do
	for x = 0 to j-1 do
	  ret.(y).(x) <- mat.(y).(x)
	done;
	for x = j+1 to m-1 do
	  ret.(y).(x-1) <- mat.(y).(x)
	done
      done;
      for y = i+1 to n-1 do
	for x = 0 to j - 1 do
	  ret.(y-1).(x) <- mat.(y).(x)
	done;
	for x = j+1 to m-1 do
	  ret.(y-1).(x-1) <- mat.(y).(x)
	done
      done;
      ret

  let rec determinant mat = 
    assert_square mat;
    let (n, _) = size mat in
      match n with 
	| 0 -> NM.zero
	| 1 -> mat.(0).(0)
	| n ->
	    let fac = ref NM.one in
	    let ret = ref NM.zero in
	      for i = 0 to n - 1 do
		let temp = except_cross 0 i mat in
		  ret := NM.plus !ret
		    (NM.prod [!fac;
			      mat.(0).(i);
			      (determinant temp)]);
		  fac := NM.negate !fac
	      done;
	      !ret

  let reduced_form mat =
    ifdebug (printf "reducing %s\n" (to_string mat));
    (* second pseudocode at http://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode *)

    let mat = copy mat in
    let (m, n) = size mat in
    let find_max_row k start =
      let i = ref start in
	while !i < m && NM.is_zero mat.(!i).(k) do
	  i := !i + 1
	done;
	if !i < m then (!i, mat.(!i).(k)) else (0, NM.zero) in
    let swap_rows r1 r2 =
      let temp = mat.(r1) in
	mat.(r1) <- mat.(r2);
	mat.(r2) <- temp in
    let k = ref (-1) in
    let good_k = ref (-1) in
      (try
      (while !k < n - 1 do
	k := !k + 1;
	let (i_max, i_max_val) = find_max_row !k (!good_k + 1) in
	  (*printf "k = %d, i_max=%d\n" !k i_max;*)
	  if (NM.sign i_max_val) <> 0 then
	    (
	      good_k := !good_k + 1;
	      if !good_k >= m then raise Loop_exit;
	      swap_rows !good_k i_max;
	      for i = !good_k + 1 to m-1 do
		for j = !k+1 to n-1 do
		  mat.(i).(j) <- NM.minus mat.(i).(j)
		    (
		      NM.times mat.(!good_k).(j)
			(
			  NM.div mat.(i).(!k) mat.(!good_k).(!k)
			)
		    )
		done;
		mat.(i).(!k) <- NM.zero
	      done)
      done; ())
      with Loop_exit -> ());
      ifdebug (printf "reduced = %s\n" (to_string mat));
	mat
	
  let is_zero m =
    List.for_all VM.is_zero (Array.to_list m)

  let rank m =
    let m = reduced_form m in
    let (rows, cols) = size m in
    let row = ref 0 in
      while (!row < rows) && not (VM.is_zero m.(!row)) do
	row := !row + 1
      done;
      !row

end;;

module INTVECTOR = VECTOR(INT);;
module INTMATRIX = MATRIX(INT);;

