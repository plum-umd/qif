open Util
open Gmp
open Gmp.Q.Infixes
open Gmp.Z.Infixes
open Printf
open Numeric

let zzero = Z.of_int 0
let zone  = Z.of_int 1
let ztwo  = Z.of_int 2
let znone = Z.of_int (-1)

let qzero = Q.from_ints 0 1
let qone  = Q.from_ints 1 1
let qnone = Q.from_ints (-1) 1

let qmin q1 q2 = if q1 <=/ q2 then q1 else q2
let qmax q1 q2 = if q1 <=/ q2 then q2 else q1

let qceil q =
  let temp = Z.cdiv_q (Q.get_num q) (Q.get_den q) in
    temp
;;

let qfloor q = 
  let temp = Z.fdiv_q (Q.get_num q) (Q.get_den q) in
    temp

let list_min_q l = list_ultimate (fun a b -> a </ b) l (List.hd l)
let list_max_q l = list_ultimate (fun a b -> a >/ b) l (List.hd l)

let list_min_q_fun f l = list_ultimate (fun a b -> f a </ f b) l (List.hd l)
let list_max_q_fun f l = list_ultimate (fun a b -> f a >/ f b) l (List.hd l)
  
let rec zlist_range blower bupper =
  if blower >! bupper then
    []
  else
    blower :: (zlist_range (blower +! zone) bupper)
;;

let qcoeff_to_string q =
  (if (Q.sgn q == 1) then "+" else "" )
  ^
    (
      if Z.equal_int (Q.get_den q) 1 then
	(Z.to_string (Q.get_num q))
      else Q.to_string q
    )

let qlist_sum ql = List.fold_left (fun a v -> a +/ v) qzero ql
  
let qsum f ibegin iend =
  List.fold_left
    (fun a i -> a +/ (f i))
    qzero
    (list_range ibegin iend)

let qprod f ibegin iend =
  List.fold_left
    (fun a i -> a */ (f i))
    qone
    (list_range ibegin iend)

let qprod_list f l =
  List.fold_left
    (fun a i -> a */ (f i))
    qone
    l

let zprod_list f l =
  List.fold_left
    (fun a i -> a *! (f i))
    zone
    l

let qpow q p =
  let num = Q.get_num q in
    Q.from_zs (Z.pow_ui num p) (Q.get_den q)

module ZNUM: (NUMBER_TYPE with type t = Z.t) = struct
  type t = Z.t

  let of_int = Z.of_int
  let to_int = Z.to_int

  let zero = zzero
  let one = zone
  let none = znone

  let negate = Z.neg

  let plus = Z.add
  let sum = List.fold_left plus zero
  let minus = Z.sub
  let times = Z.mul
  let prod = List.fold_left times one
  let div = Z.tdiv_q

  let greater a b = Z.cmp a b < 0
  let sign a = Z.cmp a zero

  let is_zero q = (sign q = 0)

  let to_string = Z.to_string
end;;

module QNUM: (NUMBER_TYPE with type t = Q.t) = struct
  type t = Q.t

  let of_int i = Q.from_ints i 1
  let to_int q = Z.to_int (Z.tdiv_q (Q.get_num q) (Q.get_den q))

  let zero = qzero
  let one = qone
  let none = qnone

  let negate = Q.neg

  let plus = Q.add
  let sum = List.fold_left plus zero
  let minus = Q.sub
  let times = Q.mul
  let prod = List.fold_left times one
  let div = Q.div

  let greater a b = Q.cmp a b < 0
  let sign a = Q.cmp a zero

  let is_zero q = (sign q = 0)

  let to_string = Q.to_string
end;;

module ZVECTOR = VECTOR(ZNUM);;
module ZMATRIX = MATRIX(ZNUM);;

module QVECTOR = VECTOR(QNUM);;
module QMATRIX = MATRIX(QNUM);;

let qmatrix_of_zmatrix zm =
  QMATRIX.of_ints (ZMATRIX.to_ints zm)

let qmatrix_of_intmatrix im =
  QMATRIX.of_ints (INTMATRIX.to_ints im)

let intmatrix_rank im = 
  QMATRIX.rank (qmatrix_of_intmatrix im)

let zmatrix_rank im = 
  QMATRIX.rank (qmatrix_of_zmatrix im)

let intvector_of_qvector qv =
  INTVECTOR.of_ints (QVECTOR.to_ints qv)
