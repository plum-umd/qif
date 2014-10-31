type ('a, 'b) bmap = {fmap: ('a, 'b) Hashtbl.t;
		      bmap: ('b, 'a) Hashtbl.t}

type ('a, 'b) t = ('a, 'b) bmap

let create isize =
  {fmap = Hashtbl.create isize;
   bmap = Hashtbl.create isize};;

let add bm k v =
  Hashtbl.replace bm.fmap k v;
  Hashtbl.replace bm.bmap v k;;

let copy bm =
  {fmap = Hashtbl.copy bm.fmap;
   bmap = Hashtbl.copy bm.bmap};;

let mem bm k = Hashtbl.mem bm.fmap k;;

let find bm k = Hashtbl.find bm.fmap k;;

let get_fmap bm = bm.fmap;;

let get_bmap bm = bm.bmap;;

let iter f bm = Hashtbl.iter f bm.fmap;;

let length bm = Hashtbl.length bm.fmap;;

let remove bm k =
  let v = find bm k in
    Hashtbl.remove bm.fmap k;
    Hashtbl.remove bm.bmap v

let keys bm =
  Hashtbl.fold
    (fun k v tally ->
       k :: tally) bm.fmap []

let pairs bm =
  Hashtbl.fold (fun k v a -> (k, v) :: a) bm.fmap []

let vals bm =
  Hashtbl.fold
    (fun k v tally ->
       k :: tally) bm.bmap []

let filter bm f =
  let ks = keys bm in
    List.iter
      (fun k ->
	 let v = find bm k in
	   if not (f k v) then remove bm k) ks
	 
		 
