open Printf
open Unix
open Hashtbl
open Util

class timer = object(self)
  val time = ref (-1.0)
  val time_real = ref (-1.0)

  method reset =
    time := -1.0;
    time_real := -1.0; ()
    
  method mark : float * float = 
    let now = Unix.times () in
    let now_real = Unix.gettimeofday () in
    let newtime = now.tms_utime +. now.tms_stime +. now.tms_cutime +. now.tms_cstime in
    let ret =       
      if !time < 0.0 then
	(0.0, 0.0)
      else
	(newtime -. !time,
	 now_real -. !time_real)
    in
      (*printf "timer reporting %f, time = %f utime = %f stime = %f\n" ret !time now.tms_utime now.tms_stime;*)

      time := newtime;
      time_real := now_real;

      ret
end

class bench = object(self)
  val mutable out_chan: out_channel ref = ref Pervasives.stdout
(*open_out "bench.csv"   *)

  val mutable timer_total: (string, timer) Hashtbl.t = Hashtbl.create 16
  val mutable timer_current: (string, timer) Hashtbl.t = Hashtbl.create 16

  val mutable time_total: (string, float * float) Hashtbl.t = Hashtbl.create 16
  val mutable time_current: (string, float * float) Hashtbl.t = Hashtbl.create 16

  val mutable records: (string, string) Hashtbl.t = Hashtbl.create 16
  val mutable records_init: (string, string) Hashtbl.t = Hashtbl.create 16

  val mutable time_names: string list ref = ref []
  val mutable record_names: string list ref = ref []

  val mutable epoch: int ref = ref 0

  initializer
    self#new_timer "_epoch";
    self#start_timer "_epoch";

  method set_bench s =
    out_chan := open_out s

  method new_record r i =
    Hashtbl.replace records r i;
    Hashtbl.replace records_init r i;
    record_names := r :: !record_names

  method set_record r v = Hashtbl.replace records r v

  method inc_record r =
    Hashtbl.replace records r
      (string_of_int (1 + (int_of_string (Hashtbl.find records r))))

  method inc_val_record r v =
    Hashtbl.replace records r
      (string_of_int (v + (int_of_string (Hashtbl.find records r))))

  method max_record r v =
    Hashtbl.replace records r
      (string_of_int (max v (int_of_string (Hashtbl.find records r))))

  method new_timer (r:string) : unit =
    Hashtbl.replace timer_total r (new timer);
    Hashtbl.replace timer_current r (new timer);
    Hashtbl.replace time_total r (0.0, 0.0);
    Hashtbl.replace time_current r (0.0, 0.0);
    time_names := r :: !time_names

  method stop_timer r =
    let temp_current = Hashtbl.find timer_current r in
    let (elapsed_old, elapsed_old_real) = Hashtbl.find time_current r in
    let (elapsed, elapsed_real) = temp_current#mark in
      Hashtbl.replace time_current r ((elapsed_old +. elapsed),
				      (elapsed_old_real +. elapsed_real));
      temp_current#reset

  method start_timer r =
    let temp_current = Hashtbl.find timer_current r in
      ignore (temp_current#mark)

  method mark_epoch =
    self#stop_timer "_epoch";
    Hashtbl.fold
      (fun r (v,v_real) a ->
	 let (temp_total, temp_total_real) = Hashtbl.find time_total r in
	   Hashtbl.replace time_total r ((temp_total +. v), (temp_total_real +. v_real)))
      time_current ();

  method next_epoch =
    Hashtbl.fold
      (fun r v a -> Hashtbl.replace time_current r (0.0,0.0))
      time_current ();
    ignore (List.map (fun r -> Hashtbl.replace records r
			(Hashtbl.find records_init r))
	      !record_names);
     self#start_timer "_epoch"

  method str_header: string = String.concat ","
    (lists_append
       [["_epoch #"];
	!record_names;
	(List.map (fun r -> r ^ " (s)") !time_names);
	(List.map (fun r -> r ^ " (real s)") !time_names);
	(List.map (fun r -> r ^ " total (s)") !time_names);
	(List.map (fun r -> r ^ " total (real s)") !time_names)
       ]
    )

  method str_epoch: string =
    String.concat ","
      (lists_append
	 [[string_of_int !epoch];
	  (List.map (fun r -> Hashtbl.find records r) !record_names);
	  (List.map (fun r -> string_of_float (pair_first (Hashtbl.find time_current r))) !time_names);
	  (List.map (fun r -> string_of_float (pair_second (Hashtbl.find time_current r))) !time_names);
	  (List.map (fun r -> string_of_float (pair_first (Hashtbl.find time_total r))) !time_names);
	  (List.map (fun r -> string_of_float (pair_second (Hashtbl.find time_total r))) !time_names)
	 ]
      )

  method print_header =
    fprintf !out_chan "%s\n" self#str_header;
    flush !out_chan

  method print_epoch =
    fprintf !out_chan "%s\n" self#str_epoch;
    flush !out_chan;
    epoch := (!epoch + 1);

  method close =
    if (!out_chan != Pervasives.stdout) then close_out !out_chan

end
