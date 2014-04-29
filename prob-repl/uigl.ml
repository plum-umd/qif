open Printf
open GL
open Glu
open Glut
open Lang
open Global
open Glutil
open Ipc

let lid = GL_LIGHT 0;;

let num_samples = ref 0;;
let target_samples = ref 0;;
let weight_total = ref 0.0;;
let weight_min = ref 0.0;;
let weight_max = ref 1.0;;

let absweight_total = ref 0.0;;
let absweight_min = ref 0.0;;
let absweight_max = ref 1.0;;
let abssamples_have_changed = ref true;;
let abssamples_call_list = ref 0;;

let bar_width = 0.25;;

(*
  let (samples_all: ((float * (string * Lang.value) list) DynArray.t)) = DynArray.create ();;
  let (samples_view: ((float * float * float) DynArray.t)) = DynArray.create ();;
  let samples_hash = Hashtbl.create 1024;;
*)

let (samples_all: (float * (float array)) DynArray.t) = DynArray.create ();;
let (samples_view: (float array) DynArray.t) = DynArray.create ();;
let samples_hash = Hashtbl.create 1024;;

type coord = float array
type face = coord list
type shape = face list

let show_samples = ref true;;
let show_abssamples = ref true;;

let toggle_show_samples () =
  show_samples := not !show_samples;;
let toggle_show_abssamples () =
  show_abssamples := not !show_abssamples;;

let abssamples_all: (float * float * shape) DynArray.t = DynArray.create ();;

let samples_call_list = ref 0;;
let samples_last_time = ref 0.0;;
let samples_have_changed = ref true;;

let workers_load = Array.create Config.num_cores 0;;
let workers_completed = Array.create Config.num_cores 0;;
let workers_accepted = Array.create Config.num_cores 0;;
let workers_rejected = Array.create Config.num_cores 0;;

let view = Array.of_list ["x"; "y"; "z"];;

let range_min   = Array.of_list [infinity; infinity; infinity];;
let range_max   = Array.of_list [neg_infinity; neg_infinity; neg_infinity];;
let range_width = Array.of_list [1.0; 1.0; 1.0];;

let range_diam  = ref 3.464;;
let range_scale = ref 0.6;;

let a = ref 0.0;;
let b = ref 0.0;;
let c = ref 0.0;;

let val_numeric aval =
  match aval with
    | VBool true -> 1.0
    | VBool false -> 0.0
    | VInt i -> float_of_int i
    | VReal i -> i
    | _ -> 0.0
;;

let get_dump_pos adump aname =
  try val_numeric (List.assoc aname adump)
  with Not_found -> 0.0
;;

let get_dump_view adump =
  (get_dump_pos adump view.(0),
   get_dump_pos adump view.(1),
   get_dump_pos adump view.(2))
;;

let setup_subject_view () =
  glTranslate ~x: 0.0 ~y: 0.0 ~z: (-2.0);

  glScalev (!range_scale, !range_scale, !range_scale);

  glRotate ~angle: !b ~x: 0.0 ~y: 1.0 ~z: 0.0;
  glRotate ~angle: !c ~x: 1.0 ~y: 1.0 ~z: 0.0;

  glTranslate
    ~x: (-. (range_min.(0) +. 0.5 *. range_width.(0)))
    ~y: (-. (range_min.(1) +. 0.5 *. range_width.(1)))
    ~z: (-. (range_min.(2) +. 0.5 *. range_width.(2)))
;;

let setup_light_position () =
  glMatrixMode GL_MODELVIEW;
  glLoadIdentity ();
;;

let setup_light () =
  glEnable GL_LIGHTING;
  glEnable GL_LIGHT0;

  glLight ~light: lid 
    ~pname: (Light.GL_AMBIENT (0.4, 0.4, 0.4, 1.0));
  glLight ~light: lid 
    ~pname: (Light.GL_DIFFUSE (1.0, 1.0, 1.0, 1.0));
  glLight ~light: lid 
    ~pname: (Light.GL_SPECULAR (1.0, 1.0, 1.0, 1.0));
  glLight ~light: lid ~pname: (Light.GL_POSITION (0.0, 0.0, 10.0, 0.0));
  
  glMaterial ~face: GL_FRONT ~mode: (Material.GL_SPECULAR (1.0, 1.0, 1.0, 0.25));
  glMaterial ~face: GL_FRONT ~mode: (Material.GL_SHININESS 10.0);
;;

let recompute_ranges () =
  List.iter (fun i ->
    range_width.(i) <- 1.0 +. (range_max.(i) -. range_min.(i))
  ) [0;1;2];

  range_diam := sqrt (range_width.(0) ** 2.0 +.
                        range_width.(1) ** 2.0 +.
                        range_width.(2) ** 2.0);

  range_scale := 2.0 /. !range_diam;

  setup_light_position ();
  setup_light ();

  glMatrixMode GL_PROJECTION;
  glLoadIdentity ();
  gluPerspective ~fovy:60.0 ~aspect:!w_aspect ~zNear: 1.0 ~zFar: 20.0
;;

let consider_abssample (weight, weight_total, adump) =
  absweight_total := weight_total +. !absweight_total;
  if weight < !absweight_min then absweight_min := weight;
  if weight > !absweight_max then absweight_max := weight;
;;

let consider_sample (weight, adump) =
  Array.iteri (fun i anum ->
    let current_min = range_min.(i) in
    let current_max = range_max.(i) in

    if anum > current_max then begin
      range_max.(i) <- anum
    end;
    if anum < current_min then begin
      range_min.(i) <- anum
    end;

  ) adump;

  weight_total := weight +. !weight_total;

  recompute_ranges ();

  try
    let old_weight = Hashtbl.find samples_hash adump in
    let new_weight = old_weight +. weight in
    Hashtbl.replace samples_hash adump new_weight;
    if new_weight > !weight_max then weight_max := new_weight
  with Not_found ->
    Hashtbl.replace samples_hash adump weight;
    DynArray.add samples_view adump;
;;

let reset_view () = 
  List.iter
    (fun i -> range_min.(i) <- 0.0; range_max.(i) <- 0.0; range_width.(i) <- 1.0) [0;1;2];

  range_diam := 3.464;
  range_scale := 0.6;

  setup_light_position ();
  setup_light ();

  glMatrixMode GL_PROJECTION;
  glLoadIdentity ();
  gluPerspective ~fovy:60.0 ~aspect:!w_aspect ~zNear: 1.0 ~zFar: 20.0;
;;

let add_abssample data =
  let (weight, weight_total, shape) = data.abswork_dump in

  abssamples_have_changed := true;

  DynArray.add abssamples_all (weight, weight_total, shape);

  consider_abssample (weight, weight_total, shape)
;;

let add_sample data =
  samples_have_changed := true;

  let weight = data.work_weight in
  let dump = data.work_dump in
  let num = data.worker_num in
  
  workers_load.(num) <- data.worker_load;
  if workers_completed.(num) < data.worker_completed then workers_completed.(num) <- data.worker_completed;
  if workers_accepted.(num) < data.worker_accepted then workers_accepted.(num) <- data.worker_accepted;
  if workers_rejected.(num) < data.worker_rejected then workers_rejected.(num) <- data.worker_rejected;

  consider_sample (weight, dump);

  DynArray.add samples_all (weight, dump);

  num_samples := !num_samples + 1;
;;

let clear_samples () =
  weight_total := 0.0;
  num_samples := 0;
  weight_min := infinity;
  weight_max := neg_infinity;

  List.iter (fun i ->
    workers_completed.(i) <- 0;
    workers_accepted.(i) <- 0;
    workers_rejected.(i) <- 0) (Util.list_range 0 Config.num_cores);

  DynArray.clear abssamples_all;
  absweight_total := 0.0;
  absweight_min := infinity;
  absweight_max := neg_infinity;

  DynArray.clear samples_all;
  DynArray.clear samples_view;
  Hashtbl.clear samples_hash;
;;

let reset_samples new_target_samples =
  reset_view ();
  target_samples := new_target_samples;
  clear_samples ()
;;

let reset new_target_samples =
  reset_samples new_target_samples;
;;

let set_view f1 f2 f3 =
  view.(0) <- f1;
  view.(1) <- f2;
  view.(2) <- f3;

  reset !target_samples
;;

let init () = 
  target_samples := !Worker.num_samples * Config.num_cores;
;;

let reinit () =
  let icon = load_texture "gfx/alpha_prob_icon.png" in

  samples_have_changed := true;
  abssamples_have_changed := true;

  Sdlwm.set_caption ~title: "alphaProb" ~icon: "alphaProb";
  Sdlwm.set_icon icon.surface;

  ignore begin if !fullscreen then
      Sdlvideo.set_video_mode !w_width !w_height [`OPENGL; `RESIZABLE; `FULLSCREEN]
    else begin
      Sdlvideo.set_video_mode !w_width !w_height [`OPENGL; `RESIZABLE]
    end end;

  samples_call_list := glGenList ();
  abssamples_call_list := glGenList ();

  let logo = load_texture "gfx/alpha_prob_logo.png" in

  make_gl_texture logo;

  glEnable GL_CULL_FACE;
  glCullFace GL_BACK;

  glMatrixMode GL_PROJECTION; glLoadIdentity ();
  gluPerspective ~fovy:60.0 ~aspect:!w_aspect ~zNear: 1.0 ~zFar: 20.0;
  glViewport ~x: 0 ~y: 0 ~width: !w_width ~height: !w_height;

  glEnable GL_NORMALIZE;

  glEnable GL_BLEND;
  glBlendFunc Sfactor.GL_SRC_ALPHA Dfactor.GL_ONE_MINUS_SRC_ALPHA;

  setup_light_position ();
  setup_light ();

  glClearColor ~r: 0.2 ~g: 0.2 ~b: 0.2 ~a: 1.0;

;;

let draw_light () =
  glEnable GL_LIGHTING;
  glEnable GL_LIGHT0;

  glPushMatrix ();

  setup_light_position ();

  glPushMatrix ();
  glTranslatev (0.0, 0.0, 2.5);
  glutSolidSphere ~radius: 1.0 ~slices: 10 ~stacks: 10;

  glTranslatev (0.0, 0.0, 2.5);
  glutSolidSphere ~radius: 1.0 ~slices: 10 ~stacks: 10;

  glTranslatev (0.0, 0.0, 2.5);
  glutSolidSphere ~radius: 1.0 ~slices: 10 ~stacks: 10;

  glTranslatev (0.0, 0.0, 2.5);
  glutSolidSphere ~radius: 1.0 ~slices: 10 ~stacks: 10;
  glPopMatrix ();

  glBegin GL_LINES;
  glVertex3v (0.0, 0.0, 0.0);
  glVertex3v (0.0, 0.0, 10.0);
  glEnd ();

  glPopMatrix ()
;;

let draw_range () =
  glEnable GL_LIGHTING;
  glDisable GL_DEPTH_TEST;

  glLineWidth 1.0;
  glColor4v (1.0, 1.0, 1.0, 0.5);

  draw_wire_box
    ~xmin: range_min.(0)
    ~xmax: (range_min.(0) +. range_width.(0))
    ~ymin: range_min.(1)
    ~ymax: (range_min.(1) +. range_width.(1))
    ~zmin: range_min.(2)
    ~zmax: (range_min.(2) +. range_width.(2))
;;

let draw_abssamples () =
  glDisable GL_LIGHTING;
  glDisable GL_DEPTH_TEST;

  glColor4v (1.0, 0.0, 0.0, 1.0);

  glLineWidth 0.5;

  if !abssamples_have_changed then begin
    abssamples_have_changed := false;

    glNewList !abssamples_call_list GL_COMPILE_AND_EXECUTE;

    DynArray.iter (fun (weight,weighttotal,shape) ->

      glColor4v (1.0, 0.0, 0.0, weight /. !absweight_max);
      List.iter (fun face ->
        glBegin GL_POLYGON;
        List.iter (fun coord -> 

          glVertex3v (coord.(0), coord.(1), coord.(2));

          Array.iteri (fun i v ->
            if v > range_max.(i) then range_max.(i) <- v;
            if v < range_min.(i) then range_min.(i) <- v
          ) coord
        ) face;
        glEnd ()) shape;

      glColor4v (1.0, 0.0, 0.0, 1.0);
      List.iter (fun face ->
        glBegin GL_LINE_LOOP;
        List.iter (fun coord -> 

          glVertex3v (coord.(0), coord.(1), coord.(2));

          Array.iteri (fun i v ->
            if v > range_max.(i) then range_max.(i) <- v;
            if v < range_min.(i) then range_min.(i) <- v
          ) coord
        ) face;
        glEnd ()) shape

    ) abssamples_all;

    glEndList ();
    
  end else begin
    glCallList !abssamples_call_list
  end
;;


let draw_samples () =
  (*  glClear [GL_DEPTH_BUFFER_BIT];
      glEnable GL_DEPTH_TEST;*)

  glDisable GL_LIGHTING;
  glDisable GL_DEPTH_TEST;

  let temp_weight = ref infinity in
  let current_time = Unix.time () in

  if current_time -. !samples_last_time > 1.0 && !samples_have_changed then begin
    samples_have_changed := false;

    glNewList !samples_call_list GL_COMPILE_AND_EXECUTE;

    glBegin GL_QUADS;

    DynArray.iter
      (fun s ->
        let (x,y,z) = (s.(0), s.(1), s.(2)) in
        let weight = Hashtbl.find samples_hash s in

        if weight < !temp_weight then temp_weight := weight;
        let weight = weight /. !weight_max in

        glColor4v (0.0, 0.0, 1.0, weight);

        glVertex3v (x,y,z);
        glVertex3v (x,y+.1.0,z);
        glVertex3v (x+.1.0,y+.1.0,z);
        glVertex3v (x+.1.0,y,z);

        glVertex3v (x,y,z+.1.0);
        glVertex3v (x+.1.0,y,z+.1.0);
        glVertex3v (x+.1.0,y+.1.0,z+.1.0);
        glVertex3v (x,y+.1.0,z+.1.0);

        glVertex3v (x, y+.1.0, z+.1.0);
        glVertex3v (x+.1.0, y+.1.0, z+.1.0);
        glVertex3v (x+.1.0, y+.1.0, z);
        glVertex3v (x, y+.1.0, z);

        glVertex3v (x, y, z);
        glVertex3v (x+.1.0, y, z);
        glVertex3v (x+.1.0, y, z+.1.0);
        glVertex3v (x, y, z+.1.0);

        glVertex3v (x, y, z);
        glVertex3v (x, y, z+.1.0);
        glVertex3v (x, y+.1.0, z+.1.0);
        glVertex3v (x, y+.1.0, z);

        glVertex3v (x+.1.0, y, z);
        glVertex3v (x+.1.0, y+.1.0, z);
        glVertex3v (x+.1.0, y+.1.0, z+.1.0);
        glVertex3v (x+.1.0, y, z+.1.0);

      ) samples_view;

    glEnd();

    (*
    glLineWidth 0.1;
    glColor4v (0.0, 0.0, 1.0, 1.0);

    DynArray.iter
      (fun s ->
        let (x,y,z) = (s.(0), s.(1), s.(2)) in

        glBegin GL_LINE_LOOP;
        glVertex3v (x,y,z);
        glVertex3v (x,y+.1.0,z);
        glVertex3v (x+.1.0,y+.1.0,z);
        glVertex3v (x+.1.0,y,z);
        glEnd ();
        glBegin GL_LINE_LOOP;
        glVertex3v (x,y,z+.1.0);
        glVertex3v (x+.1.0,y,z+.1.0);
        glVertex3v (x+.1.0,y+.1.0,z+.1.0);
        glVertex3v (x,y+.1.0,z+.1.0);
        glEnd ();
        glBegin GL_LINE_LOOP;
        glVertex3v (x, y+.1.0, z+.1.0);
        glVertex3v (x+.1.0, y+.1.0, z+.1.0);
        glVertex3v (x+.1.0, y+.1.0, z);
        glVertex3v (x, y+.1.0, z);
        glEnd ();
        glBegin GL_LINE_LOOP;
        glVertex3v (x, y, z);
        glVertex3v (x+.1.0, y, z);
        glVertex3v (x+.1.0, y, z+.1.0);
        glVertex3v (x, y, z+.1.0);
        glEnd ();
        glBegin GL_LINE_LOOP;
        glVertex3v (x, y, z);
        glVertex3v (x, y, z+.1.0);
        glVertex3v (x, y+.1.0, z+.1.0);
        glVertex3v (x, y+.1.0, z);
        glEnd ();
        glBegin GL_LINE_LOOP;
        glVertex3v (x+.1.0, y, z);
        glVertex3v (x+.1.0, y+.1.0, z);
        glVertex3v (x+.1.0, y+.1.0, z+.1.0);
        glVertex3v (x+.1.0, y, z+.1.0);
        glEnd ();
      ) samples_view;

    *)

    glEndList ();

    samples_last_time := Unix.time ();

    weight_min := !temp_weight;
  end else begin
    glCallList !samples_call_list

  end

;;

let draw_compass () =
  glPushMatrix ();

  glRotate ~angle: !b ~x: 0.0 ~y: 1.0 ~z: 0.0;
  glRotate ~angle: !c ~x: 1.0 ~y: 1.0 ~z: 0.0;

  let (xx, xy, _) = get_viewport_position (1.0, 0.0, 0.0) in
  let (yx, yy, _) = get_viewport_position (0.0, 1.0, 0.0) in
  let (zx, zy, _) = get_viewport_position (0.0, 0.0, 1.0) in

  glLineWidth 3.0;
  
  glBegin GL_LINES;
  glColor4 ~r: 1.0 ~g: 0.0 ~b: 0.0 ~a: 1.0;
  glVertex3 ~x: 0.0 ~y: 0.0 ~z: 0.0;
  glVertex3 ~x: 1.0 ~y: 0.0 ~z: 0.0;

  glColor4 ~r: 0.0 ~g: 1.0 ~b: 0.0 ~a: 1.0;
  glVertex3 ~x: 0.0 ~y: 0.0 ~z: 0.0;
  glVertex3 ~x: 0.0 ~y: 1.0 ~z: 0.0;

  glColor4 ~r: 0.0 ~g: 0.0 ~b: 1.0 ~a: 1.0;
  glVertex3 ~x: 0.0 ~y: 0.0 ~z: 0.0;
  glVertex3 ~x: 0.0 ~y: 0.0 ~z: 1.0;
  glEnd ();

  glPopMatrix ();

  glDisable GL_DEPTH_TEST;

  enable2d ();

  glColor4v (1.0, 0.0, 0.0, 1.0);
  glRasterPos2v (xx +. 1.0, xy +. 1.0);
  bitmap_string view.(0);

  glColor4v (0.0, 1.0, 0.0, 1.0);
  glRasterPos2v (yx +. 1.0, yy +. 1.0);
  bitmap_string view.(1);

  glColor4v (0.0, 0.0, 1.0, 1.0);
  glRasterPos2v (zx +. 1.0, zy +. 1.0);
  bitmap_string view.(2);

  disable2d ();
;;

let draw_scale cbox c0 c1 maxval =
  glDisable GL_LIGHTING;

  glLineWidth 1.0;
  glColor4v cbox;

  glDisable GL_DEPTH_TEST;

  colored_wire_box_by_coords (Array.of_list [(c1, (0.0, 0.0, 0.0));
                                             (c1, (3.0, 0.0, 0.0));
                                             (c1, (3.0, bar_width, 0.0));
                                             (c1, (0.0, bar_width, 0.0));
                                             
                                             (c1, (0.0, 0.0, bar_width));
                                             (c1, (3.0, 0.0, bar_width));
                                             (c1, (3.0, bar_width, bar_width));
                                             (c1, (0.0, bar_width, bar_width))]);

  colored_solid_box_by_coords (Array.of_list [(c0, (0.0, 0.0, 0.0));
                                              (c1, (3.0, 0.0, 0.0));
                                              (c1, (3.0, bar_width, 0.0));
                                              (c0, (0.0, bar_width, 0.0));

                                              (c0, (0.0, 0.0, bar_width));
                                              (c1, (3.0, 0.0, bar_width));
                                              (c1, (3.0, bar_width, bar_width));
                                              (c0, (0.0, bar_width, bar_width))]);

  let (x1,y1, _) = get_viewport_position (0.0, -0.5, bar_width) in
  let (x2,y2, _) = get_viewport_position (3.0, -0.5, bar_width) in

  glColor4v c1;

  glBegin GL_LINES;
  glVertex3v (0.0, 0.0, bar_width);
  glVertex3v (0.0, -0.5, bar_width);

  glVertex3v (3.0, 0.0, bar_width);
  glVertex3v (3.0, -0.5, bar_width);
  glEnd ();

  enable2d ();

  glLineWidth 1.0;

  glPushMatrix ();
  glColor4v (1.0, 1.0, 1.0, 1.0);

  glRasterPos2v (x1 +. 2.0, y1 +. 2.0);
  bitmap_string (sprintf "%0.3f" 0.0);

  glRasterPos2v (x2 +. 2.0, y2 +. 2.0);
  bitmap_string (sprintf "%0.3f" maxval);
  glPopMatrix ();

  disable2d ();
;;

let draw_progress () =
  glDisable GL_LIGHTING;

  glPushMatrix ();

  glLineWidth 1.0;

  let c0 = (0.0, 1.0, 0.0, 0.0) in
  let c1 = (0.0, 1.0, 0.0, 1.0) in

  let width = bar_width in
  let total_width = ref 0.0 in

  let total_completed = ref 0 in
  let total_rejected = ref 0 in

  List.iter (fun i ->
    let premain = (float_of_int (workers_load.(i) - workers_accepted.(i))) /.
      (float_of_int (workers_load.(i))) in

    total_completed := !total_completed + workers_completed.(i);
    total_rejected := !total_rejected + workers_rejected.(i);

    colored_wire_box_by_coords (Array.of_list [(c1, (0.0, 0.0, 0.0));
                                               (c1, (width, 0.0, 0.0));
                                               (c1, (width, 1.0, 0.0));
                                               (c1, (0.0, 1.0, 0.0));
                                               
                                               (c1, (0.0, 0.0, width));
                                               (c1, (width, 0.0, width));
                                               (c1, (width, 1.0, width));
                                               (c1, (0.0, 1.0, width))]);

    colored_solid_box_by_coords (Array.of_list [(c0, (0.0, 0.0, 0.0));
                                                (c0, (width, 0.0, 0.0));
                                                (c1, (width, premain, 0.0));
                                                (c1, (0.0, premain, 0.0));
                                                
                                                (c0, (0.0, 0.0, width));
                                                (c0, (width, 0.0, width));
                                                (c1, (width, premain, width));
                                                (c1, (0.0, premain, width))]);

    glTranslatev (width *. 1.5, 0.0, 0.0);

    total_width := !total_width +. 1.5 *. width;

  ) Config.list_cores;

  total_width := !total_width -. 0.5 *. width;

  let (x,y,_) = get_viewport_position (0.0, 0.5, 0.0) in

  glPopMatrix();

  glPushMatrix();

  glTranslatev (0.0, -. width *. 1.5, 0.0);

  let c0 = (0.0, 1.0, 0.0, 0.0) in
  let c1 = (0.0, 1.0, 0.0, 1.0) in

  glLineWidth 1.0;
  colored_wire_box_by_coords (Array.of_list [(c1, (0.0, 0.0, 0.0));
                                             (c1, (!total_width, 0.0, 0.0));
                                             (c1, (!total_width, width, 0.0));
                                             (c1, (0.0, width, 0.0));
                                             
                                             (c1, (0.0, 0.0, width));
                                             (c1, (!total_width, 0.0, width));
                                             (c1, (!total_width, width, width));
                                             (c1, (0.0, width, width))]);

  let sscale = !total_width *. (float_of_int !num_samples) /. (float_of_int !target_samples) in
  let temp = (!total_width -. sscale) /. !total_width in

  colored_solid_box_by_coords (Array.of_list [(c0, (0.0, 0.0, 0.0));
                                              (c1, (sscale, 0.0, 0.0));
                                              (c1, (sscale, width, 0.0));
                                              (c0, (0.0, width, 0.0));
                                              
                                              (c0, (0.0, 0.0, width));
                                              (c1, (sscale, 0.0, width));
                                              (c1, (sscale, width, width));
                                              (c0, (0.0, width, width))]);



  glBegin GL_LINES;

  glColor4v c1;
  glVertex3v (!total_width, 0.0, width);
  glVertex3v (!total_width, -0.25, width);

  glColor4v (0.0, 1.0, 0.0, temp);
  glVertex3v (sscale, 0.0, width);
  glVertex3v (sscale, -0.25, width);

  glEnd ();

  let (x1, y1, _) = get_viewport_position (sscale, -0.25, width) in
  let (x2, y2, _) = get_viewport_position (!total_width, -0.25, width) in

  glPopMatrix ();

  enable2d ();

  glLineWidth 2.0;

  if !total_completed > 0 then begin
    let rejection = float_of_int !total_rejected /. float_of_int !total_completed in
    glColor4v (rejection, 1.0 -. rejection, 0.0, 1.0);
    glRasterPos2v (x +. 1.0, y);
    bitmap_string (sprintf "%0.0f%% rejection" (100.0 *. rejection))
  end;

  glColor4v (0.0, 1.0, 0.0, temp);
  glRasterPos2v (x1 +. 2.0, y1 +. 2.0);
  bitmap_string (sprintf "%d" !num_samples);

  glColor4v c1;
  glRasterPos2v (x2 +. 2.0, y2 +. 2.0);
  bitmap_string (sprintf "%d" !target_samples);

  disable2d ();
;;

let draw_hud () =
  glPushMatrix ();
  glTranslatev (0.0, 0.0, -10.0);

  glPushMatrix ();
  glTranslatev(-5.0, -4.5, 0.0);
  draw_progress ();
  glPopMatrix ();

  glPushMatrix ();
  glTranslatev (-5.0, 4.5, 0.0);

  draw_scale
    (0.25, 1.0, 0.25, 0.8)
    (0.0, 0.0, 1.0, 0.0)
    (0.0, 0.0, 1.0, 1.0)
    (!weight_max /. !weight_total);

  glTranslatev (0.0, -1.0, 0.0);

  draw_scale
    (1.0, 0.25, 0.25, 0.8)
    (1.0, 0.0, 0.0, 0.0)
    (1.0, 0.0, 0.0, 1.0)
    (!absweight_max /. !absweight_total);

  glPopMatrix ();

  glPushMatrix ();
  glTranslatev (-4.0, 0.0, 0.0);
  draw_compass ();
  glPopMatrix ();

  glPopMatrix ();
;;

let mark () =
  a := !a +. 1.27;
  b := !b +. 0.111;
  c := !c +. 0.213
;;

let draw_logo () =
  glColor4 ~r: 1.0 ~g: 1.0 ~b: 1.0 ~a: 0.1;
  draw_2d_texture "gfx/alpha_prob_logo.png" ~x: !w_widthf ~y: 0.0
;;

let draw_cube () =
  glColor4 ~r: 1.0 ~g: 1.0 ~b: 1.0 ~a: 1.0;
  glutSolidCube ~size: 1.0;

  glColor4 ~r: 1.0 ~g: 1.0 ~b: 1.0 ~a: 1.0;
  glutWireCube ~size: 1.0
;;

let draw_guides () =
  enable2d ();

  glLineWidth 1.0;

  glColor4 ~r: 1.0 ~g: 1.0 ~b: 1.0 ~a: 0.25;

  glBegin GL_LINES;  
  glVertex3 ~x: 0.0       ~y: (0.5 *. !w_heightf) ~z: 0.0;
  glVertex3 ~x: !w_widthf ~y: (0.5 *. !w_heightf) ~z: 0.0;

  glVertex3 ~y: 0.0        ~x: (0.5 *. !w_widthf) ~z: 0.0;
  glVertex3 ~y: !w_heightf ~x: (0.5 *. !w_widthf) ~z: 0.0;

  glEnd ();

  disable2d ()

let render () =
  glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];

  glMatrixMode GL_MODELVIEW;
  glLoadIdentity ();

  (*draw_light ();*)

  draw_guides ();

  glPushMatrix ();
  setup_subject_view ();

  glDisable GL_DEPTH_TEST;
  glLineWidth ~width: 1.0;

  glEnable GL_LIGHTING;
  glEnable GL_LIGHT0;
  
  if DynArray.length samples_view = 0 then
    begin
      (* draw_cube ();*)
      draw_range ()
    end
  else begin
    if !show_abssamples then draw_abssamples ();
    if !show_samples then draw_samples ();
    draw_range ()
  end;

  glPopMatrix ();

  draw_logo ();
  draw_hud ();

  mark ();

  Sdlgl.swap_buffers ()
;;



