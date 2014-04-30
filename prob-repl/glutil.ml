open GL
open Glu
open Glut
open Sdl
open Sdlimage
open Printf
open Global
open Sdldefs

let (sender: (Netlog.level -> string -> unit) Global.t) = empty "";;

let string_of_version (a,b,c) = 
  sprintf "sdl version %d.%d.%d" a b c;;

let logf level (form: ('a, unit, string, unit) format4) =
  ksprintf (fun s -> if isdef sender then
      ((get sender) level s) else
      begin () (*printf "%s\n" s; flush stdout*) end)
    form
;;

let w_width = ref 800;;
let w_widthf = ref (float_of_int !w_width);;
let w_height = ref 600;;
let w_heightf = ref (float_of_int !w_height);;
let w_aspect = ref 1.0;;
let fullscreen = ref false;;

let window_main = ref None;;

type tex_info = {surface: Sdlsurface.t;
                 mutable tid: texture_id option;
                 width: int;
                 height: int;
                 widthf: float;
                 heightf: float
                }

(*let font = empty "font";;*)

let (init_handler: ((unit -> unit) Global.t))  = empty "";;
let reinit_handler = empty "";;

let exec_handler h = if isdef h then (get h) ()

let get_viewport_position (x, y, z) =
  Glu.gluProjectUtil
    ~obj_x: x
    ~obj_y: y
    ~obj_z: z

let (textures: (string, tex_info) Hashtbl.t) = Hashtbl.create 8;;

let load_texture filename =
  try Hashtbl.find textures filename
  with Not_found ->
    let rwo = Sdlrwops.from_file ~filename:filename ~mode: "rb" in
    let surface = Sdlimage.load_png_rw rwo in

    (*let sinfo = Sdlsurface_ba.surface_info surface in*)

    let temp = {surface = surface;
                tid = None;
                width = Sdlsurface.get_width surface;
                height = Sdlsurface.get_height surface;
                widthf = float_of_int (Sdlsurface.get_width surface);
                heightf = float_of_int (Sdlsurface.get_height surface)} in

    Hashtbl.replace textures filename temp;

    temp
;;

let texture_get_tid t =
  match t.tid with
  | Some t -> t
  | None -> failwith "surface does not have opengl texture id"
;;

let make_gl_texture t =
  let tid = glGenTexture () in
  let s = t.surface in

  glBindTexture ~target: BindTex.GL_TEXTURE_2D ~texture: tid;

  let (intmode, mode) =
    (if (Sdlsurface.get_bits_per_pixel s) = 32 then
        (InternalFormat.GL_RGBA, GL_RGBA) else 
        (InternalFormat.GL_RGB, GL_RGB)) in

  glTexImage2D
    ~target: TexTarget.GL_TEXTURE_2D
    ~level: 0
    ~internal_format: intmode
    ~width: t.width
    ~height: t.height
    ~format_: mode
    ~type_: GL_UNSIGNED_BYTE
    ~pixels: (Bigarray.genarray_of_array1 (Sdlsurface_ba.get_pixels s));

  glTexParameter
    ~target: TexParam.GL_TEXTURE_2D
    ~param: (TexParam.GL_TEXTURE_MIN_FILTER Min.GL_LINEAR);
  glTexParameter
    ~target: TexParam.GL_TEXTURE_2D
    ~param: (TexParam.GL_TEXTURE_MAG_FILTER Mag.GL_LINEAR);

  t.tid <- Some tid
;;

(*
let make_ttf_text s =
  try Hashtbl.find textures s
  with Not_found ->
    let surface = Sdlttf.render_text_blended (get font) s Sdlvideo.white in
    let sinfo = Sdlvideo.surface_info surface in
    let temp = {surface = surface;
                tid = None;
                width = sinfo.Sdlvideo.w;
                height = sinfo.Sdlvideo.h;
                widthf = float_of_int (sinfo.Sdlvideo.w);
                heightf = float_of_int (sinfo.Sdlvideo.h)} in

    Hashtbl.replace textures s temp;
    temp
;;
*)

let enable2d () =
  (* http://www.gamedev.net/community/forums/topic.asp?topic_id=104791*)
  let (v0,v1,v2,v3) = glGetInteger4 Get.GL_VIEWPORT in

  glMatrixMode GL_PROJECTION; glPushMatrix (); glLoadIdentity ();
  glOrtho 0.0 (float_of_int v2) 0.0 (float_of_int v3) (-1.0) 1.0;
  glMatrixMode GL_MODELVIEW; glPushMatrix (); glLoadIdentity ()
;;

let disable2d () =
  glMatrixMode GL_PROJECTION; glPopMatrix();
  glMatrixMode GL_MODELVIEW; glPopMatrix()
;;

let draw_2d_texture tname ~x ~y =
  enable2d ();

  glEnable GL_TEXTURE_2D;  
  glDisable GL_LIGHTING;
  glDisable GL_DEPTH_TEST;

  let t = (Hashtbl.find textures tname) in
  glBindTexture BindTex.GL_TEXTURE_2D (texture_get_tid t);

  glBegin GL_QUADS;
  glTexCoord2 ~s: 1.0 ~t: 1.0; glVertex2 ~x: x ~y: y;
  glTexCoord2 ~s: 1.0 ~t: 0.0; glVertex2 ~x: x ~y: (y +. (t.heightf *. 0.5));
  glTexCoord2 ~s: 0.0 ~t: 0.0; glVertex2 ~x: (x -. 0.5 *. t.widthf) ~y: (y +. (t.heightf *. 0.5));
  glTexCoord2 ~s: 0.0 ~t: 1.0; glVertex2 ~x: (x -. 0.5 *. t.widthf) ~y: y;
  glEnd ();

  glDisable GL_TEXTURE_2D;

  disable2d ()
;;

let draw_wire_box ~xmin ~xmax ~ymin ~ymax ~zmin ~zmax =
  glBegin GL_LINE_LOOP;
  glVertex3 ~x: xmin ~y: ymin ~z: zmax;
  glVertex3 ~x: xmin ~y: ymax ~z: zmax;
  glVertex3 ~x: xmin ~y: ymax ~z: zmin;
  glVertex3 ~x: xmin ~y: ymin ~z: zmin;
  glEnd ();

  glBegin GL_LINE_LOOP;
  glVertex3 ~x: xmax ~y: ymin ~z: zmax;
  glVertex3 ~x: xmax ~y: ymax ~z: zmax;
  glVertex3 ~x: xmax ~y: ymax ~z: zmin;
  glVertex3 ~x: xmax ~y: ymin ~z: zmin;
  glEnd ();

  glBegin GL_LINES;
  glVertex3 ~x: xmin ~y: ymin ~z: zmax;
  glVertex3 ~x: xmax ~y: ymin ~z: zmax;

  glVertex3 ~x: xmin ~y: ymax ~z: zmax;
  glVertex3 ~x: xmax ~y: ymax ~z: zmax;

  glVertex3 ~x: xmin ~y: ymax ~z: zmin;
  glVertex3 ~x: xmax ~y: ymax ~z: zmin;

  glVertex3 ~x: xmin ~y: ymin ~z: zmin;
  glVertex3 ~x: xmax ~y: ymin ~z: zmin;
  glEnd ()
;;

let colored_wire_box_by_coords carray =
  let draw_vec = (fun i ->
    let (col, vec) = carray.(i) in
    glColor4v col; glVertex3v vec) in

  glBegin GL_LINE_LOOP;
  List.iter draw_vec [0;3;2;1];
  glEnd ();

  glBegin GL_LINE_LOOP;
  List.iter draw_vec [4;5;6;7];
  glEnd ();

  glBegin GL_LINES;
  List.iter draw_vec [0;4; 1;5; 2;6; 3;7];
  glEnd ();
;;

let colored_solid_box_by_coords carray =
  let draw_vec = (fun i ->
    let (col, vec) = carray.(i) in
    glColor4v col; glVertex3v vec) in

  glBegin GL_QUADS;
  List.iter draw_vec [0;3;2;1; 4;5;6;7; 0;4;7;3; 1;2;6;5; 0;1;5;4; 3;7;6;2];
  glEnd ();
;;

let set_window_size w h =
  w_width := w;
  w_height := h;
  w_widthf := float_of_int !w_width;
  w_heightf := float_of_int !w_height;
  w_aspect := !w_widthf /. !w_heightf;

  exec_handler reinit_handler
;;

let toggle_fullscreen () =
  fullscreen := not !fullscreen;

  (*
  if !fullscreen then begin
    match Sdlvideo.list_modes [`FULLSCREEN; `OPENGL] with
      | Sdlvideo.DIM (modes) ->
        begin match List.rev modes with
          | (w,h) :: _ -> set_window_size w h
          | _ -> failwith "could not get any video modes"
        end
      | _ -> failwith "could not get any video modes"
  end;
  *)

  exec_handler reinit_handler
;;

let init () =
  logf `Info "sdl = %s" (string_of_version (Sdl.Version.get_runtime_version ()));

  Sdl.init [`VIDEO];

  let w = Sdl.Window.create
    ~title: "ffi test"
    ~pos: (`centered, `centered) 
    ~dims: (640, 480)
    ~flags: [Sdlwindow.OpenGL;
             Sdlwindow.Resizable
            ] in

  let win = {W.width= 640;
             W.height= 480;
             W.widthf= 640.0;
             W.heightf= 480.0;
             W.aspect=640.0 /. 480.0;
             W.win= w} in

  window_main := Some win;

  let r = Sdl.Render.create_renderer
    ~win: w
    ~index: 0
    ~flags: [Sdl.Render.Accelerated] in

  let c = Sdl.GL.create_context ~win: w in
  ignore (Sdl.GL.make_current ~win: w ~ctx: c);
  let x = Sdl.GL.get_swap_interval () in

  (*
    let dname = (Sdl.Audio.get_drivers ()).(0) in
    printf "initializing audio: %s\n%!" dname;
    Sdl.Audio.init ~driver_name: dname;
    printf "initializing mixer: %d.%d\n%!"
    (Sdlmixer.get_major_version ())
    (Sdlmixer.get_minor_version ());
    Sdlmixer.init [`OGG];
  
    Sdlmixer.open_audio
    (Sdlmixer.get_default_frequency ())
    (Sdlmixer.get_default_format ())
    (Sdlmixer.get_default_channels ())
    4096;
  *)

  (*Sdlttf.init ();*)
  (*set font (Sdlttf.open_font "gfx/BemboStd.otf" 40);*)
  (*Sdlgl.set_attr [Sdlgl.DOUBLEBUFFER true];*)

  glutInit [||];

  exec_handler init_handler;
  exec_handler reinit_handler
;;

let finish () =
  Sdl.quit ()
;;

let stroke_string astring =
  glPushMatrix ();
  String.iter (fun c ->
    glutStrokeCharacter ~font: GLUT_STROKE_ROMAN ~c: c;
    glTranslatev (float_of_int (glutStrokeWidth ~font: GLUT_STROKE_ROMAN ~c: c),
                  0.0,
                  0.0))
    astring;
  glPopMatrix ();    
;;

let bitmap_string_font font astring =
  String.iter (fun c ->
    glutBitmapCharacter ~font: font ~c: c;
  ) astring
;;

let bitmap_string = bitmap_string_font GLUT_BITMAP_9_BY_15;;

let bitmap_string_small = bitmap_string_font GLUT_BITMAP_8_BY_13;;
