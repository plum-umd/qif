open GL
open Sdl
open Sdlimage
open Printf
open Sdldefs

let textures = Hashtbl.create 16;;

let load_png f =
  printf "loading [%s]\n%!" f;
  let rwo = Sdlrwops.from_file ~filename:f ~mode: "rb" in

  let s = load_png_rw rwo in

  let w = Sdlsurface.get_width s in
  let h = Sdlsurface.get_height s in

  let temp = {T.gltid = None;
              T.surf = s;
              T.width = w;
              T.height = h;
              T.widthf = float_of_int w;
              T.heightf = float_of_int h;
              T.aspect = (float_of_int w) /. (float_of_int h)
             } in

  let tid = glGenTexture () in
  
  temp.T.gltid <- Some tid;

  glBindTexture ~target: BindTex.GL_TEXTURE_2D ~texture: tid;

  let (intmode, mode) =
    (if (Sdlsurface.get_bits_per_pixel s) = 32 then
        (InternalFormat.GL_RGBA, GL_RGBA) else 
        (InternalFormat.GL_RGB, GL_RGB)) in

  glTexImage2D
    ~target: TexTarget.GL_TEXTURE_2D
    ~level: 0
    ~internal_format: intmode
    ~width: temp.width
    ~height: temp.height
    ~format_: mode
    ~type_: GL_UNSIGNED_BYTE
    ~pixels: (Bigarray.genarray_of_array1 (Sdlsurface_ba.get_pixels s));

  glTexParameter
    ~target: TexParam.GL_TEXTURE_2D
    ~param: (TexParam.GL_TEXTURE_MIN_FILTER Min.GL_LINEAR);
  glTexParameter
    ~target: TexParam.GL_TEXTURE_2D
    ~param: (TexParam.GL_TEXTURE_MAG_FILTER Mag.GL_LINEAR);

  Hashtbl.replace textures f temp;
  temp

let bind_texture f =
  let t = try Hashtbl.find textures f with
      Not_found -> load_png f in
  let tid = Util.get_some t.gltid in
  glBindTexture BindTex.GL_TEXTURE_2D tid;
  t
