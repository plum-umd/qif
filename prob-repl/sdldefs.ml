module W = struct
  type window = {mutable width: int;
                 mutable height: int;
                 mutable win: Sdlwindow.t;
                 mutable widthf: float;
                 mutable heightf: float;
                 mutable aspect: float}
end;;

module T = struct
  type tex = {mutable gltid: GL.texture_id option;
              mutable surf: Sdlsurface.t;
              mutable width: int;
              mutable height: int;
              mutable widthf: float;
              mutable heightf: float;
              mutable aspect: float}
end;;
