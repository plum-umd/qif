open Printf
open Uigl
open Ipc
open Global
open Glutil

let process_events ~sender_to_master ~working =
  let has_events = ref (Sdlevent.has_event ()) in
  while !has_events do
    let e = Sdlevent.wait_event () in
    begin
      match e with
        | Sdlevent.KEYUP ke ->
          begin match ke.Sdlevent.keysym with
            | Sdlkey.KEY_ESCAPE -> 
              sender_to_master#send (UiAction (UiQuit true))
            | Sdlkey.KEY_f -> begin
              Glutil.toggle_fullscreen ();
            end
            | Sdlkey.KEY_1 -> Uigl.toggle_show_samples ()
            | Sdlkey.KEY_2 -> Uigl.toggle_show_abssamples ()
            | _ -> ()
          end
        | Sdlevent.VIDEORESIZE (w,h) -> begin
          Glutil.set_window_size w h
        end
        | Sdlevent.QUIT ->
          sender_to_master#send (UiAction (UiQuit true))
        | _ -> ()
    end;
    has_events := Sdlevent.has_event () 
  done
;;

let ui_loop ~receiver ~sender_to_master =
  let working = ref true in
  
  while !working do
    process_events ~sender_to_master: sender_to_master ~working: working;

    Uigl.render ();

    receiver#process (fun msg -> match msg with
      | UiCmds alist -> List.iter (fun acmd ->
        match acmd with
          | Lang.UiReset (numsamples, splitrate) ->
            Uigl.reset numsamples;
            sender_to_master#send (UiCleared true)

          | Lang.UiView (f1, f2, f3) ->
            set_view
              (Lang.string_of_exp f1)
              (Lang.string_of_exp f2)
              (Lang.string_of_exp f3);

            sender_to_master#send (UiViewChanged true)

          | Lang.UiExit -> sender_to_master#send (UiAction (UiQuit true))

      ) alist
        
      | UiAbsDumpReady adump ->
        add_abssample adump

      | UiDumpReady adump ->
        add_sample adump

      | UiClear _ ->
        clear_samples ();
        sender_to_master#send (UiCleared true)

      | UiShutdown _ -> working := false)
  done
;;

let run ~master_box_addr ~wakeup_sem_addr =
  let pid = Unix.getpid () in
  logf `Info "running gl process at pid %d" pid;

  let sender_to_master = Ipc.make_sender_ui_to_master
    ~master_box_addr: master_box_addr
    ~wakeup_sem_addr: wakeup_sem_addr in
  let receiver = new Ipc.receiver_ui in

  sender_to_master#send (UiBoxReady (receiver#get_addr ()));

  set Glutil.sender (fun l s -> sender_to_master#send (DebugLog (l, s)));
  set Glutil.init_handler Uigl.init;
  set Glutil.reinit_handler Uigl.reinit;

  Glutil.init ();
  
  ui_loop
    ~receiver: receiver
    ~sender_to_master: sender_to_master;

  receiver#close ();
  
  Glutil.finish ();

  logf `Info "gl process at pid %d finished" pid
;;
