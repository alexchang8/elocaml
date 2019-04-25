
type active_client = {in_game: bool; elo:int}
type status = LoggedOut | LoggedIn of active_client
type loginstage = EmptyForm | Username of string
type page = Login of loginstage | CreateAccount of loginstage | Lobby | CreateLobby
type lobbystatus = Live | Waiting of ((in_channel * out_channel * int * string) -> unit)
type lobby = {game:string; players: int; max_p: int; host: string;
              lobbystatus: lobbystatus; client_ids: int list}
type client_view = {oc: out_channel; mouse_cbs: mouse_cb list; overlay_string: string; username:string;
                    status: status; page: page; ocs: out_channel list; in_lobby: bool} and
  mouse_cb = int * int -> int * client_view -> t -> t option and
  t = {client_views: (int * client_view) list; lobbies: lobby list; dced: int list}

let (init_state:t) = {client_views = []; lobbies=[]; dced = []}

let get_dced t = t.dced

let remove_client_id t id =
  {t with client_views = List.remove_assoc id t.client_views; dced = id::t.dced}

let flush_dced t = {t with dced = []}

let account_template = Gui.draw_rect 15 8 64 26 ^ Gui.draw_rect 29 14 59 16
                       ^ Gui.draw_rect 29 17 59 19 ^
                       Gui.print_object_tl 18 15 "username:" ^
                       Gui.print_object_tl 18 18 "password:" ^
                       Gui.draw_rect 18 23 34 25

let login_template uname = account_template ^ Gui.print_object_tl 34 20 "(press enter to login)" ^
                           Gui.print_object_tl 39 12 "Login" ^
                           Gui.print_object_tl 30 15 uname ^
                           Gui.print_object_tl 19 24 "Create Account?"

let create_account uname = account_template  ^ Gui.print_object_tl 39 12 "Create account" ^
                           Gui.print_object_tl 34 20 "(press enter to create)" ^
                           Gui.print_object_tl 30 15 uname ^
                           Gui.print_object_tl 24 24 "Login?"

let create_lobby_view = Gui.draw_rect 15 8 64 26 ^
                        Gui.print_object_tl 32 12 "Create a Lobby" ^
                        Gui.draw_rect 18 23 33 25 ^
                        Gui.print_object_tl 22 24 "Cancel" ^
                        Gui.draw_rect 20 15 38 18 ^
                        Gui.print_object_tl 21 16 "    Go Lobby" ^
                        Gui.draw_rect 40 15 58 18 ^
                        Gui.print_object_tl 41 16 "Battleship Lobby"

let login_user_cursor = login_template "" ^ Gui.set_cursor 30 15

let update_client_view (cview:client_view) (id:int) cvlist =
  (id, cview)::(List.remove_assoc id cvlist)

let update_client t p_id new_client =
  {t with client_views = update_client_view new_client p_id t.client_views}

let create_cb ((x,y):int*int) x1 x2 y1 y2 cv mouse_cbs overlay_string page t id =
  if x >= x1 && x<= x2 && y>=y1 && y<= y2 then
    let cv' = {cv with mouse_cbs = mouse_cbs; overlay_string = overlay_string;
                       page = page} in
    Some(update_client t id cv')
  else None

module GoServer = Integrated_server.MakeServer(Go)
module BshipServer = Integrated_server.MakeServer(Battleship)

let start_go_server (x,y) (id,cv) t =
  if x>=20 && x<=38 && y >=15 && y<=18 then begin
    print_endline "starting go server!";
    let ic1,oc1 =
      let f1,f2 =  Unix.pipe () in
      (Unix.in_channel_of_descr f1, Unix.out_channel_of_descr f2) in
    (**TODO: we need to update lobbies also*)
    let lobbystatus = Waiting(GoServer.run (ic1, cv.oc, 0, cv.username)) in
    let lobby = {game="Go"; players = 1; max_p = 2; host = cv.username;
                 lobbystatus = lobbystatus; client_ids = [id]} in
    (**TODO: should probably add a way to wait/cancel the lobby*)
    let new_cv = {cv with mouse_cbs = []; overlay_string = ""; in_lobby = true;
                          page = Lobby; ocs = oc1::cv.ocs} in
    Some(update_client t id new_cv |> fun st -> {st with lobbies = lobby::List.rev st.lobbies |> List.rev})
  end
  else None

let start_bship_server (x,y) (id,cv) t =
  if x>=40 && x<=58 && y >=15 && y<=18 then begin
    print_endline "starting battleship server!";
    let ic1,oc1 =
      let f1,f2 =  Unix.pipe () in
      (Unix.in_channel_of_descr f1, Unix.out_channel_of_descr f2) in
    (**TODO: we need to update lobbies also*)
    let lobbystatus = Waiting(BshipServer.run (ic1, cv.oc, 0, cv.username)) in
    let lobby = {game="Bship"; players = 1; max_p = 2; host = cv.username;
                 lobbystatus = lobbystatus; client_ids = [id]} in
    (**TODO: should probably add a way to wait/cancel the lobby*)
    let new_cv = {cv with mouse_cbs = []; overlay_string = ""; in_lobby = true;
                          page = Lobby; ocs = oc1::cv.ocs} in
    Some(update_client t id new_cv |> fun st -> {st with lobbies = lobby::List.rev st.lobbies |> List.rev})
  end
  else None

let pending_lobbies = List.filter (fun lob ->
    match lob.lobbystatus with
    | Live -> false
    | Waiting(_) -> true)

let lobby_start_cb (x,y) (id,cv) t =
  let n = (y - 11)/2 in
  let avail_lobbies = pending_lobbies t.lobbies in
  if y mod 2 = 1 && n >=0 && n < (List.length avail_lobbies) && x >= 4 && x <= 19 then
    begin
      print_endline "server is starting!";
      let ic2,oc2 =
        let f1,f2 =  Unix.pipe () in
        (Unix.in_channel_of_descr f1, Unix.out_channel_of_descr f2) in
      let lobby = List.nth avail_lobbies n in
      match Unix.fork () with
      | 0 -> begin
          match lobby.lobbystatus with
          | Live -> failwith "rep invariant failure"
          | Waiting (run) ->
            run (ic2, cv.oc, 1, cv.username);
            failwith "child process should die"
        end
      | child_id ->
        let new_cv = {cv with mouse_cbs = []; overlay_string = ""; in_lobby = false;
                              page = Lobby; ocs = oc2::cv.ocs} in
        let id2 = List.hd lobby.client_ids in
        let new_cv2 = match List.assoc_opt id2 t.client_views with
          | None -> failwith "unimplemented" (*trying to connect to a lobby where
                                               one player dced *)
          | Some cv2 -> {cv2 with mouse_cbs=[]; overlay_string = ""; in_lobby = false;
                                  page = Lobby} in
        let new_lobby = {lobby with players=2; lobbystatus = Live; client_ids = id::lobby.client_ids} in
        let new_lobbies = (List.fold_left (fun (acc, z) lo -> match lo.lobbystatus with
            | Live -> (lo::acc, z)
            | Waiting(_) when z = n -> (new_lobby::acc, z+1)
            | Waiting(_) -> (lo::acc, z+1)) ([], 0) t.lobbies)
                          |> fst |> List.rev in
        let removed = List.remove_assoc id t.client_views |> List.remove_assoc id2 in
        Some({t with client_views = (id,new_cv) :: (id2, new_cv2) :: removed; lobbies = new_lobbies})
    end
  else None

let rec create_lobby_cb coords (id,cv) t =
  create_cb coords 5 17 1 1 cv [cancel_lobby_cb;start_go_server;start_bship_server] create_lobby_view (CreateLobby) t id
and cancel_lobby_cb coords (id, cv) t =
  create_cb coords 18 33 23 25 cv [create_lobby_cb] "" (Lobby) t id

let rec login_cb coords (id,cv) t =
  create_cb coords 18 34 23 25 cv [create_account_cb] login_user_cursor
    (Login(EmptyForm)) t id
and create_account_cb coords (id,cv) t =
  create_cb coords 18 34 23 25 cv [login_cb] (create_account "" ^ Gui.set_cursor 30 15)
    (CreateAccount(EmptyForm)) t id


let new_client (id:int) oc =
  (id, {oc = oc; mouse_cbs = [create_account_cb]; overlay_string = login_user_cursor ; in_lobby = true;
        username = ""; status = LoggedOut; page = Login(EmptyForm); ocs = []})

let rec resolve_mouse_cbs (id,cv) state (mouse_cbs:mouse_cb list) coords =
  match mouse_cbs with
  | h::t -> begin
      match h coords (id,cv) state with
      | None -> resolve_mouse_cbs (id,cv) state t coords
      | Some state' -> state'
    end
  | [] -> state

let update_page_string t cview p_id p str =
  let client' = {cview with page = p; overlay_string = str} in
  update_client t p_id client'

let next_state t p_id oc str =
  (*print_endline str;*)
  match List.assoc_opt p_id t.client_views with
  | None -> {t with client_views = (new_client p_id oc :: t.client_views)}
  | Some client_view ->
    (**TODO: forward to all lobbies the client is connected to*)
    List.iter (fun game_oc -> output_string game_oc (str^"\n") ; flush game_oc) client_view.ocs;
    match Tools.is_mouse_click str with
    | None -> begin
        let str = Tools.remove_mouse str in
        match client_view.page with
        | Login(EmptyForm) ->
          update_page_string t client_view p_id (Login(Username(str)))
            (login_template str ^ Gui.set_cursor 30 18)
        | Login(Username(uname)) -> begin
            match User.login uname str "battleship.json" with
            | ValidUser(u) ->
              let status = LoggedIn({in_game = false; elo = User.get_elo u}) in
              let client' = {client_view with username = User.get_username u;
                                              status = status; mouse_cbs = [create_lobby_cb; lobby_start_cb]} in
              update_page_string t client' p_id Lobby ""
            | InvalidUser(_) ->
              update_page_string t client_view p_id (Login(EmptyForm))
                (login_template "" ^ Gui.print_object_tl 34 21 "Invalid login!"
                 ^ Gui.set_cursor 30 15)
          end
        | CreateAccount(EmptyForm) ->
          if User.valid_name str "battleship.json" then
            begin
              update_page_string t client_view p_id (CreateAccount(Username(str)))
                (create_account str ^ Gui.set_cursor 30 18)
            end
          else
            begin
              update_page_string t client_view p_id (CreateAccount(EmptyForm))
                (client_view.overlay_string ^ Gui.print_object_tl 30 13 "username taken!"
                 ^ Gui.set_cursor 30 15)
            end
        | CreateAccount(Username(uname)) ->
          User.create_user uname str "battleship.json";
          update_page_string t client_view p_id (Login(EmptyForm)) (login_template "" ^ Gui.set_cursor 30 15)
        | Lobby -> t
        | CreateLobby -> t
      end
    | Some (x,y) ->
      let ordered = List.rev client_view.ocs in
      if y = 1 then
        let n = if x >= 27 && x <= 32 then -1
          else if x >= 34 && x <= 40 then 0
          else if x >= 42 && x <= 48 then 1
          else if x >= 50 && x <= 56 then 2
          else -2 in
        if n = -2 then resolve_mouse_cbs (p_id, client_view) t client_view.mouse_cbs (x,y)
        else if n = -1 then begin
          List.iter (fun game_oc -> output_string game_oc "\n__DISABLE__\n"; flush game_oc) client_view.ocs;
          let new_client = {client_view with in_lobby = true; overlay_string = "";
                                             mouse_cbs = [create_lobby_cb; lobby_start_cb]} in
          update_client t p_id new_client
        end
        else if n >= 0 && n < List.length ordered then
          let active_oc = List.nth ordered n in
          List.iter (fun game_oc -> output_string game_oc "\n__DISABLE__\n"; flush game_oc) client_view.ocs;
          output_string active_oc "\n__ENABLE__\n";
          flush active_oc;
          let new_client = {client_view with in_lobby = false; overlay_string = "";
                                             mouse_cbs = []} in
          update_client t p_id new_client
        else
          resolve_mouse_cbs (p_id, client_view) t client_view.mouse_cbs (x,y)

      else
        resolve_mouse_cbs (p_id, client_view) t client_view.mouse_cbs (x,y)

let lobbystrings lst =
  (**todo: format host name nicely*)
  pending_lobbies lst |>
  List.map (fun lobby ->
      let play = match lobby.lobbystatus with
        | Live -> "        "
        | Waiting(_) -> "[play!]" in
      "   " ^ lobby.game ^ play ^ "                        " ^ lobby.host ^
      "                           "
      ^ string_of_int lobby.players ^ " of 2") |>
  String.concat "\n╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌\n"

let print_player_state p_id t =
  match List.assoc_opt p_id t.client_views with
  | None -> failwith "rep invariant violated"
  | Some cv ->
    (**TODO: print lobbies you are in and llbbies you could join*)
    if cv.in_lobby then begin
      "   |Create Lobby |       | Home | Tab 1 | Tab 2 | Tab 3 |        user: " ^ cv.username ^ "\n\n" ^
      Gui.centered ^ "\n\n" ^ "    Game                            Host                          Players         "
      ^ "\n────────────────────────────────────────────────────────────────────────────────\n"
      ^ lobbystrings t.lobbies
      ^ cv.overlay_string
    end
    else begin
      ""
    end
