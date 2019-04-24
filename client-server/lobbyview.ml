
type active_client = {in_game: bool; elo:int}
type status = LoggedOut | LoggedIn of active_client
type loginstage = EmptyForm | Username of string
type page = Login of loginstage | CreateAccount of loginstage | Lobby
type client_view = {mouse_cbs: mouse_cb list; overlay_string: string; in_lobby: bool; username:string;
                    status: status; page: page} and
  mouse_cb = int * int -> client_view -> client_view option
type lobby = {players: int; max_p: int; host: string}
type t = {client_views: (int * client_view) list; lobbies: lobby list}

let (init_state:t) = {client_views = []; lobbies=[]}


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

let login_user_cursor = login_template "" ^ Gui.set_cursor 30 15

let update_client_view (cview:client_view) (id:int) cvlist =
  (id, cview)::(List.remove_assoc id cvlist)

let create_cb (x,y) x1 x2 y1 y2 cv mouse_cbs overlay_string page =
  if x >= x1 && x<= x2 && y>=y1 && y<= y2 then
    Some({cv with mouse_cbs = mouse_cbs; overlay_string = overlay_string;
                  page = page})
  else None

let rec login_cb coords cv = create_cb coords 18 34 23 25 cv [create_account_cb]
    login_user_cursor (Login(EmptyForm)) and

  create_account_cb coords cv = create_cb coords 18 34 23 25 cv [login_cb]
                                             (create_account "" ^ Gui.set_cursor 30 15)
                                             (CreateAccount(EmptyForm))

let new_client (id:int) =
  (id, {mouse_cbs = [create_account_cb]; overlay_string = login_user_cursor ; in_lobby = true;
        username = ""; status = LoggedOut; page = Login(EmptyForm)})

let rec resolve_mouse_cbs (cv:client_view) (mouse_cbs:mouse_cb list) coords =
  match mouse_cbs with
  | h::t -> begin
      match h coords cv with
      | None -> resolve_mouse_cbs cv t coords
      | Some cv' -> cv'
    end
  | [] -> cv

let update_client t p_id new_client =
  {t with client_views = update_client_view new_client p_id t.client_views}

let update_page_string t cview p_id p str =
  let client' = {cview with page = p; overlay_string = str} in
  update_client t p_id client'


let next_state t p_id str =
  match List.assoc_opt p_id t.client_views with
  | None -> {t with client_views = (new_client p_id :: t.client_views)}
  | Some client_view ->
    print_endline str;
    match Tools.is_mouse_click str with
    | None -> begin
        match client_view.page with
        | Login(EmptyForm) ->
          update_page_string t client_view p_id (Login(Username(str)))
            (login_template str ^ Gui.set_cursor 30 18)
        | Login(Username(uname)) -> begin
            match User.login uname str "battleship.json" with
            | ValidUser(u) ->
              let status = LoggedIn({in_game = false; elo = User.get_elo u}) in
              let client' = {client_view with username = User.get_username u;
                                              status = status} in
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
      end
    | Some coords ->
      let client' = resolve_mouse_cbs client_view client_view.mouse_cbs coords in
      update_client t p_id client'


let print_player_state p_id t =
  match List.assoc_opt p_id t.client_views with
  | None -> failwith "rep invariant violated"
  | Some cv ->
    (**TODO: print lobbies you are in and llbbies you could join*)
    if cv.in_lobby then begin
      " ⏻ |Create Lobby |                  | Ladder |          user: " ^ cv.username ^ "\n\n" ^
      Gui.centered ^ "\n\n" ^ "    Host                            Players                     Spectators"
      ^ "\n────────────────────────────────────────────────────────────────────────────────\n"
      ^ cv.overlay_string
    end
    else begin
      ""
    end

(**need to do stuff with mouse callbacks *)
(**TODO: figure out communication protocols between client and
   server.
   The client will: receive some string that tells it to connect to an ip.
   Instead of just swapping the connections, it will maintain connections to all
   servers it is connected to. Everything typed is broadcast to all all servers concurrently;
   the servers keep track of whether or not they should respond.

   The server will: when a lobby is created, start a new process. Then tell
   the client to connect to that server

*)
