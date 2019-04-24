type active_client = {username: string; in_game: bool}
type status = LoggedOut | LoggedIn of active_client
type client_view = {mouse_cbs: mouse_cb list; overlay_string: string; in_lobby: bool; username:string;
                    status: status} and
  mouse_cb = int * int -> client_view option
type lobby = {players: int; max_p: int; host: string}
type t = {client_views: (int * client_view) list; lobbies: lobby list}




let (init_state:t) = {client_views = []; lobbies=[]}

let new_client (id:int) = (id,
                           {mouse_cbs = []; overlay_string = ""; in_lobby = true;
                            username = ""; status = LoggedOut})

let rec resolve_mouse_cbs (cv:client_view) (mouse_cbs:mouse_cb list) coords =
  match mouse_cbs with
  | h::t -> begin
      match h coords with
      | None -> resolve_mouse_cbs cv t coords
      | Some cv' -> cv'
    end
  | [] -> cv

let rec next_state t (p_id,status) str =
  match List.assoc_opt p_id t.client_views with
  | None ->
    let t' = {t with client_views = (new_client p_id :: t.client_views)} in
    next_state t' (p_id,status) str
  | Some client_view -> t(*failwith "unimplemented"*)

let print_player_state p_id t =
  match List.assoc_opt p_id t.client_views with
  | None -> failwith "rep invariant violated"
  | Some cv ->
    (**TODO: print lobbies you are in and lobbies you could join*)
    if cv.in_lobby then
      " ⏻ |Create Lobby |                  | Ladder |        " ^ cv.username ^ "\n\n" ^
      Gui.centered ^ "\n\n" ^ "    Host                            Players                     Spectators"
      ^ "\n────────────────────────────────────────────────────────────────────────────────\n"
      ^ cv.overlay_string
    else ""

let _ =
  next_state init_state (1, LoggedOut) "a string" |> print_player_state 1 |> print_endline
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
