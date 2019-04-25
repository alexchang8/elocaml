(**The main module for the battleship game logic*)
open Player
open Command
(**
   The abstract type representing the state of the game
   players: An association list
   holding a player and string, where the string will be printed
   as a message to the player when they request a game state
   phase: The phase of the game. 1 is the "placing" phase, and 2 is the
   guessing phase
   ship: the ship size to be placed when in phase 1
   turn: the p_id of the active player*)
type t = {players:(Player.t * string) list; phase:int; ship:int; turn:int}
(**The abstract type representing the command type*)
type command = Command.command
(**The initial state of the board.*)
let init_state = {players = ((Player.init_player 8 8 1), "")::((Player.init_player 8 8 2), "")::[]; phase = 1; ship = 2; turn = 1}

(**Only 2 players are currently supported. This is needed for the standalone
   server, which blocks until there are this many connections.*)
let max_players = 2

(**[parse s] is the same as [Command.parse s]. This is so the function is
   wrapped in the game signature.*)
let parse = Command.parse

(**The terminal size that should be set so the game prints properly*)
let terminal_size = 80, 52

(**[update_player_error t p_id np e_msg] returns [t], with [t.players] changed
   so that the player corresponding to [p_id] is updated to be (np, e_msg).
*)
let update_player_error t p_id np e_msg =
  let new_p = List.mapi (fun i (p, e) ->
      if i = p_id then (np, e_msg)
      else (p, e)) t.players in
  {t with players = new_p}

(**[update_player_error t p_id e_msg] returns [t], except changed
   so that the [e_msg] will print at the end of the game state.*)
let update_error t p_id e_msg =
  let new_p = List.mapi (fun i (p, e) ->
      if i = p_id then (p, "\n" ^ e_msg)
      else (p, e)) t.players in
  {t with players = new_p}

(**String for the name of the game to be printed*)
let name = "CAMLSHIP"

(**[update_player players p_id np] returns players, with the
   value correpsonding to the key [p_id] is changed to be [np]*)
let update_player (players:(Player.t * string) list) p_id np =
  List.mapi (fun i (p, e) ->
      if i = p_id then (np, "")
      else (p, e)) players

(**[next_pid pid] returns 0 if [pid] is 1 and 0 otherwise*)
let next_pid pid = if pid = 0 then 1 else 0

(**[next_turn x] returns 1 if [x] is 1 and 2 otherwise*)
let next_turn x = if x = 1 then 2 else 1

(**[next_state t p_id command] returns the new state of the board
   after [command] is executed by [p_id]*)
let next_state t p_id command =
  if t.turn - 1 <> p_id then
    update_error t p_id "not your turn!"
  else
    match command with
    (**TODO: want to constantly print boards so these should be removed *)
    | PrintMe | PrintOpp -> failwith "unimplemented"
    | Place(start, dir) ->
      begin
        if t.phase = 1 then
          let try_player =
            try Player.insert_ship (List.nth t.players p_id |> fst)
                  (Player.make_coord start) (Player.make_coord dir) t.ship with
            | Invalid_Placement -> InvalidB("Can't place there")
            | Diagonal_Ship ->InvalidB("Cannot place diagonal ships")
            | Out_of_Bounds ->InvalidB("out of board bounds") in
          match try_player with
          | ValidB (np) ->
            let new_players = update_player t.players p_id np in
            if t.turn = 2 && t.ship = 4 then
              {t with phase = 2; players = new_players}
            else if t.turn = 2 then
              {t with ship = t.ship + 1; turn = 1; players = new_players}
            else
              {t with turn = 2; players = new_players}
          | InvalidB(msg) -> update_error t p_id msg
        else update_error t p_id "wrong phase"
      end
    | Check(coordinate) -> begin
        if t.phase = 2 then
          let opp = next_pid p_id in
          let p = (opp |> List.nth t.players |> fst) in
          if Player.already_guessed p
              coordinate then
            update_error t p_id "already guessed"
          else
            match Player.check p coordinate with
            | Continue (new_p, msg) ->
              let np = update_player t.players opp new_p in
              {t with players = np; turn = next_turn t.turn} |>
              fun x -> update_error x p_id msg
            | Loss(_) ->
              let t' = ((update_error t p_id "you won!") |>
                        fun x -> update_error x (next_pid p_id) "you lost!") in
              {t' with turn = -1}
        else update_error t p_id "wrong phase"
      end
    | Quit -> failwith "unimplemented"
    | Invalid -> update_error t p_id "invalid command!"

(**[print_player_state t p_id] returns a string representing the complete view
   of the player [p_id].*)
let print_player_state t p_id =
  let opp = (List.nth t.players (next_pid p_id) |> fst) in
  let (p,e) = (List.nth t.players p_id) in
  let my_board = Player.print_my_board p in
  let their_board = Player.print_opp_board opp in
  let turn = if t.turn - 1 = p_id then "it is your turn\n" else "it is not your turn\n" in
  "opponents board:\n" ^ their_board ^ "your board:\n" ^ my_board ^ turn ^ e
