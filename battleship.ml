open Player
open Command
type t = {players:(Player.t * string) list; phase:int; ship:int; turn:int}
type command = Command.command
let init_state = {players = ((Player.init_player 8 8 1), "")::((Player.init_player 8 8 2), "")::[]; phase = 1; ship = 2; turn = 1}
let max_players = 2

let parse = Command.parse


let update_player_error t p_id np e_msg =
  let new_p = List.mapi (fun i (p, e) ->
      if i = p_id then (np, e_msg)
      else (p, e)) t.players in
  {t with players = new_p}

let update_error t p_id e_msg =
  let new_p = List.mapi (fun i (p, e) ->
      if i = p_id then (p, e ^ "\n" ^ e_msg)
      else (p, e)) t.players in
  {t with players = new_p}

let name = "CAMLSHIP"

let replace_error t p_id e_msg =
  let new_p = List.mapi (fun i (p, e) ->
      if i = p_id then (p, e_msg)
      else (p, e)) t.players in
  {t with players = new_p}

let update_player (players:(Player.t * string) list) p_id np =
  List.mapi (fun i (p, e) ->
      if i = p_id then (np, "")
      else (p, e)) players

let next_pid pid = if pid = 0 then 1 else 0

let next_turn x = if x = 1 then 2 else 1

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
              fun x -> replace_error x p_id msg
            | Loss(_) ->
              let t' = ((replace_error t p_id "you won!") |>
                        fun x -> replace_error x (next_pid p_id) "you lost!") in
              {t' with turn = -1}
        else update_error t p_id "wrong phase"
      end
    | Quit -> failwith "unimplemented"
    | Invalid -> update_error t p_id "invalid command!"

let print_player_state t p_id =
  let opp = (List.nth t.players (next_pid p_id) |> fst) in
  let (p,e) = (List.nth t.players p_id) in
  let my_board = Player.print_my_board p in
  let their_board = Player.print_opp_board opp in
  let turn = if t.turn - 1 = p_id then "it is your turn\n" else "it is not your turn\n" in
  "opponents board:\n" ^ their_board ^ "your board:\n" ^ my_board ^ turn ^ e
