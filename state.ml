open Player
open Command
type t = {players:Player.t list; phase:int; ship:int; turn:int}
type state_check = Invalid of string|Valid of t|Loss of string|Quit
let init = {players = (Player.init_player 8 8 1)::(Player.init_player 8 8 2)::[]; phase = 1; ship = 1; turn = 1}

let parse_check t = function
  |Continue(player) -> Valid({t with players = List.rev ( player::((List.nth t.players 1))::[])})
  |Loss(t) -> Loss(t)
let update t = function
  (*TODO: figure out how to determine which ship to place*)    
  |Place(start, dir) -> if t.phase = 1 then let new_player = 
                                            Player.insert_ship (List.hd t.players) (Player.make_coord start) (Player.make_coord start) t.ship in match new_player with
  					      |ValidB(new_player) -> if t.ship = 3 then Valid({t with phase = 2; players = List.rev (new_player::(List.nth (t.players) 1)::[])}) else if t.turn = 2 then
						Valid({t with ship = t.ship+1; players = List.rev (new_player::(List.nth (t.players) 1)::[]); turn = (t.turn+1)}) else Valid({t with turn = t.turn+1; players = List.rev (new_player::(List.nth (t.players) 1)::[])}) 
						
						|InvalidB(c) -> Invalid(c)
		 else Invalid("wrong phase")
  (*possibly add player indices to allow more than 2 players*)
  |Check(coordinate) ->  if t.phase = 2 then let new_player =
Player.check (List.nth t.players 1) coordinate in (if Player.already_guessed (List.nth t.players 1)  coordinate then Invalid("already guessed") else parse_check t new_player) else Invalid("wrong phase")
  |Quit -> Quit
  |Invalid -> Invalid("Invalid command")


