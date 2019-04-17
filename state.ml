open Player
open Command
type t = {players:Player.t list; phase:int; ship:int; turn:int}
type state_check = Invalid of string|Valid of t|Loss of string|Quit
let init = {players = (Player.init_player 8 8 1)::(Player.init_player 8 8 2)::[]; phase = 1; ship = 2; turn = 1}

let parse_check t = function
  |Continue(player) -> Valid({t with players = player::(List.nth t.players 0)::[]})
  |Loss(t) -> Loss(t)
let update t = function
  (*TODO: figure out how to determine which ship to place*) 
  (*to change for >2 players*)
  |PrintMe -> let () = print_my_board (List.hd t.players) in Valid(t)
  |PrintOpp -> let () = print_opp_board (List.nth t.players 1) in Valid(t)
  |Place(start, dir) -> if t.phase = 1 then let new_player = 
                                              try Player.insert_ship (List.hd t.players) (Player.make_coord start) (Player.make_coord dir) t.ship with 
                                              | Invalid_Placement -> InvalidB("Can't place there")
                                              | Diagonal_Ship ->InvalidB("Cannot place diagonal ships")
                                              | Out_of_Bounds ->InvalidB("out of board bounds")

      in match new_player with                                              
|ValidB(new_player) -> let () = Player.print_my_board new_player in if t.turn=2 && t.ship = 4 then Valid({t with phase = 2; players = List.rev (new_player::(List.nth (t.players) 1)::[])}) else if t.turn = 2 then
          Valid({t with ship = t.ship+1; players = List.rev (new_player::(List.nth (t.players) 1)::[]); turn = (1)}) else Valid({t with turn = 2; players = List.rev (new_player::(List.nth (t.players) 1)::[])}) 


      |InvalidB(c) -> Invalid(c)
    else Invalid("wrong phase")
  (*possibly add player indices to allow more than 2 players*)
  |Check(coordinate) ->  if t.phase = 2 then if Player.already_guessed (List.nth t.players 1)  coordinate then Invalid("already guessed") else let new_player =
                                               Player.check (List.nth t.players 1) coordinate in (parse_check t new_player) else Invalid("wrong phase")
  |Quit -> Quit
  |Invalid -> Invalid("Invalid command")


