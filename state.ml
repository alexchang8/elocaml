
type t = {players:Player.t list; phase:int; continuing:bool}
type state_check = Invalid|Valid of t|Loss
let update t = function
  (*TODO: figure out how to determine which ship to place*)
  |Place(start, dir) -> if phase = 1 then let new_player = 
                                            Player.insert (List.hd t.players) start dir in {t with players = List.rev ( new_player::(List.rev (List.tl t.players))::[])} else Invalid
  (*possibly add player indices to allow more than 2 players*)
  |Check(coordinate) =  if phase = 2 then let new_player =
                                            Player.check (List.nth t.players target) start dir in parse_check t new_player else Invalid



let parse_check t = function
  |Continue(player) -> Valid({t with players = List.rev ( player::(List.rev (List.tl t.players))::[])})
  |Loss -> Loss

let check_continuing t = t.continuing
