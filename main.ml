
let rec play_game state =
  let input = read_line () in
  let command = parse input in
  let new_state = State.update command state in
  match new_state with 
  |Valid(v) -> play_game v
  |Invalid -> let () = print_endline ("Invalid Command") in play_game state










let main () =
  let state = State.init in
  play_game state
