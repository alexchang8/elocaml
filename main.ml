open state
let rec play_game state =
  let input = read_line () in
  let command = parse input in
  let new_state = State.update command state in
  match new_state with 
  |Valid(v) -> play_game v
  |Invalid(c) -> let () = print_endline (c) in play_game state
  |Loss(c) -> print_endline("state")
  |Quit -> print_endline("bye")










let main () =
  let state = State.init in
  play_game state
