open State
open Command
let rec play_game state =
  print_endline ("test");
  let input = read_line () in
  let command = Command.parse input in
  let new_state = State.update state command in
  match new_state with 
  |Valid(v) -> play_game v
  |Invalid(c) -> let () = print_endline (c) in play_game state
  |Loss(c) -> print_endline(c)
  |Quit -> print_endline("bye")










let main () =
  let state = State.init in
  play_game state
