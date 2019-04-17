open State
open Command
let rec play_game state =
  let input = read_line () in
  let command = Command.parse input in
  let new_state = State.update state command in
  match new_state with 
  |Valid(v) -> play_game v
  |Invalid(c) -> let () = print_endline (c) in play_game state
  |Loss(c) -> print_endline(c)
  |Quit -> print_endline("bye")










let main () =
  let () = print_endline 
      "\nWelcome to Battleship! \nEach player has three ships of size 2, 3 and 4. \nPlace ships with 'place [A-H][1-8] [A-H][1-8]', guess with 'check [A-H][1-8]' \nYou may view your board with \"print me\" or your opponent's with \"print opponent\" \nPlayer 1 begin by placing ship of size 2" in
  let state = State.init in
  play_game state


let () = main ()
