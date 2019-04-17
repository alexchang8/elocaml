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
<<<<<<< HEAD
  let () = print_endline 
      "\nWelcome to Battleship! \nEach player has three ships of size 2, 3 and 4. \nPlace ships by typing \"place\" followed by your ship's start and end coordinates.\nAfter placing all your ships, begin guessing by typing \"guess\" followed by a coordinate!\nYou may view your board with \"print me\" or your opponent's with \"print opponent\" " in
=======
  let () = print_endline "Welcome to Battleship! \n Each player has three ships of size 2, 3 and 4. \n Place ships and then start guessing! \n Place ships with 'place [A-H][1-8] [A-H][1-8]', guess with 'check [A-H][1-8]' \n Player 1 place ship of size 2" in
>>>>>>> 6aecb69c1dffb83012e5f144105713a8a483fdce
  let state = State.init in
  play_game state


let () = main ()
