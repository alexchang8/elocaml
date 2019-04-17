open Player
open Command
type t
(**A type to wrap states to determine if the game has been lost or if the previous command was valid**)
type state_check = Invalid of string|Valid of t|Loss of string|Quit
(** [update t cmd] takes in a state and a command and modifies the state according to the command, or prints on a print statement. It will modify the boards
of players using insert ship on a place command and check on a check command**)

val update: t -> Command.command -> state_check

(**[init] returns a new state with 2 players with 8 by 8 boards, an initial ship size of 2 and the players first turn as 1**)
val init : t
