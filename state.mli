open Player
open Command
type t
type state_check = Invalid of string|Valid of t|Loss of string|Quit
val update: t -> Command.command -> state_check
val init : t
