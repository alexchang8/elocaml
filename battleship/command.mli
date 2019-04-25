(**The command line parser for the battleship game*)

(**the type representing the different kinds of commands a player can
   execute.*)
type command =
  | Place of (int*int)*(int*int)
  | Check of (int*int)
  | PrintMe
  | PrintOpp
  | Invalid
  | Quit

(** [parse string] takes in a string and returns the corresponding command, based on the form of the string, if it is place A1 A2, it will be a place with those two coordinates
    if its print me it will be a print me, and so on. Anything else will return invalid.**)
val parse : string -> command

(**[check_string_form target] returns true if the string has the form of 1 set
    of coordinates, e. g.  "A1" **)
val check_string_form : string -> bool

(**[check_list_form t] returns true if the list has the form of either 1 or 2
   sets of coordinates, e. g. ["A1", "A2"]**)
val check_list_form : string list -> bool
