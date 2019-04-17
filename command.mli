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
