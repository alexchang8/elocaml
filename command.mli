type command = 
  | Place of (int*int)*(int*int)
  | Check of (int*int)
  | PrintMe
  | PrintOpp
  | Invalid
  | Quit
val parse : string -> command
