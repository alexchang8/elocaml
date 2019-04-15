type command = 
  | Place of (int*int)*(int*int)
  | Check of (int*int)
  | Invalid
  | Quit
val parse : string -> command
