(**The abstract signature representing any game*)
module type Game = sig
  (**The abstract type representing game state*)
  type t

  (**The abstract type representing a parsed player input*)
  type command

  (**[init_state] is an abstract value representing the initial state of a game*)
  val init_state : t

  (**[max_players] is the number of clients that must connect to the server
      for a game to start*)
  val max_players : int

  (**[next_state t p_id command] returns the abstract state of the game after
      a player with [p_id] inputs a string corresponding to [command].*)
  val next_state : t -> int -> command -> t

  (**[parse s] returns the [command] corresponding to [s]. The implementation
     depends on the game*)
  val parse: string -> command

  (**[print_player_state t id] is a string representing the perspective of the
      player corresponding to [id]*)
  val print_player_state: t -> int -> string

end
