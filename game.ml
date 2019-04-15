module type Game = sig
  (**The abstract type representing game state*)
  type t

  (**The abstract type representing a parsed player input*)
  type command

  (**[next_state t c] is the game state after [c] is applied.*)
  val next_state : t -> command -> t

  (**[parse s] returns the [command] corresponding to [s]. The implementation
     depends on the game*)
  val parse: string -> command

  (**[print_player_state t id] is a string representing the perspective of the
      player corresponding to [id]*)
  val print_player_state: t -> int -> string

end
