type t
type player
type valid_board
val init_player : int -> int -> int -> t
val insert_ship: player -> (int*int) -> (int*int) -> int -> valid_board
