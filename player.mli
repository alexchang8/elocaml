type t
type player
type valid_board = ValidB of t | InvalidB of string
type game_over = Continue of t | Loss of string
val init_player : int -> int -> int -> t
val insert_ship: t -> (int*int) -> (int*int) -> int -> valid_board
val check:  t -> (int*int) -> game_over

