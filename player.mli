
type t
type player
type coord
type valid_board = ValidB of t | InvalidB of string
type game_over = Continue of t | Loss of string
val already_guessed: t->(int*int) -> bool
val init_player : int -> int -> int -> t
val insert_ship: t -> coord -> coord-> int -> valid_board
val check:  t -> (int*int) -> game_over
val make_coord: (int*int) -> coord
exception Diagonal_Ship
exception Out_of_Bounds
exception Invalid_Placement

