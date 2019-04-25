(**The module that holds logic for a single player in battleship*)
type cell = Empty|Ship|Miss|Hit|Sunk
type t
type player
type ship
type coord
type board=cell list list
type valid_board = ValidB of t | InvalidB of string
type game_over = Continue of t * string | Loss of string

(**[print_my_board t] returns a string corresponding to a complete view
   of [t]'s board*)
val print_my_board: t -> string

(**[print_my_board t] returns a string corresponding to a view of [t]'s board
   without ships*)
val print_opp_board: t -> string

(** [already_guessed board (c1, c2)] is the function that determines if cell
    [(c1, c2)] has previously been guessed by a user. A cell has not been
    guessed if its value is Empty or a Ship. *)
val already_guessed: t->(int*int) -> bool

(** [init_player x y name] initializes an new player containing an empty board
    of size ([x], [y]), an empty ship array, and the board's shape ([x], [y]).
*)
val init_player : int -> int -> int -> t

(** [insert_ship player start_cord end_cord size] inserts a ship given the
    starting and ending coordinates of the ship ([start_cord] and [end_cord])
    to the current players board and returns an updated player. If the [size]
    does not match the number of coordinates of the ship, then an InavlidB
    is returned. *)
val insert_ship: t -> coord -> coord-> int -> valid_board

(** [check p (c1, c2)] returns the new player with the coordinate (c1, c2)
    updated to represent the player's guess and the current player [p].
    Requires: Cell (c1, c2) has not previously been guessed
    Returns the option of continue or loss *)
val check:  t -> (int*int) -> game_over

(** [make_coord (a,b)] boxes the coordinate (a,b) into a coord of the same
    coordinates. *)
val make_coord: (int*int) -> coord


(* val match_coord: coord ->(int*int) *)

(** [get_name p] returns the name of the current player [p]. *)
val get_name: t -> int

(** [get_ships p] returns the ships of the current player [p]. *)
val get_ships: t -> ship list

(** [get_shape p] returns the shape of the current player's [p] board. *)
val get_shape: t -> (int*int)

(** [get_board p] returns the board of the current player [p]. *)
val get_board: t -> board
(**exception thrown when a player tries to place a diagonal ship*)
exception Diagonal_Ship
(**exception thrown when a player tries to check something out of bounds*)
exception Out_of_Bounds
(**exception thrown when a player tries to place a ship on top of another ship*)
exception Invalid_Placement
(**exception thrown when a player plays a ship of the wrong size*)
exception Inva_Order
