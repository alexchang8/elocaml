
type cell = Empty|Ship|Miss|Hit|Sunk
type board = cell list list
type coord = Hit | Coord of int * int
type ship = {sunk :bool ; size:int; inserted:bool; coords: coord list}
(* type exc = Valid | Invalid  *)

(* maybe wrap in valid | invalid  *)
exception Diagonal_Ship
exception Out_of_Bounds
exception Invalid_Placement


(** RI: Board shape can be no larger than 26 by 26 *)
type player = {
  board : board;
  ships : ship list;
  shape : int * int;
  name : int
}

type t = player

type valid_board = Valid of t | Invalid of string
type game_over = Continue of t | Loss

(** [alphabet] is the alphabet allowed for the y axis coordinates of the 
    game board. *)
let alphabet = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"

(** [index c] is the 0-based index of [c] in the alphabet.
    Requires: [c] is an uppercase letter in A..Z. *)
let index (c:char) : int =
  String.index alphabet c

(** [inv_index n] takes an index value and returns the corresponding letter
    in the alphabet.
    Requires: [n] is an int between 0-25. *)
let inv_index (n:int) : char = 
  String.get alphabet n

(** [set_inner acc y] is a helper function for initializing the empty board. 
    It tail recursively computes a list of Empty cells of length [y]. *)
let rec set_inner (acc:cell list) (y:int) = 
  if y > 0 
  then set_inner (Empty::acc) (y-1)
  else acc

(** [init_empty_board x y] is the function that initializes an empty board of
    shape ([x], [y]). An empty game board is a 2D array of all Empty cells.
    Requires: 0 < x < 26, 0 < y < 26 *)
let init_empty_board x y =
  let rec set_outer acc x y = 
    if x > 0
    then let new_arr = set_inner [] y in 
      set_outer (new_arr::acc) (x-1) y
    else acc
  in set_outer [] x y

(** [init_shape x y] initializes the shape of the game board given a row
    length of [x] and column length of [y]. *)
let init_shape x y = 
  (x, y)

(** [init_player x y name] initializes an new player containing an empty board 
    of size ([x], [y]), an empty ship array, and the board's shape ([x], [y]). 
*)
let init_player x y name : t = 
  {
    board = init_empty_board x y;
    ships = [];
    shape = init_shape x y;
    name = name
  }

(** [add_ship ships coords] adds a new ship of coordinates [coords] to the 
    list of current ships for the player: [ships].
    Requires: [coords] is a valid list of coordinates given the current
    game board. *)
let add_ship ships coords = 
  let new_ship = 
    {
      sunk = false;
      size = List.length coords;
      inserted = true;
      coords = coords
    } in
  new_ship::ships

(** [empty_cell c] is the boolean function for whether a given cell [c] is 
    Empty. *)
let empty_cell c = c = Empty

(** [add_ship_to_board board coords] adds a ship of coordinates [coords] 
    to the current game board [board] and returns a new game board. 
    Requires: [coords] is a valid list of coordinates given the current
    game board [board]. *)
let add_ship_to_board board coords = 
  let rec inner_loop acc row_pos col_pos row =
    match row with
    | [] -> acc
    | h::t -> 
      begin
        if List.mem (Coord (row_pos, col_pos)) coords
        then begin 
          if not (empty_cell h) 
          then raise Invalid_Placement (* TODO Add real exception *)
          else inner_loop (Ship::acc) row_pos (col_pos+1) t
        end
        else inner_loop (h::acc) row_pos (col_pos+1) t
      end in
  let rec outer_loop acc row_pos b =
    match b with
    | [] -> acc
    | h::t -> begin 
        outer_loop ((List.rev (inner_loop [] row_pos 1 h))::acc) (row_pos+1) t
      end
  in List.rev (outer_loop [] 1 board)

let match_coord c = 
  match c with 
  |Hit-> failwith "RI doesn't hold"
  |Coord (x,y) -> (x,y)

(* Given start coord and end coord, output a coord-list **)
let get_all_cords (start_cord:coord) (end_cord:coord) : coord list  = (*TODO **)  [Coord (1,1); Coord (1,2); Coord (1,3)] 
(*
  let my_coords = start_cord::[] in
    (* checks for ship > 25 pieces**)
    if 
    (start_cord |> match_coord |> fst > 25) || 
    (start_cord |> match_coord |> snd > 25) ||
    (end_cord |> match_coord |> fst > 25) || 
    (end_cord |> match_coord |> snd > 25)
    then raise Out_of_Bounds
    (* TODO: check for a ship larger than length **)
    else match (match_coord start_cord), (match_coord end_cord) with 
      | (a1, b1), (a2, b2) when a1=a2 -> 
        (* while b2 > b2 
          add (a, b2) to acc
          b2 = b2-1  *)         
      | (a1, b1), (a2, b2) when b1=b2-> 
      | (_,_), (_, _) -> raise Diagonal_Ship
      **)


(* let my_coords = start_cord::[] in  *)

let exn_catcher exn = 
  match exn with 
  |Diagonal_Ship -> "Cannot place diagonal ship" 
  |Out_of_Bounds -> "Ship must be within bounds"
  |Invalid_Placement -> "Cannot place ships on top of each other"

(** [insert_ship player start_cord end_cord] inserts a ship given the starting
    and ending coordinates of the ship ([start_cord] and [end_cord]) to the
    current players board and returns an updated player. *)
let insert_ship (player:t) start_cord end_cord size = 
  let cord_list = get_all_cords start_cord end_cord in
  if List.length cord_list <> size 
  then Invalid "Invalid size"
  else
    let new_ships = add_ship player.ships cord_list in
    let new_board = add_ship_to_board player.board cord_list in
    Valid {
      board=new_board;
      ships=new_ships;
      shape=player.shape;
      name=player.name
    }

(** [cell_spacing] is a constant representing the space between cells on a
    gameboard. Incorporated to display the game to the user. *)
let cell_spacing = " | "

(** [get_dash_space] computes the string of all white space the same size
    as [cell_spacing]. *)
let get_dash_space () = 
  let rec tr_helper acc s = 
    if s > 0
    then tr_helper (" " ^ acc) (s-1)
    else acc
  in tr_helper "" (String.length cell_spacing)

(** [dash_spacing] is a constant representing the space between cells on a
    gameboard. The space is the same size as cell_spacing except all white 
    space. Incorporated to display the game to the user. *)
let dash_space = get_dash_space ()

(** [parse_cell c verbose] gives the string representation of the cell [c]. 
    If [verbose] is true then ships are displayed. Otherwise, they are skipped
    over and given the empty space. *)
let parse_cell c verbose = 
  match c with
  | Empty -> " "
  | Ship -> if verbose then "S" else " "
  | Miss -> "0"
  | Hit ->  "X"
  | Sunk -> "Z"

(** [get_row row_acc row verbose] computes the string representation of [row]. 
    Prepends the current row label to the row and spaces each cell by
    the [cell_spacing] variable. If [verbose] then the players ships will 
    also be displayed. *)
let get_row (row_acc:int) (row:cell list) verbose = 
  let rec tr_helper acc r = 
    match r with 
    | [] -> acc
    | h::t -> tr_helper (acc ^ (parse_cell h verbose ^ cell_spacing)) t
  in tr_helper ((row_acc |> inv_index |> Char.escaped) ^ cell_spacing) row

(** [print_row row_acc row verbose] prints the string representation of [row] 
    to the console. If [verbose] then the players ships will also be displayed. 
*)
let print_row row_acc row verbose = get_row row_acc row verbose |> print_endline

(** [print_dash row_size] prints a row of dashes seperated by a [dash_space]. *)
let rec print_dash row_size = 
  if row_size > 0
  then begin print_string ("-" ^ dash_space); print_dash (row_size - 1) end
  else print_endline ""

(** [print_top_labels s] prints the x-axis coordinates of the game board 
    seperated by [cell_spacing]. Begins with value of 1 and continues for
    [s] coordinate labels. *)
let print_top_labels s = 
  let () = print_string " " in (* Use as an offset for the row labels *)
  let rec helper acc size = 
    if size > 0
    then begin 
      cell_spacing ^ (string_of_int acc) |> print_string;
      helper (acc+1) (size-1)
    end
    else print_endline cell_spacing
  in helper 1 s

let temp_player = init_player 5 5

(** [print_board player verbose] prints the board of the current [player]. If 
    [verbose] then the players ships will also be displayed. *)
let print_board (player:t) verbose = 
  let shape = player.shape in
  let max_size = (snd shape) in (* change if we extend to unique boards *)
  let () = print_top_labels max_size in
  let rec intermediate (row_acc:int) (cur_board:board) =
    match cur_board with
    | [] -> begin (fst shape + 1) |> print_dash; print_endline "" end
    | h::t -> begin
        (fst shape + 1) |> print_dash; 
        print_row row_acc h verbose; 
        intermediate (row_acc+1) t
      end
  in intermediate 1 player.board

(** [print_my_board player] prints the board of the current [player] showing 
    them their own ships and which spots on the players board has been attacked.
*)
let print_my_board (player:t) = print_board player true

(** [print_opp_board player] prints the opposing players view of the current 
    [player]'s board. This view will hide the players ships from being printed. 
*)
let print_opp_board (player:t) = print_board player false

(** [is_sunk s] returns whether ship [s] is sunk. A ship is considered sunk when
    all of its coordinates have been hit. *)
let is_sunk (s:ship) = 
  let f acc b = acc && (b = Hit) in
  List.fold_left f true s.coords

(** [all_sunk p] returns whether all ships for player [p] are sunk. A ship is 
    considered sunk when all of its coordinates have been hit. If every ship
    that the player has is sunk then the player has lost the game. *)
let all_sunk (p:t) = 
  let f acc b = acc && (is_sunk b) in
  List.fold_left f true p.ships

let update_cell c = 
  match c with
  | Empty -> Miss
  | Ship -> Hit
  | _ -> failwith "RI does not hold"

(** [check (c1, c2)] returns the new player with the coordinate (c1, c2) 
    updated to represent the player's guess. Returns the option of continue or loss
    TODO update this comment *)
let check (player:t) (c1, c2) = 
  let b = player.board in
  let rec inner_loop acc row_pos col_pos row =
    match row with
    | [] -> acc
    | h::t -> if col_pos = c2 
      then update_cell h
      else inner_loop (h::acc) row_pos (col_pos+1) t  in
  let rec outer_loop acc row_pos b =
    match b with
    | [] -> acc
    | h::t ->  
      if h = c1 
      then 
        outer_loop (List.rev (inner_loop [] row_pos 1 h)::acc) (row_pos+1) t
      else outer_loop (h::acc) (row_pos+1) t
  in 
  let new_board = List.rev (outer_loop [] 1 b) in
  let new_ships = 
    let new_player = player with {board=new_board} in 
if (all_sunk new_player)
then Loss
else Continue(new_player)