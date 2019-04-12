
type cell = Empty|Ship|Miss|Hit|Sunk
type board = cell list list
type coord = Hit | Coord of int * int
type ship = {sunk :bool ; size:int; inserted:bool; coords: coord list}

(** RI: Board shape can be no larger than 26 by 26 *)
type player = {
  board : board;
  ships : ship list;
  shape : int * int
}

type  t = player

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

let rec set_inner (inner_acc:cell list) (y:int) = 
  if y > 0 
  then set_inner (Empty::inner_acc) (y-1)
  else inner_acc

let init_empty_board x y =
  let rec set_outer acc x y = 
    if x > 0
    then let new_arr = set_inner [] y in 
      set_outer (new_arr::acc) (x-1) y
    else acc
  in set_outer [] x y

let init_shape x y = 
  (x, y)

let init_player x y ship_sizes = 
  {
    board = init_empty_board x y;
    ships = [];
    shape = init_shape x y
  }

let add_ship ships coords = 
  let new_ship = 
    {
      sunk = false;
      size = List.length coords;
      inserted = true;
      coords = coords
    } in
  new_ship::ships

let empty_cell c = c = Empty

let add_ship_to_board board coords = 
  let rec inner_loop acc row_pos col_pos row =
    match row with
    | [] -> acc
    | h::t -> 
      begin
        if List.mem (Coord (row_pos, col_pos)) coords
        then begin 
          if not (empty_cell h) 
          then failwith "BLAH" (* Add real exception *)
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

let get_all_cords start_cord end_cord = [Coord (1,1); Coord (1,2); Coord (1,3)]

let insert_ship player start_cord end_cord = 
  let cord_list = get_all_cords start_cord end_cord in
  let new_ships = add_ship player.ships cord_list in
  let new_board = add_ship_to_board player.board cord_list in
  {
    board=new_board;
    ships=new_ships;
    shape=player.shape
  }

(** [cell_spacing] is a constant representing the space between cells on a
    gameboard. Incorporated to display the game to the user. *)
let cell_spacing = " | "

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

let parse_cell c = 
  match c with
  | Empty -> " "
  | Ship -> " "
  | Miss -> "0"
  | Hit ->  "X"
  | Sunk -> "Z"

let get_row (row_acc:int) (row:cell list) = 
  let rec tr_helper acc r = 
    match r with 
    | [] -> acc
    | h::t -> tr_helper (acc ^ (parse_cell h ^ cell_spacing)) t
  in tr_helper ((row_acc |> inv_index |> Char.escaped) ^ cell_spacing) row

let print_row row_acc row = get_row row_acc row |> print_endline

let rec print_dash row_size = 
  if row_size > 0
  then begin print_string ("-" ^ dash_space); print_dash (row_size - 1) end
  else print_endline ""

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

let temp_player = init_player 5 5 [1;2;3]

let print_board player = 
  let shape = player.shape in
  let max_size = (snd shape) in (* change if we extend to unique boards *)
  let () = print_top_labels max_size in
  let rec intermediate (row_acc:int) (cur_board:board) =
    match cur_board with
    | [] -> begin (fst shape + 1) |> print_dash; print_endline "" end
    | h::t -> begin
        (fst shape + 1) |> print_dash; 
        print_row row_acc h; 
        intermediate (row_acc+1) t
      end
  in intermediate 1 player.board