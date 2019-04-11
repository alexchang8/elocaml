
type cell = Empty|Ship|Miss|Hit
type board = cell list list
type coord = Hit | Coord of int * int
type ship = {sunk :bool ; size:int; inserted:bool; coords: coord list}

type player = {
  board : board;
  ships : ship list;
  shape : int * int
}

type  t = player

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

let init_ship size =
  {
    sunk=false;
    size=size;
    inserted=false;
    coords=[]
  }

let init_ships ship_sizes = 
  let rec tr_helper acc sships = 
    match sships with
    | [] -> acc 
    | h::t -> tr_helper ((init_ship h)::acc) t
  in tr_helper [] ship_sizes

let init_shape x y = 
  (x, y)

let init_player x y ship_sizes = 
  {
    board = init_empty_board x y;
    ships = init_ships ship_sizes;
    shape = init_shape x y
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

let get_row (row:cell list) = 
  let rec tr_helper acc r = 
    match r with 
    | [] -> acc
    | h::t -> 
  in get_row "" row

let print_row row = get_row row |> print_endline

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

let temp_player = init_player 3 3 [1;2;3]

let print_board player = 
  let shape = player.shape in
  let max_size = (snd shape) in (* change if we extend to unique boards *)
  let () = print_top_labels max_size in
  let rec intermediate (cur_board:board) =
    match cur_board with
    | [] -> begin (snd shape + 1) |> print_dash; print_endline "" end
    | h::t -> begin
        (snd shape + 1) |> print_dash; print_row h; intermediate t
      end
  in intermediate player.board